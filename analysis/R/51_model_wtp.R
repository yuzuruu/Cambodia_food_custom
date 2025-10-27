# 51_model_wtp.R — WTP full model with safe interactions (fixed)

suppressPackageStartupMessages({
  library(tidyverse); library(checkmate); library(brms); library(broom.mixed)
})

# helpers ---------------------------------------------------------
if (!exists("num_clean_999")) {
  num_clean_999 <- function(x) { v <- suppressWarnings(as.numeric(x)); v[v==999] <- NA_real_; v }
}
nlev_safe <- function(x) if (is.factor(x)) length(levels(droplevels(x))) else dplyr::n_distinct(x, na.rm=TRUE)

.safe_saveRDS <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(obj, path)
  message("[saveRDS] -> ", path)
}
.safe_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_excel_csv(df, path)
  message("[write_csv] -> ", path)
}

# main ------------------------------------------------------------
#' Fit WTP model with interactions (Student-t on log-price OR Lognormal)
fit_wtp_model_x <- function(
    wtp_dat, out_dir = ".",
    fam_name = c("student","lognormal"),   # ← ここを family ではなく fam_name に
    random_slopes = c("month"),
    add_youme_month_interact = FALSE,
    quick = FALSE,
    backend = c("cmdstanr","rstan"),
    cores = 4, threads_per_chain = 4
){
  fam_name <- match.arg(fam_name)         # 文字列（"student" or "lognormal"）
  backend  <- match.arg(backend)
  
  assert_data_frame(wtp_dat, min.rows = 1)
  assert_subset("price", names(wtp_dat))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # cleaning
  dat <- wtp_dat %>%
    mutate(
      price = num_clean_999(price),
      grams = if ("grams" %in% names(.)) num_clean_999(grams) else NA_real_
    ) %>%
    filter(!is.na(price), price > 0)
  
  fac_cols <- intersect(c("district","gender","recipe","dish_type","month","season","youandme"), names(dat))
  if (length(fac_cols)) dat[fac_cols] <- lapply(dat[fac_cols], factor)
  
  # per-100g（grams 全NAならそのまま price）
  dat <- dat %>%
    mutate(price_per_100g = dplyr::if_else(!is.na(grams), price / pmax(grams, 1e-9) * 100, price),
           log_p = log(price_per_100g))
  
  dat_use <- if (isTRUE(quick)) dplyr::slice_sample(dat, n = min(nrow(dat), 3000), seed = 2025) else dat
  
  dish_var <- if ("dish_type" %in% names(dat_use)) "dish_type" else if ("recipe" %in% names(dat_use)) "recipe" else NULL
  time_var <- if ("month" %in% names(dat_use)) "month" else if ("season" %in% names(dat_use)) "season" else NULL
  
  has2 <- list(
    district = ("district" %in% names(dat_use)) && nlev_safe(dat_use$district) >= 2,
    gender   = ("gender"   %in% names(dat_use)) && nlev_safe(dat_use$gender)   >= 2,
    dish     = (!is.null(dish_var))              && nlev_safe(dat_use[[dish_var]]) >= 2,
    time     = (!is.null(time_var))              && nlev_safe(dat_use[[time_var]]) >= 2,
    youme    = ("youandme" %in% names(dat_use)) && nlev_safe(dat_use$youandme) >= 2
  )
  has2 <- lapply(has2, isTRUE)
  
  main_terms <- character(0)
  if (isTRUE(has2$gender)) main_terms <- c(main_terms, "gender")
  if (isTRUE(has2$dish))   main_terms <- c(main_terms, dish_var)
  if (isTRUE(has2$time))   main_terms <- c(main_terms, time_var)
  if (isTRUE(has2$youme))  main_terms <- c(main_terms, "youandme")
  if (!length(main_terms)) main_terms <- "1"
  
  int_terms <- character(0)
  if (isTRUE(has2$dish) && isTRUE(has2$gender)) int_terms <- c(int_terms, paste0(dish_var, ":gender"))
  if (isTRUE(has2$dish) && isTRUE(has2$time))   int_terms <- c(int_terms, paste0(dish_var, ":", time_var))
  if (isTRUE(has2$dish) && isTRUE(has2$youme))  int_terms <- c(int_terms, paste0(dish_var, ":youandme"))
  if (isTRUE(has2$gender) && isTRUE(has2$youme))int_terms <- c(int_terms, "gender:youandme")
  if (isTRUE(add_youme_month_interact) && isTRUE(has2$time) && isTRUE(has2$youme)) {
    int_terms <- c(int_terms, paste0(time_var, ":youandme"))
  }
  int_terms <- unique(int_terms)
  
  re_slopes   <- intersect(random_slopes, c("dish","month"))
  slope_names <- character(0)
  if ("dish"  %in% re_slopes && isTRUE(has2$dish))                  slope_names <- c(slope_names, dish_var)
  if ("month" %in% re_slopes && identical(time_var,"month") && isTRUE(has2$time)) slope_names <- c(slope_names, "month")
  re_terms <- if (isTRUE(has2$district)) {
    if (length(slope_names)) paste0("(1 + ", paste(slope_names, collapse=" + "), " | district)") else "(1 | district)"
  } else NULL
  
  rhs <- paste(c(main_terms, int_terms, re_terms), collapse = " + ")
  rhs <- gsub("\\+\\s*\\+","+", rhs); rhs <- trimws(rhs); if (!nzchar(rhs)) rhs <- "1"
  
  # 使用列で完全ケース化
  response <- if (fam_name == "student") "log_p" else "price_per_100g"
  used_vars <- unique(unlist(strsplit(gsub("[~+*():|]"," ", rhs), "\\s+")))
  used_vars <- used_vars[nzchar(used_vars)]
  used_vars <- intersect(used_vars, names(dat_use))
  keep_cols <- unique(c(response, used_vars))
  df_fit <- dat_use[, keep_cols, drop = FALSE]
  ok <- stats::complete.cases(df_fit)
  if (!any(ok)) stop("[fit_wtp_model_x] 完全ケースが 0 行です。式や欠測を確認してください。")
  dat_use <- dat_use[ok, , drop = FALSE]
  
  # family objects & priors
  if (fam_name == "student") {
    form   <- bf(as.formula(paste("log_p ~", rhs)))
    fam_obj<- student()
    pri    <- c(
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 1.5), class = "Intercept"),
      prior(student_t(3, 0, 2.0), class = "sigma"),
      prior(student_t(3, 0, 5.0), class = "nu")
    )
  } else {
    form   <- bf(as.formula(paste("price_per_100g ~", rhs)))
    fam_obj<- lognormal()
    pri    <- c(
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 1.5), class = "Intercept"),
      prior(exponential(1), class = "sigma")
    )
  }
  
  threads_arg <- NULL
  if (backend == "cmdstanr") threads_arg <- tryCatch(brms::threading(as.integer(threads_per_chain)), error = function(e) NULL)
  
  message("[fit_wtp_model_x] fam=", fam_name,
          " | n=", nrow(dat_use),
          " | main={", paste(main_terms, collapse=", "), "}",
          if (length(int_terms)) paste0(" | int={", paste(int_terms, collapse=", "), "}") else "",
          if (!is.null(re_terms)) paste0(" | RE=", re_terms) else "")
  
  fit <- brm(
    formula = form,
    data    = dat_use,
    family  = fam_obj,
    prior   = pri,
    chains  = 4, iter = 4000, warmup = 2000,
    cores   = as.integer(cores),
    backend = backend,
    threads = threads_arg,
    control = list(adapt_delta = 0.97, max_treedepth = 12),
    seed    = 2025, refresh = 0
  )
  
  # --- outputs
  .safe_saveRDS(fit, file.path(out_dir, sprintf("fit_wtp_interactions_%s.rds", fam_name)))
  
  est <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE, conf.level = 0.95, conf.method = "quantile") %>%
    transmute(term, estimate, std.error, l95 = conf.low, u95 = conf.high,
              Ratio = exp(estimate), Ratio_l = exp(l95), Ratio_u = exp(u95))
  .safe_write_csv(est, file.path(out_dir, sprintf("estimates_wtp_interactions_%s.csv", fam_name)))
  
  # 代表予測
  model_vars <- names(fit$data)
  dish_var   <- if ("dish_type" %in% model_vars) "dish_type" else if ("recipe" %in% model_vars) "recipe" else NULL
  time_var   <- if ("month"     %in% model_vars) "month"     else if ("season" %in% model_vars) "season" else NULL
  cats <- intersect(c(dish_var, "gender", time_var, "youandme"), model_vars)
  cats <- cats[!is.na(cats) & nzchar(cats)]
  get_levels <- function(v) { x <- fit$data[[v]]; if (is.factor(x)) levels(droplevels(x)) else sort(unique(na.omit(x))) }
  vals <- setNames(vector("list", length(cats)), cats)
  for (v in cats) vals[[v]] <- get_levels(v)
  vals <- vals[!vapply(vals, function(z) length(z)==0, logical(1))]
  cap_levels <- function(x, n=3) if (length(x) > n) x[1:n] else x
  if (length(vals) > 0) {
    if ("youandme" %in% names(vals)) vals[["youandme"]] <- vals[["youandme"]][1]
    if (!is.null(time_var) && time_var %in% names(vals)) vals[[time_var]] <- cap_levels(vals[[time_var]], 2)
  }
  newdat <- if (length(vals)==0) tibble(.dummy=1) else tidyr::expand_grid(!!!vals)
  
  pr <- stats::fitted(fit, newdata = newdat, re_formula = NA, summary = TRUE) %>% as_tibble()
  if (fam_name == "student") {
    est_col <- dplyr::coalesce(pr$Estimate, pr$estimate)
    lcl_col <- dplyr::coalesce(pr$`Q2.5`,    pr$conf.low)
    ucl_col <- dplyr::coalesce(pr$`Q97.5`,   pr$conf.high)
    pred <- bind_cols(newdat, tibble(est_price_per_100g = exp(est_col), lwr = exp(lcl_col), upr = exp(ucl_col)))
  } else {
    if (!"Estimate" %in% names(pr)) pr <- dplyr::rename(pr, Estimate = estimate, `Q2.5` = conf.low, `Q97.5` = conf.high)
    pred <- bind_cols(newdat, dplyr::transmute(pr, est_price_per_100g = Estimate, lwr = `Q2.5`, upr = `Q97.5`))
  }
  .safe_write_csv(pred, file.path(out_dir, sprintf("predictions_wtp_interactions_%s.csv", fam_name)))
  
  invisible(fit)
}
