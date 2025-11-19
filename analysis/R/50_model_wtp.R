# 50_model_wtp.R
# Purpose: WTP/price models with robust Student-t (default) or Lognormal
# NOTE: month は明示的に無視し、season を使用します。
# Usage:
#   source(here::here("analysis","R","50_model_wtp.R"))
#   fit <- fit_wtp_model(wtp, out_dir = OUT_DIR, family = "lognormal")

suppressPackageStartupMessages({
  library(tidyverse)
  library(checkmate)
  library(brms)
  library(broom.mixed)
})

# ---------- helpers ----------
num_clean_999 <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  v[v == 999] <- NA_real_
  v
}

scale_guard <- function(x) {
  mu <- mean(x, na.rm = TRUE); sdv <- stats::sd(x, na.rm = TRUE)
  if (is.na(sdv) || sdv == 0) return(rep(0, length(x)))
  (x - mu) / sdv
}

# ---------- main ----------
#' Fit WTP/price model（month は無視、season を使用）
#' @param wtp_dat tibble with at least 'price'. Optional: grams, district, gender, dish_type or recipe, season, youandme
#' @param out_dir output directory
#' @param family  "student" (robust on log-price) or "lognormal"
#' @param random_intercept logical; add (1|district) if district exists with ≥2 levels (default TRUE)
#' @param quick   subsample up to 3000 rows
#' @param dry_run only validate structure
#' @param backend "cmdstanr" or "rstan"（推奨: "cmdstanr"）
#' @param cores,threads_per_chain 並列設定（cmdstanr使用時は threads も）
#' @return brmsfit (or list if dry_run)
fit_wtp_model <- function(
    wtp_dat, out_dir = ".",
    family = c("student","lognormal"),
    random_intercept = TRUE,
    quick = FALSE, dry_run = FALSE,
    backend = c("cmdstanr","rstan"),
    cores = 4, threads_per_chain = 4
) {
  family  <- match.arg(family)
  backend <- match.arg(backend)
  
  # ---- validations ----
  assert_data_frame(wtp_dat, min.rows = 1)
  assert_subset("price", names(wtp_dat))
  
  # ---- cleaning / derived ----
  dat <- wtp_dat %>%
    mutate(
      price = num_clean_999(price),
      grams = if ("grams" %in% names(.)) num_clean_999(grams) else NA_real_
    ) %>%
    filter(!is.na(price), price > 0) %>%
    mutate(
      # month はあっても使わないが型だけは合わせておく
      across(any_of(c("district","gender","recipe","dish_type","month","season","youandme")),
             ~ as.factor(.x))
    )
  
  # per-100g
  if ("grams" %in% names(dat)) {
    dat <- dat %>% mutate(price_per_100g = price / pmax(grams, 1e-9) * 100)
  } else {
    dat <- dat %>% mutate(price_per_100g = price)
  }
  
  # log-price for student family
  dat <- dat %>% mutate(log_p = log(price_per_100g))
  
  # optional quick
  dat_use <- if (isTRUE(quick)) dplyr::slice_sample(dat, n = min(nrow(dat), 3000), seed = 2025) else dat
  
  # ---- availability flags (month は強制的に使わない) ----
  avail <- names(dat_use)
  has <- function(v) v %in% avail && any(!is.na(dat_use[[v]]))
  has_district <- has("district")
  has_gender   <- has("gender")
  has_recipe   <- has("recipe")
  has_dish     <- has("dish_type")
  has_season   <- has("season")
  has_youme    <- has("youandme")
  has_month    <- FALSE  # ← 明示的に無視
  
  # drop all-NA columns (safety)
  dat_use <- dat_use %>% dplyr::select(where(~ !all(is.na(.x))))
  if (!"price_per_100g" %in% names(dat_use)) dat_use <- dat_use %>% mutate(price_per_100g = price)
  if (!"log_p" %in% names(dat_use))          dat_use <- dat_use %>% mutate(log_p = log(price_per_100g))
  
  if (dry_run) {
    return(list(
      ok = TRUE,
      n_rows = nrow(dat_use),
      price_nonmissing = sum(!is.na(dat_use$price)),
      usable_predictors = c(
        if (has_district) "district",
        if (has_gender)   "gender",
        if (has_dish)     "dish_type" else if (has_recipe) "recipe",
        if (has_season)   "season",
        if (has_youme)    "youandme"
      ),
      note = "month は仕様により未使用です。"
    ))
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # ---- predictors (only those available; month は入れない) ----
  dish_var <- if (has_dish) "dish_type" else if (has_recipe) "recipe" else NULL
  fixed_terms <- c(
    if (has_gender)   "gender" else NULL,
    dish_var,
    if (has_season)   "season" else NULL,
    if (has_youme)    "youandme" else NULL
  )
  
  # district RE only if ≥2 levels
  re_term <- NULL
  if (isTRUE(random_intercept) && has_district) {
    nlev <- dplyr::n_distinct(stats::na.omit(dat_use$district))
    if (is.finite(nlev) && nlev >= 2) re_term <- "(1 | district)"
  }
  
  rhs <- paste(c(fixed_terms, re_term), collapse = " + ")
  if (rhs == "" || all(is.na(rhs))) rhs <- "1"
  
  # ---- formula & priors ----
  if (family == "student") {
    form <- bf(as.formula(paste("log_p ~", rhs)))
    fam  <- student()
    pri  <- c(
      prior(normal(0, 1.5),  class = "b"),
      prior(normal(0, 2.5),class = "Intercept"),
      prior(student_t(3, 0, 2.5), class = "sigma"),
      prior(student_t(3, 0, 2.5), class = "nu")
    )
  } else {
    form <- bf(as.formula(paste("price_per_100g ~", rhs)))
    fam  <- lognormal()
    pri  <- c(
      prior(normal(0, 1.5),  class = "b"),
      prior(normal(0, 2.5),class = "Intercept"),
      prior(exponential(1), class = "sigma")  # >0 を素直に
    )
  }
  
  # ---- parallel backend ----
  threads_arg <- NULL
  if (backend == "cmdstanr") {
    threads_arg <- tryCatch(brms::threading(as.integer(threads_per_chain)), error = function(e) NULL)
  }
  
  message("[fit_wtp_model] family=", family,
          "  n=", nrow(dat_use),
          "  RE(district)=", !is.null(re_term),
          "  backend=", backend,
          if (!is.null(threads_arg)) paste0("  threads_per_chain=", threads_per_chain) else "")
  
  fit <- brm(
    formula = form,
    data    = dat_use,
    family  = fam,
    prior   = pri,
    chains  = 4, iter = 4000, warmup = 2000,
    cores   = as.integer(cores),
    backend = backend,
    threads = threads_arg,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    seed    = 123, refresh = 0
  )
  
  saveRDS(fit, file.path(out_dir, paste0("fit_wtp_", family, ".rds")))
  
  # ---- export fixed effects as multiplicative ratios (exp on log-scale params) ----
  est <- broom.mixed::tidy(
    fit, effects = "fixed",
    conf.int = TRUE, conf.level = 0.95, conf.method = "quantile"
  ) %>%
    dplyr::transmute(
      term,
      estimate = estimate,
      std.error = std.error,
      l95 = conf.low,
      u95 = conf.high,
      Ratio   = exp(estimate),
      Ratio_l = exp(l95),
      Ratio_u = exp(u95)
    )
  
  readr::write_excel_csv(est, file.path(out_dir, paste0("estimates_wtp_", family, ".csv")))
  
  # ---- compact predictions（dish × gender 優先、他は基準に固定。month は使わない） ----
  pred_groups_main <- c(dish_var, if (has_gender) "gender" else NULL)
  used_terms <- c(
    pred_groups_main,
    if (has_season) "season",
    if (has_youme)  "youandme" else NULL
  )
  used_terms <- used_terms[!is.null(used_terms)]
  
  if (length(used_terms) > 0) {
    newdat <- dat %>%
      group_by(across(any_of(pred_groups_main))) %>%
      summarise(.n = n(), .groups = "drop") %>%
      arrange(desc(.n)) %>%
      slice_head(n = 50)
    
    if (nrow(newdat) > 0) {
      # 基準水準で補う
      for (v in setdiff(used_terms, names(newdat))) {
        if (v %in% names(fit$data) && is.factor(fit$data[[v]])) {
          base_level <- levels(fit$data[[v]])[1]
          newdat[[v]] <- factor(base_level, levels = levels(fit$data[[v]]))
        } else {
          newdat[[v]] <- 0
        }
      }
      # 学習側 levels に揃える（未知レベルをNA→drop）
      for (v in used_terms) {
        if (v %in% names(fit$data) && is.factor(fit$data[[v]])) {
          allowed <- levels(fit$data[[v]])
          newdat[[v]] <- factor(as.character(newdat[[v]]), levels = allowed)
          newdat <- dplyr::filter(newdat, !is.na(.data[[v]]))
        }
      }
      newdat <- newdat %>% select(any_of(used_terms), .n)
      
      pr <- stats::fitted(fit, newdata = newdat, re_formula = NA, summary = TRUE) %>% as_tibble()
      pred <- bind_cols(newdat, pr)
      
      # 統一出力：予測の価格スケール（per-100g）に揃える
      if (family == "student") {
        # student: fitted は logスケール → expで戻す（列名差に耐性）
        est_col <- dplyr::coalesce(pred$Estimate, pred$estimate)
        lcl_col <- dplyr::coalesce(pred$`Q2.5`, pred$conf.low)
        ucl_col <- dplyr::coalesce(pred$`Q97.5`, pred$conf.high)
        pred <- pred %>% mutate(
          est_price_per_100g = exp(est_col),
          lwr = exp(lcl_col),
          upr = exp(ucl_col)
        )
      } else {
        # lognormal: fitted は応答スケール（=価格）なのでそのまま
        if (!"Estimate" %in% names(pred)) {
          pred <- pred %>% rename(Estimate = estimate, `Q2.5` = conf.low, `Q97.5` = conf.high)
        }
        pred <- pred %>% rename(est_price_per_100g = Estimate, lwr = `Q2.5`, upr = `Q97.5`)
      }
      
      readr::write_excel_csv(pred, file.path(out_dir, paste0("predictions_wtp_", family, ".csv")))
    }
  }
  
  message("[fit_wtp_model] Saved model and exports to: ", out_dir)
  fit
}

# --- optional demo when sourcing directly ---
if (interactive() && identical(sys.nframe(), 0L)) {
  message("[50_model_wtp.R] Demo start")
  OUT_DIR <- "analysis/outputs"
  wtp_syn <- tibble::tibble(
    price = c(3000, 5000, 4500, 7000, 3200, 5200),
    grams = c(150, 200, 180, 220, 160, 210),
    district = factor(c("A","A","B","B","B","A")),
    gender   = factor(c("female","male","male","female","male","female")),
    dish_type= factor(c("chopped_fish","sliced_fish","pufferfish","pufferfish","sliced_fish","chopped_fish")),
    season   = factor(c("dry","dry","rainy","rainy","dry","rainy"))
  )
  print(fit_wtp_model(wtp_syn, out_dir = OUT_DIR, family = "lognormal", quick = TRUE, dry_run = TRUE))
  fit_l <- fit_wtp_model(wtp_syn, out_dir = OUT_DIR, family = "lognormal", quick = TRUE, backend = "cmdstanr")
  print(summary(fit_l), digits = 2)
  message("[50_model_wtp.R] Demo complete")
}
