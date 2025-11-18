# 30_model_participation.R
# Purpose: Bernoulli (participation) model using brms/cmdstanr
# NOTE: month/season は使いません。図も出力しません。

suppressPackageStartupMessages({
  library(tidyverse)
  library(checkmate)
  library(brms)
  library(broom.mixed)
  library(forcats)
})

#' Fit Bernoulli participation model (no month/season, no plotting)
#'
#' @param long  Long-format tibble from make_pca(); one row per id×dish_type
#' @param out_dir Output directory for model objects/CSV
#' @param quick   If TRUE, subsample up to 3,000 rows for a fast fit
#' @param dry_run If TRUE, only validate structure and return brief report
#' @param use_monotonic If TRUE and q022_ord exists, include mo(q022_ord)
#' @param use_q022_cat5 If TRUE and q022_cat5 exists, include q022_cat5
#' @param interact_q022_dish If TRUE, include interaction with dish_type
#' @param random_slope If TRUE, use (1 + dish_type | district); else (1 | district)
#' @return brmsfit object (or small list for dry_run)
fit_participation <- function(
    long,
    out_dir = ".",
    quick = FALSE,
    dry_run = FALSE,
    use_monotonic = TRUE,
    use_q022_cat5 = FALSE,
    interact_q022_dish = FALSE,
    random_slope = FALSE
) {
  # ---- validations (month/season 不要) ----
  assert_data_frame(long, min.rows = 1)
  must_have_min <- c(
    "id","district","gender","age",
    "novelty","experience","tradition",
    "dish_type","yes_no","consumed",
    "age_z","novelty_z","experience_z","tradition_z"
  )
  assert_subset(must_have_min, names(long))
  
  # optional q022
  has_mo  <- "q022_ord"  %in% names(long) && any(!is.na(long$q022_ord))
  has_cat <- "q022_cat5" %in% names(long) && any(!is.na(long$q022_cat5))
  
  if (dry_run) {
    return(list(
      ok = TRUE,
      n_rows = nrow(long),
      n_dishes = dplyr::n_distinct(long$dish_type),
      share_zero = mean(long$consumed == 0, na.rm = TRUE),
      has_q022_ord  = has_mo,
      has_q022_cat5 = has_cat,
      note = "Structure OK. Set dry_run=FALSE to fit model. (no month/season, no plots)"
    ))
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # ---- quick ----
  long_use <- if (isTRUE(quick)) {
    dplyr::slice_sample(long, n = min(nrow(long), 3000), seed = 2025)
  } else long
  
  # ---- factor hygiene & response casting ----
  long_use <- long_use %>%
    mutate(
      gender = if (is.numeric(gender) || is.character(gender)) {
        forcats::fct_recode(as.factor(gender),
                            "female"="1","male"="2","prefer_not"="3")
      } else as.factor(gender),
      dish_type = forcats::fct_relevel(as.factor(dish_type), "chopped_fish"),
      district  = as.factor(district),
      consumed  = as.integer(consumed)  # 0/1 厳密化（bayesplot不使用でも安全のため）
    )
  
  # ---- priors ----
  pri <- c(
    prior(normal(0, 1.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(student_t(3, 0, 2.5), class = "sd")
  )
  # mo() の simplex に事前を置くなら次を追記:
  # pri <- c(pri, prior(dirichlet(1), class = "simplex", coef = "q022_ord"))
  
  # ---- formula ----
  rhs_core <- "gender + age_z + novelty_z + experience_z + tradition_z"
  rhs_dish <- "dish_type"
  
  rhs_q22 <- NULL
  data_for_fit <- long_use
  
  if (use_monotonic && has_mo) {
    data_for_fit <- data_for_fit %>% filter(!is.na(q022_ord))
    rhs_q22 <- "mo(q022_ord)"
  } else if (use_q022_cat5 && has_cat) {
    rhs_q22 <- "q022_cat5"
  } else {
    if (use_monotonic && !has_mo)
      message("[fit_participation] NOTE: q022_ord not found or all NA; mo(q022_ord) skipped.")
    if (use_q022_cat5 && !has_cat)
      message("[fit_participation] NOTE: q022_cat5 not found or all NA; q022_cat5 skipped.")
  }
  
  rhs_int <- NULL
  if (!is.null(rhs_q22) && isTRUE(interact_q022_dish)) {
    rhs_int <- paste0(rhs_q22, " * ", rhs_dish)
  }
  
  re_part <- if (isTRUE(random_slope)) "(1 + dish_type | district)" else "(1 | district)"
  rhs_terms <- c(rhs_core, if (is.null(rhs_int)) c(rhs_q22, rhs_dish) else rhs_int, re_part)
  rhs_terms <- rhs_terms[nzchar(rhs_terms)]
  rhs <- paste(rhs_terms, collapse = " + ")
  
  form <- bf(as.formula(paste("consumed ~", rhs)))
  
  message("[fit_participation] q022 term: ",
          ifelse(is.null(rhs_q22), "none",
                 if (use_monotonic && has_mo) "mo(q022_ord)" else "q022_cat5"),
          " | interaction: ", isTRUE(interact_q022_dish),
          " | RE: ", ifelse(random_slope, "(1 + dish_type | district)", "(1 | district)"),
          " | n = ", nrow(data_for_fit))
  
  # ---- fit ----
  fit <- brm(
    formula = form,
    data    = data_for_fit,
    family  = bernoulli(link = "logit"),
    prior   = pri,
    chains  = 4, iter = 4000, warmup = 2000,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    seed    = 123,
    refresh = 0
  )
  
  # ---- save ----
  saveRDS(fit, file.path(out_dir, "fit_participation.rds"))
  
  # ---- exports: OR table only（図なし） ----
  est_or <- broom.mixed::tidy(
    fit, effects = "fixed",
    conf.int = TRUE, conf.level = 0.95, conf.method = "quantile"
  ) %>%
    mutate(
      OR   = exp(estimate),
      OR_l = exp(conf.low),
      OR_u = exp(conf.high),
      sig  = (OR_l > 1) | (OR_u < 1)
    )
  readr::write_excel_csv(est_or, file.path(out_dir, "estimates_participation_OR.csv"))
  
  message("[fit_participation] Saved brmsfit and OR table to: ", out_dir)
  return(fit)
}

# --- optional demo (not executed in .qmd) ----
if (exists("RUN_DEMO") && isTRUE(RUN_DEMO) && interactive() && identical(sys.nframe(), 0L)) {
  message("[30_model_participation.R] Demo start")
  OUT_DIR <- "analysis/outputs"
  long_path <- file.path(OUT_DIR, "analysis_long.rds")
  if (!file.exists(long_path)) stop("Missing analysis_long.rds. Run 20_pca_scores.R first.")
  long <- readr::read_rds(long_path)
  
  print(fit_participation(long, out_dir = OUT_DIR, quick = TRUE, dry_run = TRUE))
  
  fit_main <- fit_participation(
    long, out_dir = OUT_DIR, quick = TRUE,
    use_monotonic = TRUE, use_q022_cat5 = FALSE,
    interact_q022_dish = FALSE, random_slope = FALSE
  )
  print(summary(fit_main), digits = 2)
  message("[30_model_participation.R] Demo complete")
}
