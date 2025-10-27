# 41_model_frequency.R
# Purpose: Frequency model (NegBin, log link) with rich interactions (brms/cmdstanr)
# Usage:
#   source(here::here("analysis","R","32_model_frequency.R"))
#   fit <- fit_frequency_x(long_freq, out_dir = OUT_DIR)

suppressPackageStartupMessages({
  library(tidyverse); library(checkmate); library(brms)
  library(broom.mixed); library(forcats)
})

# --- helpers -----------------------------------------------------------------
.safe_interact <- function(lhs, rhs) as.vector(outer(lhs, rhs, paste, sep=":"))

# mo() 用：factor/ordered/numeric を 1..K の整数に正規化（交互作用は因子版で）
normalize_mo <- function(v) {
  if (is.factor(v) || is.ordered(v)) {
    idx <- as.integer(v)
  } else {
    idx <- suppressWarnings(as.numeric(v))
  }
  idx[!is.finite(idx)] <- NA_integer_
  K <- max(idx, na.rm = TRUE); K <- max(1L, K)
  idx <- pmin(pmax(as.integer(idx), 1L), K)
  list(x = idx, K = K)
}

#' Fit frequency model with interactions (NegBin, log link)
#'
#' Required columns in `dat`:
#'   district, gender, age_z, novelty_z, experience_z, tradition_z,
#'   dish_type, season, occasion, frequency
#' Optional:
#'   livelihood_ord (monotonic: agriculture→fishing intensity etc.)
#'
#' @param dat tibble
#' @param out_dir output directory
#' @param quick logical: subsample up to 3000 rows
#' @param dry_run logical: structure check only
#' @param use_livelihood_mo logical: use mo(livelihood_ord) as main effect if present
#' @param interact_liv_season logical: include (factorized livelihood) × season
#' @param random_slope_set character subset of c("season","dish_type")
#' @param adapt_delta,max_treedepth MCMC controls
#' @return brmsfit (invisible)

fit_frequency_x <- function(
    dat, out_dir = ".", quick = FALSE, dry_run = FALSE,
    use_livelihood_mo = TRUE,
    interact_liv_season = TRUE,
    random_slope_set = c("season"),
    adapt_delta = 0.97, max_treedepth = 12
){
  assert_data_frame(dat, min.rows = 1)
  must <- c("district","gender","age_z","novelty_z","experience_z","tradition_z",
            "dish_type","season","occasion","frequency")
  assert_subset(must, names(dat))
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  df <- if (quick) dplyr::slice_sample(dat, n = min(nrow(dat), 3000), seed = 2025) else dat
  
  df <- df %>%
    mutate(
      gender    = if (is.numeric(gender) || is.character(gender)) {
        forcats::fct_recode(as.factor(gender), "female"="1","male"="2","prefer_not"="3")
      } else factor(gender),
      season    = factor(season),
      occasion  = factor(occasion),
      dish_type = forcats::fct_relevel(as.factor(dish_type), "chopped_fish"),
      district  = factor(district)
    )
  
  # --- 出現水準チェック（1水準しか無い因子は落とす） -------------------------
  nlev <- function(x) length(levels(droplevels(x)))
  has2 <- list(
    gender   = nlev(df$gender)   >= 2,
    season   = nlev(df$season)   >= 2,
    occasion = nlev(df$occasion) >= 2,
    dish_type= nlev(df$dish_type)>= 2
  )
  
  # ---- optional monotonic: livelihood ---------------------------------------
  has_liv <- "livelihood_ord" %in% names(df) && any(!is.na(df$livelihood_ord))
  rhs_liv_main <- NULL
  liv_fac_for_interact <- NULL
  if (use_livelihood_mo && has_liv) {
    df <- df %>% filter(!is.na(livelihood_ord))
    nm <- normalize_mo(df$livelihood_ord)
    df$liv_mo <- nm$x
    rhs_liv_main <- "mo(liv_mo)"                 # 主効果のみ mo()
    df$liv_f <- factor(df$liv_mo, levels = sort(unique(df$liv_mo)), ordered = TRUE)
    liv_fac_for_interact <- "liv_f"
  }
  
  # ---- main effects (因子は2水準以上のときだけ入れる) ------------------------
  rhs_core <- c("age_z","novelty_z","experience_z","tradition_z")
  if (has2$gender)    rhs_core <- c("gender", rhs_core)
  rhs_dish <- if (has2$dish_type) "dish_type" else NULL
  rhs_seas <- if (has2$season)    "season"    else NULL
  rhs_occ  <- if (has2$occasion)  "occasion"  else NULL
  
  main_terms <- c(rhs_core, rhs_dish, rhs_seas, rhs_occ, rhs_liv_main)
  
  # ---- interactions（安全に pruning） ----------------------------------------
  int_terms <- character(0)
  # season × dish_type
  if (has2$season && has2$dish_type) int_terms <- c(int_terms, "season:dish_type")
  # occasion × gender
  if (has2$occasion && has2$gender)   int_terms <- c(int_terms, "occasion:gender")
  # dish_type × (novelty_z, tradition_z, experience_z)
  if (has2$dish_type) int_terms <- c(int_terms,
                                     .safe_interact("dish_type", c("novelty_z","tradition_z","experience_z")))
  # livelihood × season（因子版で安全に）
  if (isTRUE(interact_liv_season) && !is.null(liv_fac_for_interact) && has2$season) {
    int_terms <- c(int_terms, paste0(liv_fac_for_interact, ":season"))
    main_terms <- unique(c(main_terms, liv_fac_for_interact))  # 階層原理
  }
  int_terms <- unique(int_terms)
  
  # ---- random effects（1水準ならスロープを外す：安全版） ---------------------
  re_slopes <- intersect(random_slope_set, c("season","dish_type"))
  # 明示フィルタ（順序や長さに依存しない）
  if ("season"   %in% re_slopes && !has2$season)    re_slopes <- setdiff(re_slopes, "season")
  if ("dish_type"%in% re_slopes && !has2$dish_type) re_slopes <- setdiff(re_slopes, "dish_type")
  
  re_terms <- if (length(re_slopes) > 0) {
    paste0("(1 + ", paste(re_slopes, collapse = " + "), " | district)")
  } else {
    "(1 | district)"
  }
  
  # ---- assemble formula ------------------------------------------------------
  rhs <- paste(c(main_terms, int_terms, re_terms), collapse = " + ")
  form <- bf(as.formula(paste("frequency ~", rhs)))
  
  # ---- priors ---------------------------------------------------------------
  pri <- c(
    prior(normal(0, 0.5), class = "b"),
    prior(normal(0, 1.0), class = "Intercept"),
    prior(exponential(1), class = "sd"),
    prior(exponential(1), class = "shape")
  )
  
  # ---- dry run --------------------------------------------------------------
  if (isTRUE(dry_run)) {
    return(list(
      ok = TRUE,
      fixed_main = main_terms,
      interactions = int_terms,
      re = re_terms,
      levels = sapply(list(gender=df$gender, season=df$season, occasion=df$occasion, dish_type=df$dish_type), nlev)
    ))
  }
  
  message("[fit_frequency_x] main: ", paste(main_terms, collapse=", "),
          " | int: ", paste(int_terms, collapse=", "),
          " | RE: ", re_terms)
  
  fit <- brm(
    formula = form, data = df, family = negbinomial(link = "log"),
    prior = pri, chains = 4, iter = 4000, warmup = 2000, cores = 4,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    seed = 2025, refresh = 0
  )
  
  saveRDS(fit, file.path(out_dir, "fit_frequency_interactions.rds"))
  
  est_rr <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
    transmute(
      term, estimate, std.error, conf.low, conf.high,
      RR = exp(estimate), RR_l = exp(conf.low), RR_u = exp(conf.high),
      sig = (RR_l > 1) | (RR_u < 1)
    )
  readr::write_excel_csv(est_rr, file.path(out_dir, "estimates_frequency_RR_interactions.csv"))
  
  base_season <- if (has2$season) levels(df$season)[1] else levels(df$season)
  base_occ    <- if (has2$occasion) levels(df$occasion)[1] else levels(df$occasion)
  newdat <- expand.grid(
    gender      = if (has2$gender) levels(df$gender)[levels(df$gender) %in% c("female","male")] else levels(df$gender),
    dish_type   = if (has2$dish_type) levels(df$dish_type) else levels(df$dish_type),
    season      = if (has2$season) c(base_season, setdiff(levels(df$season), base_season)[1]) else levels(df$season),
    occasion    = if (has2$occasion) c(base_occ, setdiff(levels(df$occasion), base_occ)[1]) else levels(df$occasion),
    novelty_z   = c(-1, 1),
    tradition_z = 0, experience_z = 0, age_z = 0
  )
  
  if (!is.null(df$liv_mo)) {
    K <- max(df$liv_mo, na.rm = TRUE); newdat$liv_mo <- ceiling(K/2)
  }
  if (!is.null(df$liv_f)) {
    newdat$liv_f <- factor(levels(df$liv_f)[ceiling(nlevels(df$liv_f)/2)],
                           levels = levels(df$liv_f), ordered = TRUE)
  }
  
  pr_mu <- posterior_epred(fit, newdata = newdat, re_formula = NA)
  mu    <- apply(pr_mu, 2, mean)
  ci_l  <- apply(pr_mu, 2, quantile, 0.025)
  ci_u  <- apply(pr_mu, 2, quantile, 0.975)
  pred_tbl <- dplyr::bind_cols(newdat, mu = mu, lwr = ci_l, upr = ci_u)
  readr::write_excel_csv(pred_tbl, file.path(out_dir, "predictions_frequency_simple_effects.csv"))
  
  suppressWarnings({
    p1 <- pp_check(fit, type = "rootogram")
    ggplot2::ggsave(file.path(out_dir, "ppc_frequency_rootogram.png"),
                    plot = p1, width = 140, height = 90, units = "mm")
  })
  
  message("[fit_frequency_x] Saved outputs to: ", out_dir)
  invisible(fit)
}

