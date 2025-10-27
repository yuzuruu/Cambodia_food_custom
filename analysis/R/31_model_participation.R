# 30_model_participation.R
# Purpose: Bernoulli (participation) models (main-effects & full-interactions)
# month/season を使わない版。pp_check 等の作図は一切しません。

suppressPackageStartupMessages({
  library(tidyverse)
  library(checkmate)
  library(brms)
  library(broom.mixed)
  library(forcats)
})

# 共通：因子・前処理
.participation_prepare <- function(long) {
  assert_data_frame(long, min.rows = 1)
  must_have <- c(
    "id","district","gender","age","novelty","experience","tradition",
    "dish_type","yes_no","consumed",
    "age_z","novelty_z","experience_z","tradition_z"
  )
  assert_subset(must_have, names(long))
  
  long %>%
    mutate(
      gender = if (is.numeric(gender) || is.character(gender)) {
        forcats::fct_recode(as.factor(gender), "female"="1","male"="2","prefer_not"="3")
      } else gender,
      dish_type = forcats::fct_relevel(as.factor(dish_type), "chopped_fish"),
      district  = as.factor(district)
    )
}

# ---------------------------
# 主効果モデル（mo はOK）
# ---------------------------
fit_participation <- function(
    long,
    out_dir = ".",
    quick = FALSE,
    dry_run = FALSE,
    use_monotonic = TRUE,
    use_q022_cat5 = FALSE,
    interact_q022_dish = FALSE,  # 主効果モデルでは通常 FALSE を推奨
    random_slope = FALSE
) {
  dat <- .participation_prepare(long)
  
  has_mo  <- "q022_ord"  %in% names(dat) && any(!is.na(dat$q022_ord))
  has_cat <- "q022_cat5" %in% names(dat) && any(!is.na(dat$q022_cat5))
  
  if (dry_run) {
    return(list(
      ok = TRUE,
      n_rows = nrow(dat),
      n_dishes = dplyr::n_distinct(dat$dish_type),
      share_zero = mean(dat$consumed == 0, na.rm = TRUE),
      has_q022_ord  = has_mo,
      has_q022_cat5 = has_cat
    ))
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  dat_use <- if (isTRUE(quick)) dplyr::slice_sample(dat, n = min(nrow(dat), 3000), seed = 2025) else dat
  
  rhs_core <- "gender + age_z + novelty_z + experience_z + tradition_z"
  rhs_dish <- "dish_type"
  
  # q022 は mo を優先（交互作用なし）
  rhs_q22 <- NULL
  if (use_monotonic && has_mo) {
    dat_use <- dat_use %>% filter(!is.na(q022_ord))
    rhs_q22 <- "mo(q022_ord)"
  } else if (use_q022_cat5 && has_cat) {
    rhs_q22 <- "q022_cat5"
  } else {
    rhs_q22 <- NULL
  }
  
  re_part <- if (isTRUE(random_slope)) "(1 + dish_type | district)" else "(1 | district)"
  rhs_terms <- c(rhs_core, rhs_q22, rhs_dish, re_part)
  rhs_terms <- rhs_terms[nzchar(rhs_terms)]
  rhs <- paste(rhs_terms, collapse = " + ")
  
  form <- bf(as.formula(paste("consumed ~", rhs)))
  
  pri <- c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(student_t(3, 0, 2.5), class = "sd")
  )
  
  # 完全ケース（式に出る列のみに限定）
  used_vars <- unique(unlist(strsplit(gsub("[~+*():|]"," ", rhs), "\\s+")))
  used_vars <- used_vars[nzchar(used_vars)]
  keep_cols <- unique(c("consumed", used_vars))
  dat_fit <- dat_use[, intersect(keep_cols, names(dat_use)), drop = FALSE] %>% drop_na()
  
  message("[fit_participation] main-effects | n=", nrow(dat_fit),
          " | q022=", ifelse(is.null(rhs_q22), "none", rhs_q22),
          " | RE=", re_part)
  
  fit <- brm(
    formula = form,
    data    = dat_fit,
    family  = bernoulli(link = "logit"),
    prior   = pri,
    chains  = 4, iter = 4000, warmup = 2000,
    control = list(adapt_delta = 0.95, max_treedepth = 12),
    seed    = 2025, refresh = 0
  )
  
  saveRDS(fit, file.path(out_dir, "fit_participation.rds"))
  
  est_or <- broom.mixed::tidy(
    fit, effects = "fixed",
    conf.int = TRUE, conf.method = "HPDinterval"
  ) %>%
    transmute(
      term,
      estimate, std.error,
      l95 = conf.low, u95 = conf.high,
      OR   = exp(estimate),
      OR_l = exp(l95),
      OR_u = exp(u95),
      sig  = (OR_l > 1) | (OR_u < 1)
    )
  readr::write_csv(est_or, file.path(out_dir, "estimates_participation_OR.csv"))
  
  message("[fit_participation] Saved to: ", out_dir)
  return(fit)
}

# --------------------------------
# 交互作用“全部入り”モデル（月/季節なし)
# mo(q022_ord)は「主効果のみ」; 交互作用は q022_cat5 を使う（あれば）
# --------------------------------
fit_participation_interactions <- function(
    long,
    out_dir = ".",
    quick = FALSE,
    use_monotonic = TRUE,   # mo は主効果としてのみ使用（安全）
    use_q022_cat5 = TRUE,   # 交互作用は cat5 を優先
    interact_q022_dish = TRUE,
    random_slope = TRUE     # (1 + dish_type | district)
) {
  dat <- .participation_prepare(long)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  dat_use <- if (isTRUE(quick)) dplyr::slice_sample(dat, n = min(nrow(dat), 3000), seed = 2025) else dat
  
  has_mo  <- "q022_ord"  %in% names(dat_use) && any(!is.na(dat_use$q022_ord))
  has_cat <- "q022_cat5" %in% names(dat_use) && any(!is.na(dat_use$q022_cat5))
  
  # ---- RHS 構築 ----
  core <- "gender + age_z + novelty_z + experience_z + tradition_z"
  dish <- "dish_type"
  
  # mo は主効果としてのみ（交互作用に入れない）
  q22_main <- NULL
  if (use_monotonic && has_mo) {
    dat_use <- dat_use %>% filter(!is.na(q022_ord))
    q22_main <- "mo(q022_ord)"
  }
  
  # 交互作用は q022_cat5 を使う（あれば）
  q22_int <- NULL
  if (use_q022_cat5 && has_cat && isTRUE(interact_q022_dish)) {
    q22_int <- "q022_cat5:dish_type"
  }
  
  # “全部入り”交互作用（pairwise）
  ints <- c(
    "gender:novelty_z",
    "gender:tradition_z",
    "dish_type:novelty_z",
    "dish_type:experience_z",
    "dish_type:tradition_z",
    "dish_type:gender"
  )
  if (!is.null(q22_int)) ints <- c(ints, q22_int)
  
  re_part <- if (isTRUE(random_slope)) "(1 + dish_type | district)" else "(1 | district)"
  
  rhs_terms <- c(core, dish, q22_main, ints, re_part)
  rhs_terms <- rhs_terms[nzchar(rhs_terms)]
  rhs <- paste(rhs_terms, collapse = " + ")
  form <- bf(as.formula(paste("consumed ~", rhs)))
  
  pri <- c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(student_t(3, 0, 2.5), class = "sd")
  )
  
  # 完全ケース（式に使う列のみ）
  used_vars <- unique(unlist(strsplit(gsub("[~+*():|]"," ", rhs), "\\s+")))
  used_vars <- used_vars[nzchar(used_vars)]
  keep_cols <- unique(c("consumed", used_vars))
  dat_fit <- dat_use[, intersect(keep_cols, names(dat_use)), drop = FALSE] %>% drop_na()
  
  message("[fit_participation_interactions] n=", nrow(dat_fit),
          " | q22(main)=", ifelse(is.null(q22_main), "none", q22_main),
          " | q22(int)=", ifelse(is.null(q22_int), "none", q22_int),
          " | RE=", re_part)
  
  fit <- brm(
    formula = form,
    data    = dat_fit,
    family  = bernoulli(link = "logit"),
    prior   = pri,
    chains  = 4, iter = 4000, warmup = 2000,
    control = list(adapt_delta = 0.97, max_treedepth = 12),
    seed    = 2025, refresh = 0
  )
  
  saveRDS(fit, file.path(out_dir, "fit_participation_interactions.rds"))
  
  est_or <- broom.mixed::tidy(
    fit, effects = "fixed",
    conf.int = TRUE, conf.method = "HPDinterval"
  ) %>%
    transmute(
      term,
      estimate, std.error,
      l95 = conf.low, u95 = conf.high,
      OR   = exp(estimate),
      OR_l = exp(l95),
      OR_u = exp(u95),
      sig  = (OR_l > 1) | (OR_u < 1)
    )
  readr::write_excel_csv(est_or, file.path(out_dir, "estimates_participation_interactions_OR.csv"))
  
  message("[fit_participation_interactions] Saved to: ", out_dir)
  return(fit)
}
