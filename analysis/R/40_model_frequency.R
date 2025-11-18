# 40_model_frequency.R
# Purpose: Frequency model (counts among consumers) with Season (dry/wet) and Occasion (daily/party)
# Design: No interaction between season and occasion (main effects only)
# Usage:
#   source(here::here("analysis","R","40_model_frequency.R"))
#   fit <- fit_frequency_season_occ(long, dat_raw = dat_scored, out_dir = OUT_DIR)

suppressPackageStartupMessages({
  library(tidyverse)
  library(checkmate)
  library(forcats)
  library(brms)
  library(broom.mixed)
})

# ---------- utility: numeric clean with 999="unknown" ----------
clean_numeric_999 <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  v[is.na(v)] <- NA_real_
  v[v == 999] <- NA_real_     # 999 => unknown
  v[v < 0]    <- 0            # safety
  v
}

sum_cols <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (length(cols) == 0L) return(rep(NA_real_, nrow(df)))
  X <- df |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::mutate(dplyr::across(everything(), clean_numeric_999))
  s <- rowSums(X, na.rm = TRUE)
  all_na_row <- apply(is.na(X), 1, all)
  s[all_na_row] <- NA_real_
  s
}

# ---------- build per-season × occasion frequency from raw questionnaire ----------
# dat_raw は q112, q113, q115, ... を含むワイド（通常は dat_scored）

build_frequency_season_occ_long <- function(dat_raw) {
  assert_data_frame(dat_raw, min.rows = 1)
  dat_raw <- dat_raw |> dplyr::mutate(id = as.character(id))   # ★追加
  
  # Q-code candidates (重複コードに備え複数候補を用意)
  # chopped (小魚・チョップ)
  chop_daily_dry  <- c("q112")
  chop_daily_wet  <- c("q113")
  chop_party_dry  <- c("q115","q116")
  chop_party_wet  <- c("q115","q117")
  
  # sliced (大魚・スライス)
  slice_daily_dry <- c("q212")
  slice_daily_wet <- c("q213")
  slice_party_dry <- c("q215","q216")
  slice_party_wet <- c("q215","q217")
  
  # puffer (フグ)
  puff_daily_dry  <- c("q312")
  puff_daily_wet  <- c("q313")
  puff_party_dry  <- c("q315","q316")
  puff_party_wet  <- c("q315","q317")
  
  # per dish × season × occasion
  chopped_dry_daily  <- sum_cols(dat_raw, chop_daily_dry)
  chopped_wet_daily  <- sum_cols(dat_raw, chop_daily_wet)
  chopped_dry_party  <- sum_cols(dat_raw, chop_party_dry)
  chopped_wet_party  <- sum_cols(dat_raw, chop_party_wet)
  
  sliced_dry_daily   <- sum_cols(dat_raw, slice_daily_dry)
  sliced_wet_daily   <- sum_cols(dat_raw, slice_daily_wet)
  sliced_dry_party   <- sum_cols(dat_raw, slice_party_dry)
  sliced_wet_party   <- sum_cols(dat_raw, slice_party_wet)
  
  puffer_dry_daily   <- sum_cols(dat_raw, puff_daily_dry)
  puffer_wet_daily   <- sum_cols(dat_raw, puff_daily_wet)
  puffer_dry_party   <- sum_cols(dat_raw, puff_party_dry)
  puffer_wet_party   <- sum_cols(dat_raw, puff_party_wet)
  
  # wide -> long (id × dish_type × season × occasion)
  freq_wide <-
    dat_raw |>
    dplyr::mutate(
      chopped_dry_daily = chopped_dry_daily,
      chopped_wet_daily = chopped_wet_daily,
      chopped_dry_party = chopped_dry_party,
      chopped_wet_party = chopped_wet_party,
      
      sliced_dry_daily  = sliced_dry_daily,
      sliced_wet_daily  = sliced_wet_daily,
      sliced_dry_party  = sliced_dry_party,
      sliced_wet_party  = sliced_wet_party,
      
      puffer_dry_daily  = puffer_dry_daily,
      puffer_wet_daily  = puffer_wet_daily,
      puffer_dry_party  = puffer_dry_party,
      puffer_wet_party  = puffer_wet_party
    ) |>
    dplyr::select(dplyr::any_of(c(
      "id","year","month","district","commune","village",
      "chopped_dry_daily","chopped_wet_daily","chopped_dry_party","chopped_wet_party",
      "sliced_dry_daily","sliced_wet_daily","sliced_dry_party","sliced_wet_party",
      "puffer_dry_daily","puffer_wet_daily","puffer_dry_party","puffer_wet_party"
    )))
  
  freq_long <-
    freq_wide |>
    tidyr::pivot_longer(
      cols = tidyselect::everything() & !any_of(c("id","year","month","district","commune","village")),
      names_to = "key",
      values_to = "frequency"
    ) |>
    dplyr::mutate(
      # key example: "chopped_dry_daily"
      dish_type = dplyr::case_when(
        grepl("^chopped_", key) ~ "chopped_fish",
        grepl("^sliced_",  key) ~ "sliced_fish",
        grepl("^puffer_",  key) ~ "pufferfish",
        TRUE ~ NA_character_
      ),
      season = dplyr::case_when(
        grepl("_dry_", key) ~ "dry",
        grepl("_wet_", key) ~ "wet",
        TRUE ~ NA_character_
      ),
      occasion = dplyr::case_when(
        grepl("_daily$", key) ~ "daily",
        grepl("_party$", key) ~ "party",
        TRUE ~ NA_character_
      ),
      dish_type = factor(dish_type, levels = c("chopped_fish","sliced_fish","pufferfish")),
      season    = forcats::fct_relevel(factor(season), "wet"),     # 基準: wet
      occasion  = forcats::fct_relevel(factor(occasion), "daily"), # 基準: daily
      frequency = clean_numeric_999(frequency)
    ) |>
    dplyr::select(id, dish_type, season, occasion, frequency)
  
  return(freq_long)
}

# ---------- main: Fit NB frequency model with Season + Occasion (no interaction) ----------
# long   : 共変量入りロング（id, dish_type, consumed, gender, age_z, novelty_z, ... district, month）
# dat_raw: q112 等を含むワイド（= dat_scored）
fit_frequency_season_occ <- function(
    long,
    dat_raw,
    out_dir = ".",
    quick = FALSE,
    dry_run = FALSE,
    random_slope = FALSE,
    trunc_zero = FALSE,        # TRUE: 0切断NB（頻度>=1のみ） / FALSE: 0許容NB
    include_month = FALSE      # 季節と場面が主目的のため既定は FALSE
) {
  
  # ---- checks ----
  assert_data_frame(long, min.rows = 1)
  assert_data_frame(dat_raw, min.rows = 1)
  long    <- long    |> dplyr::mutate(id = as.character(id))   # ★追加
  dat_raw <- dat_raw |> dplyr::mutate(id = as.character(id))   # ★追加
  
  need <- c("id","dish_type","consumed","gender","age_z",
            "novelty_z","experience_z","tradition_z","district","month")
  assert_subset(need, names(long))
  
  # ---- build frequencies (season × occasion) ----
  freq_so_long <- build_frequency_season_occ_long(dat_raw)
  
  # 念のため dish_type を共通のレベル順に
  common_levels <- c("chopped_fish","sliced_fish","pufferfish")
  long <- long |> dplyr::mutate(dish_type = forcats::fct_drop(factor(dish_type, levels = common_levels)))
  freq_so_long <- freq_so_long |> dplyr::mutate(dish_type = forcats::fct_drop(factor(dish_type, levels = common_levels)))
  
  long_freq <-
    long |>
    dplyr::select(id, dish_type, gender, age_z, novelty_z, experience_z, tradition_z,
                  district, month, consumed) |>
    dplyr::left_join(freq_so_long, by = c("id","dish_type")) |>
    dplyr::mutate(
      season    = forcats::fct_relevel(as.factor(season), "wet"),
      occasion  = forcats::fct_relevel(as.factor(occasion), "daily"),
      frequency = suppressWarnings(as.numeric(frequency))
    ) |>
    dplyr::filter(consumed == 1L, !is.na(frequency), !is.na(season), !is.na(occasion))
  
  # --- ensure integer counts for NB family ---
  # 1) 余分な小数が混じっていないかチェック（0.5などがあれば丸める）
  non_int <- which(is.finite(long_freq$frequency) & abs(long_freq$frequency - round(long_freq$frequency)) > 1e-8)
  if (length(non_int) > 0) {
    warning("[fit_frequency_season_occ] Non-integer counts found in `frequency` at ",
            length(non_int), " rows; rounding to nearest integer.")
  }
  
  # 2) 丸めて整数化（負値は0に）
  long_freq <- long_freq |>
    dplyr::mutate(
      frequency = as.integer(pmax(0, round(frequency)))
    )
  
  # 3) 0切断かどうかに応じて再フィルタ
  if (isTRUE(trunc_zero)) {
    long_freq <- long_freq %>% dplyr::filter(frequency >= 1)
  } else {
    long_freq <- long_freq %>% dplyr::filter(frequency >= 0)
  }
  
  # 任意：念のための整合性チェック（整数性）
  checkmate::assert_integerish(long_freq$frequency, lower = if (trunc_zero) 1 else 0, any.missing = FALSE)
  
  if (isTRUE(trunc_zero)) {
    long_freq <- long_freq %>% dplyr::filter(frequency >= 1)
  } else {
    long_freq <- long_freq %>% dplyr::filter(frequency >= 0)
  }
  
  if (nrow(long_freq) == 0L) stop("No rows available for frequency model after filtering.")
  
  if (isTRUE(quick)) {
    set.seed(2025)
    long_freq <- dplyr::slice_sample(long_freq, n = min(nrow(long_freq), 3000))
  }
  
  if (dry_run) {
    return(list(
      ok = TRUE,
      n_rows = nrow(long_freq),
      by_dish_season_occ = long_freq |> count(dish_type, season, occasion, name = "n"),
      min_max_freq = range(long_freq$frequency, na.rm = TRUE),
      trunc_zero = trunc_zero,
      note = "Structure OK. Set dry_run=FALSE to fit model."
    ))
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # ---- priors & formula (NO interaction) ----
  pri <- c(
    prior(normal(0, 1.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(gamma(0.01, 0.01), class = "shape")   # 過分散
  )
  
  rhs_core    <- "gender + age_z + novelty_z + experience_z + tradition_z"
  rhs_dish    <- "dish_type"
  rhs_season  <- "season"
  rhs_occasion<- "occasion"
  rhs_month   <- if (isTRUE(include_month)) "month" else NULL
  re_part     <- if (isTRUE(random_slope)) "(1 + dish_type | district)" else "(1 | district)"
  
  rhs_terms <- c(rhs_core, rhs_dish, rhs_season, rhs_occasion, rhs_month, re_part)
  rhs_terms <- rhs_terms[!is.na(rhs_terms) & nzchar(rhs_terms)]
  rhs <- paste(rhs_terms, collapse = " + ")
  
  resp <- if (isTRUE(trunc_zero)) "frequency | trunc(lb = 1)" else "frequency"
  form <- brms::bf(as.formula(paste(resp, "~", rhs)))
  
  message("[fit_frequency_season_occ] RE: ",
          ifelse(random_slope, "(1 + dish_type | district)", "(1 | district)"),
          " | trunc_zero: ", trunc_zero,
          " | month in model: ", isTRUE(include_month),
          " | n = ", nrow(long_freq))
  
  fit <- brms::brm(
    form, data = long_freq,
    family = negbinomial(),
    prior  = pri,
    chains = 4, iter = 4000, warmup = 2000,
    control = list(adapt_delta = 0.97, max_treedepth = 12),
    seed = 123, refresh = 0
  )
  
  saveRDS(fit, file.path(out_dir, "fit_frequency_season_occ.rds"))
  
  # Export RR table
  est_rr <- broom.mixed::tidy(fit, effects = "fixed") |>
    dplyr::mutate(
      RR   = exp(estimate),
      RR_l = exp(estimate - 1.96 * std.error),
      RR_u = exp(estimate + 1.96 * std.error),
      sig  = (RR_l > 1) | (RR_u < 1)
    )
  readr::write_excel_csv(est_rr, file.path(out_dir, "estimates_frequency_season_occ_RR.csv"))
  
  # PPC (hist)
  ppc_path <- file.path(out_dir, "ppc_frequency_season_occ_hist.png")
  suppressWarnings({
    p <- pp_check(fit, type = "hist", ndraws = 50)
    ggplot2::ggsave(ppc_path, plot = p, width = 140, height = 90, units = "mm")
  })
  
  message("[fit_frequency_season_occ] Saved brmsfit, RR table, and PPC to: ", out_dir)
  return(fit)
}

# ---- optional demo runner ----
if (exists("RUN_DEMO") && isTRUE(RUN_DEMO) && interactive() && identical(sys.nframe(), 0L)) {
  message("[40_model_frequency.R] Demo start")
  OUT_DIR <- "analysis/outputs"
  long_path <- file.path(OUT_DIR, "analysis_long.rds")
  if (!file.exists(long_path)) stop("Missing analysis_long.rds. Run 20_pca_scores.R first.")
  long <- readr::read_rds(long_path)
  
  # dat_scored を別途読み込んで dat_raw に渡す想定
  # dat_raw <- readr::read_rds(file.path(OUT_DIR, "dat_scored.rds"))
  
  # dry-run
  # print(fit_frequency_season_occ(long, dat_raw = dat_raw, out_dir = OUT_DIR, quick = TRUE, dry_run = TRUE))
  
  # fit (0許容 NB, 月は入れない)
  # fit <- fit_frequency_season_occ(long, dat_raw = dat_raw, out_dir = OUT_DIR,
  #                                 quick = TRUE, trunc_zero = FALSE, include_month = FALSE)
  # print(summary(fit), digits = 2)
  
  message("[40_model_frequency.R] Demo complete")
}
