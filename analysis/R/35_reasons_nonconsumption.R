# 35_reasons_nonconsumption.R
# 食べない理由：記述と多項ロジット（dish別：q131/q231/q331）
# 依存：long（id×dish_type；consumedなどを含む）, dat_raw（Excelの生データ）

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(checkmate)
  library(brms)
  library(broom.mixed)
  library(forcats)
  library(readr)
})

# --- 1) マッピング：選択肢(1-7) -> reason_cat ---
map_reason_code <- function(x_num) {
  dplyr::case_when(
    x_num %in% c(1, 6) ~ "risk",
    x_num == 2 ~ "dislike",
    x_num == 3 ~ "allergy",
    x_num == 4 ~ "religion",
    x_num == 5 ~ "hardness",
    x_num == 7 ~ "other",
    TRUE       ~ NA_character_
  )
}

# --- 2) wide -> long：q131/q231/q331 を dish_type ごとに整形 ---
extract_reasons_from_raw <- function(dat_raw) {
  assert_data_frame(dat_raw, min.rows = 1)
  df <- dat_raw |> clean_names()
  
  # id 列の代替名にも対応
  if (!"id" %in% names(df)) {
    id_cand <- intersect(names(df), c("respondent_id","resp_id","participant_id"))
    if (length(id_cand) == 1L) {
      df <- df |> rename(id = !!id_cand)
    } else {
      stop("[extract_reasons_from_raw] 'id' 列が見つかりません（候補名も不一致）。")
    }
  }
  
  # 列名は q131, q231, q331（数値コード1-7を想定）
  if (!all(c("q131","q231","q331") %in% names(df)))
    stop("[extract_reasons_from_raw] q131/q231/q331 が見つかりません。")
  
  reasons_long <-
    df |>
    transmute(
      id = as.character(id),
      chopped_fish = suppressWarnings(as.numeric(q131)),
      sliced_fish  = suppressWarnings(as.numeric(q231)),
      pufferfish   = suppressWarnings(as.numeric(q331))
    ) |>
    pivot_longer(
      cols = c(chopped_fish, sliced_fish, pufferfish),
      names_to = "dish_type",
      values_to = "reason_code"
    ) |>
    mutate(
      reason_cat = map_reason_code(reason_code),
      reason_cat = factor(reason_cat, levels = c("risk","dislike","allergy","religion","hardness","other"))
    ) |>
    select(id, dish_type, reason_code, reason_cat)
  
  reasons_long
}

# --- 3) 記述と多項ロジット（consumed==0のみを対象） ---
fit_reason_model <- function(long, reasons_long, out_dir = ".", quick = FALSE) {
  assert_data_frame(long, min.rows = 1)
  assert_data_frame(reasons_long, min.rows = 1)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # --- keys & types ---
  long <- long |> mutate(id = as.character(id))
  reasons_long <- reasons_long |> mutate(id = as.character(id))
  
  # --- base (non-consumers only) ---
  base_noncons <-
    long |>
    filter(consumed == 0) |>
    mutate(
      gender    = as.factor(gender),
      dish_type = forcats::fct_relevel(as.factor(dish_type), "chopped_fish","sliced_fish","pufferfish"),
      district  = as.factor(district)
    )
  
  # --- join reasons ---
  dat <-
    base_noncons |>
    left_join(reasons_long |> select(id, dish_type, reason_cat), by = c("id","dish_type")) |>
    filter(!is.na(reason_cat)) |>
    droplevels()
  
  # --- 早期チェック ---
  K <- nlevels(dat$reason_cat)
  if (K < 2) {
    print(dat |> count(reason_cat))
    stop("[fit_reason_model] reason_cat の水準が1つしかありません。プーリング閾値や join を見直してください。")
  }
  if (nrow(dat) < 50) stop("[fit_reason_model] non-consumers with reasons < 50. Joinキーやカバレッジを確認してください。")
  
  # --- pool rare categories -> "other" ---
  small_levels <- dat |> count(reason_cat) |> filter(n < 15) |> pull(reason_cat) |> as.character()
  if (length(small_levels) > 0) {
    dat <- dat |> mutate(
      reason_cat = forcats::fct_other(reason_cat,
                                      keep = setdiff(levels(reason_cat), small_levels),
                                      other_level = "other")
    )
  }
  dat <- droplevels(dat)
  
  # --- 使用候補の説明変数（season/occasionはそもそも尋ねていない設計） ---
  cand_terms <- c("dish_type", "gender", "age_z", "novelty_z", "tradition_z", "district")
  avail <- cand_terms[cand_terms %in% names(dat)]
  
  non_all_na <- purrr::keep(avail, ~ any(!is.na(dat[[.x]])))
  non_single <- purrr::keep(non_all_na, ~ {
    v <- dat[[.x]]
    if (is.numeric(v)) return(stats::sd(v, na.rm=TRUE) > 0)
    if (is.factor(v) || is.character(v)) return(dplyr::n_distinct(v, na.rm=TRUE) > 1)
    TRUE
  })
  
  # district はランダム効果候補
  has_district_re <- "district" %in% non_single
  rhs_fixed <- setdiff(non_single, "district")
  if (length(rhs_fixed) == 0) rhs_fixed <- "1"
  
  # --- formula（使える列だけ） ---
  re_part <- if (has_district_re) "(1 | district)" else NULL
  rhs <- paste(c(rhs_fixed, re_part), collapse = " + ")
  form <- brms::bf(as.formula(paste("reason_cat ~", rhs)))
  
  # --- 参照カテゴリ（risk が無ければ最頻カテゴリ） ---
  if ("risk" %in% levels(dat$reason_cat)) {
    dat <- dat |> mutate(reason_cat = forcats::fct_relevel(reason_cat, "risk"))
    refcat <- "risk"
  } else {
    refcat <- names(sort(table(dat$reason_cat), decreasing = TRUE))[1]
    dat <- dat |> mutate(reason_cat = forcats::fct_relevel(reason_cat, refcat))
  }
  
  # --- quick mode ---
  dat_use <- if (isTRUE(quick)) slice_sample(dat, n = min(nrow(dat), 3000), seed = 2025) else dat
  dat_use <- droplevels(dat_use)
  
  message("[fit_reason_model] n=", nrow(dat_use),
          " | K=", nlevels(dat_use$reason_cat),
          " | RHS=", rhs,
          " | refcat=", refcat)
  
  # --- priors: モデルに存在するパラメータだけ（カテゴリ別 dpar に付与） ---
  gp <- brms::get_prior(formula = form, data = dat_use,
                        family = brms::categorical(link = "logit", refcat = refcat))
  
  pri_list <- list()
  dpars <- unique(na.omit(gp$dpar))  # 例: "muallergy","mureligion",...
  
  for (dp in dpars) {
    if (any(gp$class == "b" & gp$dpar == dp))
      pri_list <- c(pri_list, list(brms::set_prior("normal(0, 1)", class = "b", dpar = dp)))
    if (any(gp$class == "Intercept" & gp$dpar == dp))
      pri_list <- c(pri_list, list(brms::set_prior("normal(0, 2.5)", class = "Intercept", dpar = dp)))
    if (any(gp$class == "sd" & gp$dpar == dp)) {
      groups <- unique(gp$group[gp$class == "sd" & gp$dpar == dp]); groups <- groups[!is.na(groups)]
      for (g in groups) pri_list <- c(pri_list, list(brms::set_prior("student_t(3, 0, 2.5)", class = "sd", dpar = dp, group = g)))
    }
  }
  priors <- if (length(pri_list) == 0) NULL else if (length(pri_list) == 1) pri_list[[1]] else do.call(c, pri_list)
  
  # --- 並列（cmdstanr + chain 内並列は brms>=2.20 相当で threading()） ---
  threads_arg <- tryCatch(brms::threading(4), error = function(e) NULL)  # 古い brms でも落ちないように
  
  fit <- brms::brm(
    formula = form,
    data    = dat_use,
    family  = brms::categorical(link = "logit", refcat = refcat),
    prior   = priors,
    chains  = 4, iter = 4000, warmup = 2000,
    cores   = 4,
    backend = "cmdstanr",
    threads = threads_arg,
    control = list(adapt_delta = 0.95, max_treedepth = 12),
    save_pars = brms::save_pars(all = TRUE),
    seed = 2025, refresh = 0,
    file = file.path(out_dir, "fit_reasons_categorical"),  # 再利用
    recompile = FALSE
  )
  
  saveRDS(fit, file.path(out_dir, "fit_reasons_categorical.rds"))
  
  # --- 係数（RRR = exp(係数)） ---
  # --- 係数（RRR = exp(係数)） ---
  # brms: パラメータ名例 -> b_muallergy_genderFemale
  post_sum <- brms::posterior_summary(fit, variable = "^b_", regex = TRUE) |> as.data.frame()
  post_sum$param <- rownames(post_sum)
  
  # 念のためのフォールバック（古いbrmsや将来の仕様差異に備える）
  if (nrow(post_sum) == 0) {
    # 全パラメータのサマリを取り、行名でフィルタ
    tmp <- brms::posterior_summary(fit) |> as.data.frame()
    tmp$param <- rownames(tmp)
    post_sum <- dplyr::filter(tmp, grepl("^b_", param))
  }
  
  coef_tbl <- post_sum |>
    dplyr::rename(
      estimate = Estimate,
      est_error = Est.Error,
      q2.5 = Q2.5,
      q97.5 = Q97.5
    ) |>
    tidyr::extract(
      col = param,
      into = c("category","term"),
      regex = "^b_mu([^_]+)_(.+)$",
      remove = TRUE
    ) |>
    dplyr::mutate(
      term = dplyr::if_else(is.na(term), "Intercept", term),
      RRR = exp(estimate),
      RRR_low = exp(q2.5),
      RRR_high = exp(q97.5)
    ) |>
    dplyr::arrange(category, term)
  
  readr::write_excel_csv(coef_tbl, file.path(out_dir, "estimates_reasons_RRR.csv"))
  
  
  # --- 記述（構成比）：dish_type × reason_cat の割合（学習データ基準）
  comp_tbl <- dat_use |>
    count(dish_type, reason_cat, name = "n") |>
    group_by(dish_type) |>
    mutate(prop = n / sum(n)) |>
    ungroup()
  readr::write_excel_csv(comp_tbl, file.path(out_dir, "reasons_composition.csv"))
  
  # --- 代表シナリオ予測：実際にモデルに残った列だけで newdata を作る ---
  model_vars <- setdiff(names(fit$data), "reason_cat")  # brms が保持した列のみ
  wanted_cats <- c("dish_type","gender")                # season/occasion は設計上なし
  used_cats <- intersect(wanted_cats, model_vars)
  
  get_levels_safe <- function(x) {
    if (is.factor(x)) {
      lv <- levels(droplevels(x)); if (length(lv) == 0) NULL else lv
    } else {
      ux <- sort(unique(na.omit(x))); if (length(ux) == 0) NULL else ux
    }
  }
  
  vals <- setNames(vector("list", length(used_cats)), used_cats)
  for (v in used_cats) vals[[v]] <- get_levels_safe(fit$data[[v]])
  vals <- vals[!vapply(vals, is.null, logical(1))]
  
  newdat <- if (length(vals) == 0) tibble(.dummy = 1) else tidyr::expand_grid(!!!vals)
  
  # 数値共変量（モデルに残ったものだけ 0 固定）
  for (nm in intersect(c("age_z","novelty_z","tradition_z"), model_vars)) newdat[[nm]] <- 0
  
  pr <- stats::fitted(fit, newdata = newdat, re_formula = NA, summary = TRUE) |> tibble::as_tibble()
  colnames(pr) <- make.names(colnames(pr))
  pred <- dplyr::bind_cols(newdat, pr)
  readr::write_excel_csv(pred, file.path(out_dir, "predicted_reason_probs.csv"))
  
  message("[fit_reason_model] Saved: fit_reasons_categorical.rds, estimates_reasons_RRR.csv, reasons_composition.csv, predicted_reason_probs.csv")
  fit
}

# ===== 使い方（例） =====
# reasons_long <- extract_reasons_from_raw(dat_raw)
# fit <- fit_reason_model(long, reasons_long, out_dir = "out/reasons", quick = FALSE)
