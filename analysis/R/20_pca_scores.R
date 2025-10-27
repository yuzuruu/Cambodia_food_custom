# 20_pca_scores.R (no plots)
# Purpose: compute neophobia PCA (scores only) & build long-format dataset
# Usage: pca_out <- make_pca(dat, out_dir = OUT_DIR, quick = FALSE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(checkmate)
})

# ---- main ----
make_pca <- function(dat, out_dir = ".", quick = FALSE, dry_run = FALSE) {
  # --- validations ---
  assert_data_frame(dat, min.rows = 1)
  must_have <- c(
    "id","year","month","district","commune","village",
    "q100","q200","q300",      # dishes (1/2)
    "q016","q017",             # gender, age
    # neophobia items (q511–q524)
    paste0("q51",1:9), "q520","q521","q522","q523","q524"
  )
  assert_subset(must_have, names(dat))
  
  if (dry_run) {
    return(list(
      ok = TRUE,
      n_rows = nrow(dat),
      n_cols = ncol(dat),
      has_neophobia = all(paste0("q51",1:9) %in% names(dat)) &&
        all(c("q520","q521","q522","q523","q524") %in% names(dat)),
      note = "Set dry_run=FALSE to compute PCA and write outputs."
    ))
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # --- optional quick mode ---
  dat_use <- if (isTRUE(quick)) dplyr::slice_sample(dat, n = min(nrow(dat), 3000), seed = 2025) else dat
  
  # --- neophobia item names (explicit to avoid range pitfalls) ---
  neo_vars <- paste0("q5", c(sprintf("1%01d",1:9), 20:24))  # q511..q519, q520..q524
  neo_vars <- intersect(neo_vars, names(dat_use))           # safety
  
  # --- numeric coercion (robust) ---
  neo_num <- dat_use %>%
    dplyr::select(dplyr::all_of(neo_vars)) %>%
    dplyr::mutate(dplyr::across(everything(), ~ suppressWarnings(as.numeric(.x))))
  
  # 完全に全部 NA の列は落とす（prcompが落ちないように）
  drop_all_na <- vapply(neo_num, function(x) all(is.na(x)), logical(1))
  if (any(drop_all_na)) {
    neo_num <- neo_num[, !drop_all_na, drop = FALSE]
    neo_vars <- neo_vars[!drop_all_na]
  }
  if (ncol(neo_num) < 3) stop("[make_pca] 有効なネオフォビア項目が 3 列未満です。")
  
  # 欠損の扱い：行完全ケースに限定（PCAはNA不可）
  row_ok <- stats::complete.cases(neo_num)
  if (!any(row_ok)) stop("[make_pca] ネオフォビア項目がすべて欠損です。")
  neo_num_cc <- neo_num[row_ok, , drop = FALSE]
  dat_cc     <- dat_use[row_ok, , drop = FALSE]
  
  # --- PCA (3 PCs) ---
  pca_obj <- stats::prcomp(neo_num_cc, scale. = TRUE, center = TRUE)
  
  # loadings / importance
  readr::write_csv(tibble::as_tibble(pca_obj$rotation, rownames = "item"),
                   file.path(out_dir, "pca_rotation.csv"))
  readr::write_csv(tibble::as_tibble(summary(pca_obj)$importance, rownames = "metric"),
                   file.path(out_dir, "pca_importance.csv"))
  
  # scores -> first 3 PCs
  pcs <- as_tibble(pca_obj$x[, 1:3, drop = FALSE], .name_repair = "minimal")
  names(pcs) <- c("novelty","experience","tradition")
  
  dat_scored <- dat_cc %>%
    dplyr::bind_cols(pcs) %>%
    dplyr::select(-dplyr::all_of(neo_vars))
  
  # --- dishes 1/2 -> consumed 0/1; longize ---
  yn_to_consumed <- function(x) dplyr::case_when(
    as.character(x) == "2" ~ 1L,  # Yes
    as.character(x) == "1" ~ 0L,  # No
    TRUE ~ NA_integer_
  )
  
  anal <- dat_scored %>%
    dplyr::select(
      id, year, month, district, commune, village,
      q016, q017, q022, q100, q200, q300,
      novelty, experience, tradition
    ) %>%
    dplyr::rename(
      gender = q016, age = q017,
      chopped_fish = q100, sliced_fish = q200, pufferfish = q300
    ) %>%
    dplyr::mutate(
      gender = forcats::fct_recode(as.factor(gender),
                                   "female"="1","male"="2","prefer_not"="3"),
      month = as.factor(month),
      district = as.factor(district),
      commune  = as.factor(commune),
      year     = as.factor(year)
    )
  
  long <- anal %>%
    tidyr::pivot_longer(
      cols = c(chopped_fish, sliced_fish, pufferfish),
      names_to = "dish_type", values_to = "yes_no"
    ) %>%
    dplyr::mutate(
      consumed = yn_to_consumed(yes_no),
      age   = suppressWarnings(as.numeric(age)),
      age_z = as.numeric(scale(age)),
      novelty_z    = as.numeric(scale(novelty)),
      experience_z = as.numeric(scale(experience)),
      tradition_z  = as.numeric(scale(tradition)),
      dish_type = forcats::fct_relevel(as.factor(dish_type), "chopped_fish")
    )
  
  # q022 normalize → ord(1..4) / cat5(1..5)
  normalize_q022_any <- function(x) {
    code_num <- suppressWarnings(readr::parse_number(as.character(x)))
    lab <- tolower(trimws(as.character(x)))
    map_labels <- c("large decline"=1L,"small decline"=2L,"small increase"=3L,"large increase"=4L,"unknown"=5L)
    code_lab <- dplyr::recode(lab, !!!map_labels, .default = NA_integer_)
    code <- dplyr::coalesce(code_num, code_lab)
    code[!(code %in% 1:5)] <- NA_integer_
    code
  }
  
  long <- long %>%
    dplyr::mutate(
      q022_code = normalize_q022_any(q022),
      q022_ord  = factor(dplyr::if_else(q022_code %in% 1:4, q022_code, NA_integer_),
                         levels = 1:4,
                         labels = c("large decline","small decline","small increase","large increase"),
                         ordered = TRUE),
      q022_cat5 = factor(q022_code, levels = 1:5,
                         labels = c("large decline","small decline","small increase","large increase","unknown")),
      q022_unknown = as.integer(q022_code == 5L)
    )
  
  # outputs
  readr::write_rds(long, file.path(out_dir, "analysis_long.rds"))
  readr::write_excel_csv(long, file.path(out_dir, "analysis_long.csv"))
  
  list(long = long, pca = pca_obj)
}

# demo (optional)
if (interactive() && identical(sys.nframe(), 0L)) {
  OUT_DIR <- "analysis/outputs"
  clean_path <- file.path(OUT_DIR, "clean_dat.rds")
  stopifnot(file.exists(clean_path))
  dat <- readr::read_rds(clean_path)
  res <- make_pca(dat, out_dir = OUT_DIR, quick = TRUE, dry_run = FALSE)
  print(res$long %>% dplyr::count(dish_type, consumed))
}

