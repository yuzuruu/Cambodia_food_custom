# 45_make_wtp_data.R
# Build wtp_data.rds from q117/q118/q217/q218/q317/q318
# Output schema:
#   id (chr), price (num), grams (num, NA), district (fct), gender (fct),
#   dish_type (fct: chopped_fish/sliced_fish/pufferfish),
#   month (fct), season (fct: wet/dry), youandme (fct: self/others)

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(readxl)
})

# ---- helpers -----------------------------------------------------------------
num_clean_999 <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  v[v == 999] <- NA_real_
  v
}

# month -> season（カンボジアの一般的な割当て。必要に応じて調整）
month_to_season <- function(m) {
  m_chr <- as.character(m)
  dplyr::case_when(
    m_chr %in% c("May","June","July","August","September","October","5","6","7","8","9","10") ~ "wet",
    m_chr %in% c("November","December","January","February","March","April","11","12","1","2","3","4") ~ "dry",
    TRUE ~ NA_character_
  )
}

# ---- main builder -------------------------------------------------------------
# dat_raw: data.frame を想定（Excelの "food_custom" をそのまま渡してOK）
build_wtp_from_qs <- function(dat_raw) {
  stopifnot(is.data.frame(dat_raw))
  need <- c("q117","q118","q217","q218","q317","q318")
  missing <- setdiff(need, names(dat_raw))
  if (length(missing) > 0) {
    stop("Missing WTP columns: ", paste(missing, collapse = ", "))
  }
  
  # pivot 前にID・性別・地区・月を整形しておく（長さ不一致対策）
  base <- dat_raw |>
    mutate(
      id = if ("id" %in% names(dat_raw)) as.character(.data[["id"]]) else as.character(dplyr::row_number()),
      # 性別（q016 があれば 1=female, 2=male, 3=prefer_not）
      gender = if ("q016" %in% names(dat_raw)) dplyr::case_when(
        .data[["q016"]] %in% c("1", 1) ~ "female",
        .data[["q016"]] %in% c("2", 2) ~ "male",
        .data[["q016"]] %in% c("3", 3) ~ "prefer_not",
        TRUE ~ NA_character_
      ) else if ("gender" %in% names(dat_raw)) as.character(.data[["gender"]]) else NA_character_,
      district = if ("district" %in% names(dat_raw)) as.factor(.data[["district"]]) else factor(NA),
      month    = if ("month"    %in% names(dat_raw)) as.factor(.data[["month"]])    else factor(NA)
    ) |>
    select(id, gender, district, month, all_of(need))
  
  # マッピング表（列名 -> dish_type & youandme）
  spec <- tibble::tribble(
    ~col,   ~dish_type,      ~youandme,
    "q117", "chopped_fish",  "self",
    "q118", "chopped_fish",  "others",
    "q217", "sliced_fish",   "self",
    "q218", "sliced_fish",   "others",
    "q317", "pufferfish",    "self",
    "q318", "pufferfish",    "others"
  )
  
  out <-
    base |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(need),
      names_to = "col", values_to = "price"
    ) |>
    dplyr::left_join(spec, by = "col") |>
    dplyr::mutate(
      price     = num_clean_999(price),
      dish_type = factor(dish_type, levels = c("chopped_fish","sliced_fish","pufferfish")),
      youandme  = factor(youandme, levels = c("self","others")),
      gender    = factor(gender, levels = c("female","male","prefer_not")),
      season    = factor(month_to_season(month), levels = c("wet","dry")),
      grams     = NA_real_
    ) |>
    dplyr::filter(!is.na(price), price > 0) |>
    dplyr::select(id, price, grams, district, gender, dish_type, month, season, youandme)
  
  return(out)
}

# ---- convenience saver --------------------------------------------------------
# Excel を読み込んで wtp_data.rds を保存するユーティリティ
save_wtp_rds_from_excel <- function(path_xlsx,
                                    sheet = "food_custom",
                                    out_rds = here::here("analysis","outputs","wtp_data.rds")) {
  dat_raw <- readxl::read_excel(path_xlsx, sheet = sheet, col_names = TRUE)
  wtp <- build_wtp_from_qs(dat_raw)
  dir.create(dirname(out_rds), showWarnings = FALSE, recursive = TRUE)
  readr::write_rds(wtp, out_rds)
  message("[WTP] saved: ", out_rds, " (n=", nrow(wtp), ")")
  invisible(out_rds)
}

# ---- direct execution (optional) ---------------------------------------------
# このファイルを単体で source したときだけ、Excel -> RDS を自動生成
if (interactive() && identical(sys.nframe(), 0L)) {
  PATH_XLSX <- here::here("analysis","data","raw","rawfish_eat_Cambodia_2020_2022.xlsx")
  save_wtp_rds_from_excel(PATH_XLSX)
}
