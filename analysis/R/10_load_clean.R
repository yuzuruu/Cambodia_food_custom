# 10_load_clean.R
# Purpose: load survey data, light cleaning, return analysis-ready frame
# Usage (from driver): out <- load_and_clean(PATH_XLSX, PATH_SHP, out_dir = OUT_DIR)
# Returns: list(dat = tibble, shapefile = sf or NULL)

# RUN_DEMO <- TRUE


suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(sf)
  library(checkmate)
})

# helper: factor if not already
.as_factor_if <- function(x) if (!is.factor(x)) factor(x) else x

# helper: safe shapefile read
.safe_read_sf <- function(p) {
  if (!is.null(p) && file.exists(p)) {
    tryCatch(sf::read_sf(p), error = function(e) NULL)
  } else NULL
}

#' Load and lightly clean the Food Customs survey
#'
#' @param path_xlsx Path to Excel file (sheet "food_custom")
#' @param path_shp  Optional path to KH shapefile (adm4). If missing, returns NULL.
#' @param out_dir   Output directory for snapshots (CSV/RDS). Default "."
#' @param dry_run   If TRUE, only validate structure and return a short report.
#' @return list(dat = tibble, shapefile = sf or NULL) OR (dry_run=TRUE) a small info list
load_and_clean <- function(path_xlsx, path_shp = NULL, out_dir = ".", dry_run = FALSE) {
  
  # ---- validations (fast, no heavy work) ----
  assert_file_exists(path_xlsx, access = "r")
  if (!is.null(path_shp)) assert_file_exists(path_shp, access = "r")
  
  message("[load_and_clean] reading: ", path_xlsx)
  raw <-
    readxl::read_excel(path_xlsx, sheet = "food_custom", col_names = TRUE) %>%
    janitor::clean_names()
  
  # required columns (ranges match your questionnaire)
  must_have <- c("id", "year", "commune", "district", "province_en", "province_kh",
                 "q016","q017","q018","q019","q020","q021","q022",  # demographics
                 "q511","q512","q513","q514","q515","q516","q517","q518","q519","q520","q521","q522","q523","q524") # neophobia block
  assert_subset(must_have, names(raw))
  assert_true(nrow(raw) > 0, .var.name = "raw rows")
  
  if (dry_run) {
    return(list(
      ok = TRUE,
      n_raw = nrow(raw),
      cols_present = intersect(must_have, names(raw)),
      note = "Structure looks good. Set dry_run=FALSE to produce cleaned outputs."
    ))
  }
  
  # ---- light pruning / typing ----
  raw <-
    raw %>%
    select(-any_of(c("q001", "q011"))) %>%                 # documented as unused
    mutate(
      id          = factor(id),
      year        = factor(year),
      commune     = .as_factor_if(commune),
      district    = .as_factor_if(district),
      province_en = .as_factor_if(province_en),
      province_kh = .as_factor_if(province_kh)
    )
  
  # ---- keep respondents with complete demographics + neophobia ----
  complete_ids <-
    raw %>%
    select(id, q016:q022, q511:q524) %>%
    tidyr::drop_na() %>%
    pull(id) %>%
    unique()
  
  dat <-
    raw %>%
    filter(id %in% complete_ids)
  
  # ---- optional shapefile ----
  khm4 <- .safe_read_sf(path_shp)
  
  # ---- snapshot outputs ----
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  readr::write_csv(dat, file.path(out_dir, "clean_dat.csv"))
  readr::write_rds(dat, file.path(out_dir, "clean_dat.rds"))
  
  retained <- tibble(
    n_raw = nrow(raw),
    n_complete = nrow(dat),
    retention_rate = ifelse(nrow(raw) > 0, nrow(dat) / nrow(raw), NA_real_)
  )
  readr::write_csv(retained, file.path(out_dir, "cleaning_summary.csv"))
  
  message("[load_and_clean] kept ", retained$n_complete, " of ", retained$n_raw,
          " (", round(100*retained$retention_rate, 1), "%)")
  
  list(dat = dat, shapefile = khm4)
}

# --- demo runner (runs only if you source this file directly) ----
if (interactive() && identical(sys.nframe(), 0L)) {
  message("[10_load_clean.R] Demo start")
  # PATH_XLSX <- "analysis/data/raw/rawfish_eat_Cambodia_2020_2022.xlsx"
  PATH_SHP  <- "analysis/data/raw/KHM_adm/KHM_adm4.shp"
  OUT_DIR   <- "analysis/outputs"
  PATH_XLSX <- here::here("analysis","data","raw","rawfish_eat_Cambodia_2020_2022.xlsx")
  dat_raw   <- readxl::read_excel(PATH_XLSX, sheet = "food_custom", col_names = TRUE)
  
  # 念のため、id を文字型に統一（後の join の型差エラー回避）
  dat_raw$id <- as.character(dat_raw$id)
  
  # quick structure check (no outputs written)
  print(load_and_clean(PATH_XLSX, PATH_SHP, dry_run = TRUE))
  
  # real run
  out <- load_and_clean(PATH_XLSX, PATH_SHP, out_dir = OUT_DIR, dry_run = FALSE)
  print(dim(out$dat))
  print(dplyr::glimpse(out$dat))
  message("[10_load_clean.R] Demo complete")
}
