################################################
# Food customs survey (Cambodia) — brms analysis
# Pipe style: tidyverse %>%
# Created: 2021-02-10 | Revised: 2025-10-22
################################################

# ---- setup ----
library(tidyverse)
library(readxl)
library(janitor)
library(gtsummary)
library(GGally)
library(sf)
library(brms)
library(cmdstanr)
library(broom.mixed)

set.seed(123)
options(
  mc.cores = max(1, parallel::detectCores() - 1),
  brms.backend = "cmdstanr"
)

# ---- paths ----
PATH_XLSX <- "rawfish_eat_Cambodia_2020_2022.xlsx"
SHEET     <- "food_custom"
PATH_SHP  <- "./KHM_adm/KHM_adm4.shp"   # optional; joins are guarded

OUT_DIR   <- "analysis_outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ---- helpers ----
as_factor_if <- function(x) if (!is.factor(x)) factor(x) else x

safe_read_sf <- function(p) {
  if (file.exists(p)) tryCatch(sf::read_sf(p), error = function(e) NULL) else NULL
}

recode_labeled <- function(x, ...) {
  # dplyr-style case_when wrapper that leaves true NA as NA (no "NA" strings)
  out <- dplyr::case_when(...)
  out[is.na(out)] <- NA
  out
}

# ---- load & pre-clean ----
raw <-
  readxl::read_excel(PATH_XLSX, sheet = SHEET, col_names = TRUE) %>%
  clean_names() %>%
  select(-any_of(c("q001", "q011"))) %>%
  mutate(
    id          = factor(id),
    year        = factor(year),
    commune     = factor(commune),
    district    = factor(district),
    province_en = factor(province_en),
    province_kh = factor(province_kh)
  )

# Keep respondents with complete neophobia + demographic block
complete_ids <-
  raw %>%
  select(id, q016:q022, q511:q524) %>%
  tidyr::drop_na() %>%
  pull(id) %>%
  unique()

dat <-
  raw %>%
  filter(id %in% complete_ids)

# Optional shapefile
khm4 <- safe_read_sf(PATH_SHP)

# ---- neophobia exploration (pairs plot) ----
neo_vars <- names(select(dat, q515:q524))

pairs_plot <-
  dat %>%
  select(all_of(neo_vars)) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
  GGally::ggpairs() +
  theme_classic() +
  theme(strip.background = element_blank())

ggsave(file.path(OUT_DIR, "pairs_neophobia.pdf"),
       plot = pairs_plot, width = 180, height = 180, units = "mm")

# ---- PCA on neophobia block: 3 PCs (novelty, experience, tradition) ----
pca_obj <-
  dat %>%
  select(all_of(neo_vars)) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
  prcomp(scale = TRUE, center = TRUE)

pc_score <-
  pca_obj$x[, 1:3] %>%
  as_tibble() %>%
  `colnames<-`(c("novelty","experience","tradition"))

dat_scored <-
  dat %>%
  bind_cols(pc_score) %>%
  select(-all_of(neo_vars))

# ---- demographic table dataset ----
demo <-
  dat %>%
  select(id, year, village, commune, district, province_en, province_kh, q016:q022) %>%
  rename(
    gender             = q016,
    age                = q017,
    marital_status     = q018,
    family_structure   = q019,
    occupation         = q020,
    revenue_status     = q021,
    revenue_fluctuation= q022
  ) %>%
  mutate(
    across(everything(), as.character),
    age = suppressWarnings(as.numeric(age)),
    gender = recode_labeled(gender,
                            gender == "1" ~ "female",
                            gender == "2" ~ "male",
                            gender == "3" ~ "prefer not to say",
                            TRUE ~ NA_character_
    ),
    marital_status = recode_labeled(marital_status,
                                    marital_status == "1" ~ "Single",
                                    marital_status == "2" ~ "Married with kids",
                                    marital_status == "3" ~ "Married without kids",
                                    marital_status == "4" ~ "Married with grandkids",
                                    marital_status == "5" ~ "Others",
                                    TRUE ~ NA_character_
    ),
    family_structure = recode_labeled(family_structure,
                                      family_structure == "1" ~ "Single",
                                      family_structure == "2" ~ "Couple",
                                      family_structure == "3" ~ "Family",
                                      family_structure == "4" ~ "Three generations",
                                      TRUE ~ NA_character_
    ),
    occupation = recode_labeled(occupation,
                                occupation == "1" ~ "Mainly agriculture",
                                occupation == "2" ~ "More agriculture than fishery",
                                occupation == "3" ~ "More fishery than agriculture",
                                occupation == "4" ~ "Mainly fishery",
                                occupation == "5" ~ "Others",
                                TRUE ~ NA_character_
    ),
    revenue_status = recode_labeled(revenue_status,
                                    revenue_status == "1" ~ "Stable",
                                    revenue_status == "2" ~ "Small fluctuation",
                                    revenue_status == "3" ~ "Large fluctuation",
                                    revenue_status == "4" ~ "Extreme fluctuation",
                                    revenue_status == "5" ~ "Unknown",
                                    TRUE ~ "Unknown"
    ),
    revenue_fluctuation = recode_labeled(revenue_fluctuation,
                                         revenue_fluctuation == "1" ~ "Large decline",
                                         revenue_fluctuation == "2" ~ "Small decline",
                                         revenue_fluctuation == "3" ~ "Small increase",
                                         revenue_fluctuation == "4" ~ "Large increase",
                                         revenue_fluctuation == "5" ~ "Unknown",
                                         TRUE ~ NA_character_
    ),
    across(where(~ !is.numeric(.x)), as_factor_if)
  )

readr::write_rds(demo, file.path(OUT_DIR, "food_custom_demography.rds"))

# Descriptives (Table 1, compact)
tab1 <-
  demo %>%
  group_by(commune, gender) %>%
  summarise(
    N     = n(),
    min   = min(age, na.rm = TRUE),
    mean  = mean(age, na.rm = TRUE),
    median= median(age, na.rm = TRUE),
    max   = max(age, na.rm = TRUE),
    sd    = sd(age, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(tab1, file.path(OUT_DIR, "table1_demographics.csv"))

# ---- merge analysis frame ----
base <-
  dat_scored %>%
  select(
    id, year, month, q100, q200, q300,
    province_kh, province_en, district, commune, village,
    novelty, experience, tradition
  ) %>%
  rename(
    chopped_fish = q100,
    sliced_fish  = q200,
    pufferfish   = q300
  )

anal <-
  base %>%
  left_join(demo, by = "id")

# optional spatial join (if shapefile and fields match)
if (!is.null(khm4)) {
  khm4_key <-
    khm4 %>%
    mutate(
      name_1 = tolower(as.character(NAME_1)),
      name_2 = tolower(as.character(NAME_2)),
      name_3 = tolower(as.character(NAME_3)),
      name_4 = tolower(as.character(NAME_4))
    ) %>%
    st_drop_geometry() %>%
    select(name_1, name_2, name_3, name_4) %>%
    distinct()
  
  anal <-
    anal %>%
    mutate(
      province_kh_l = tolower(as.character(province_kh)),
      district_l    = tolower(as.character(district)),
      commune_l     = tolower(as.character(commune)),
      village_l     = tolower(as.character(village))
    ) %>%
    left_join(
      khm4_key,
      by = c(
        "province_kh_l" = "name_1",
        "district_l"    = "name_2",
        "commune_l"     = "name_3",
        "village_l"     = "name_4"
      )
    ) %>%
    select(-ends_with("_l"))
}

# Recode dish responses 1/2 -> "No"/"Yes" -> factor -> binary
yn_map <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "1" ~ "No",
    x == "2" ~ "Yes",
    TRUE     ~ NA_character_
  )
}

anal <-
  anal %>%
  mutate(
    across(c(chopped_fish, sliced_fish, pufferfish), yn_map),
    across(c(chopped_fish, sliced_fish, pufferfish), ~ factor(.x, levels = c("No","Yes"))),
    month   = factor(month),
    district= factor(district),
    commune = factor(commune),
    year    = factor(year),
    gender  = factor(gender)
  )

# Long format for dish-specific modeling
long <-
  anal %>%
  select(
    id, district, commune, year, month, gender, age,
    novelty, experience, tradition,
    chopped_fish, sliced_fish, pufferfish
  ) %>%
  pivot_longer(
    cols      = c(chopped_fish, sliced_fish, pufferfish),
    names_to  = "dish_type",
    values_to = "yes_no"
  ) %>%
  mutate(
    consumed      = if_else(yes_no == "Yes", 1L, 0L),
    age_z         = as.numeric(scale(age)),
    novelty_z     = as.numeric(scale(novelty)),
    experience_z  = as.numeric(scale(experience)),
    tradition_z   = as.numeric(scale(tradition))
  )

readr::write_rds(long, file.path(OUT_DIR, "analysis_long.rds"))

# ---- brms: Stage 1 (Participation; Bernoulli) ----
pri_part <- c(
  prior(normal(0, 1), class = "b"),
  prior(normal(0, 2.5), class = "Intercept"),
  prior(student_t(3, 0, 2.5), class = "sd")
)

form_part <- bf(
  consumed ~ gender + age_z + novelty_z + experience_z + tradition_z +
    dish_type + month + (1 | district)
)

fit_part <-
  brm(
    formula = form_part,
    data    = long,
    family  = bernoulli(link = "logit"),
    prior   = pri_part,
    chains  = 4, iter = 4000, warmup = 2000,
    control = list(adapt_delta = 0.95, max_treedepth = 12),
    seed    = 2025
  )

saveRDS(fit_part, file.path(OUT_DIR, "fit_participation.rds"))
print(summary(fit_part), digits = 2)
pp_check(fit_part, type = "bars", ndraws = 50)

# ---- brms: Stage 2 (Frequency; truncated NB) [optional] ----
if ("frequency" %in% names(long)) {
  long_cons <-
    long %>%
    filter(consumed == 1, !is.na(frequency), frequency > 0)
  
  pri_freq <- c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(student_t(3, 0, 2.5), class = "sd"),
    prior(gamma(2, 0.1), class = "shape") # or prior(normal(0,1), class = "logshape")
  )
  
  form_freq <- bf(
    frequency | trunc(lb = 1) ~ gender + age_z + novelty_z + experience_z + tradition_z +
      dish_type + month + (1 | district)
  )
  
  fit_freq <-
    brm(
      formula = form_freq,
      data    = long_cons,
      family  = negbinomial(link = "log"),
      prior   = pri_freq,
      chains  = 4, iter = 4000, warmup = 2000,
      control = list(adapt_delta = 0.95, max_treedepth = 12),
      seed    = 2025
    )
  
  saveRDS(fit_freq, file.path(OUT_DIR, "fit_frequency_truncNB.rds"))
  print(summary(fit_freq), digits = 2)
  pp_check(fit_freq, type = "hist")
  pp_check(fit_freq, type = "bars", ndraws = 50)
}

# ---- tidy exports for manuscript ----
est_part <-
  broom.mixed::tidy(fit_part, effects = "fixed") %>%
  mutate(
    OR   = exp(estimate),
    OR_l = exp(estimate - 1.96 * std.error),
    OR_u = exp(estimate + 1.96 * std.error)
  )

readr::write_csv(est_part, file.path(OUT_DIR, "estimates_participation_OR.csv"))

if (exists("fit_freq")) {
  est_freq <-
    broom.mixed::tidy(fit_freq, effects = "fixed") %>%
    mutate(
      RR   = exp(estimate),
      RR_l = exp(estimate - 1.96 * std.error),
      RR_u = exp(estimate + 1.96 * std.error)
    )
  readr::write_csv(est_freq, file.path(OUT_DIR, "estimates_frequency_RR.csv"))
}

message("Done. Outputs in: ", OUT_DIR)
