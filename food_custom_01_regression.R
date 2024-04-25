################################################
# food custom survey in Cambodia
# Part I Analysing questionnaire results
# Original: 10th. February 2021
# Revised: 18th. March 2024
# Yuzuru Utsunomiya, Ph.D.
################################################
#
# ----- read.library ----
# read library
library(tidyverse)
library(janitor)
library(patchwork)
library(GGally)
library(ggmosaic)
library(khroma)
library(sf)
library(tableone)
library(osmdata)
library(brms)
library(cmdstanr)
library(rfishbase)
library(gtsummary)
library(patchwork)
library(ggbiplot)
library(cowplot)
library(magick)
#
# ----- load.data -----
# read data
food_custom_original <- 
  readxl::read_excel(
    path = "rawfish_eat_Cambodia_2020_2022.xlsx",
    sheet = "food_custom",
    col_names = TRUE
  ) %>% 
  # dplyr::filter(year != "2021") %>% 
  dplyr::select(-q001, -q011) %>% 
  dplyr::mutate(
    id = factor(id), 
    year = factor(year),
    commune = factor(commune),
    district = factor(district),
    province_en = factor(province_en),
    province_kh = factor(province_kh)
    )
# identify ids that complete demographic traits and 
# questions clarifying neophbia
food_custom_complete_id <- 
  food_custom_original %>% 
  dplyr::select(id,q016:q022, q511:q524) %>% 
  na.omit() %>% 
  dplyr::select(id)
# pick up complete cases using the ids above
food_custom <- 
  food_custom_original %>% 
  dplyr::filter(id %in% food_custom_complete_id$id)
# # shapefiles
shp_khm <-
  sf::read_sf(
    "./KHM_adm/KHM_adm4.shp"
    )
# save the results
readr::write_excel_csv(food_custom, "food_custom.csv")
# 
# preliminary analysis for variable selection
# (target: questions on neophobia)
# scatter plot matrix
# To avoid multicorlinearity, we decide to remove q514, 
# since the q514 and q515 have a strong relationship (r >0.90).
food_custom_pairsplot <- 
  food_custom %>% 
  dplyr::select(
    c(q515:q524)
    ) %>%
  dplyr::mutate(
    across(
      where(is.character),
      as.numeric
      )
    ) %>% 
  GGally::ggpairs() +
  theme_classic() +
  theme(
      strip.background = element_blank()
    )

ggsave(
  "food_custom_pairsplot.pdf",
  plot = food_custom_pairsplot,
  width = 300,
  height = 300,
  units = "mm"
)



food_custom_descriptive_statistics <- 
  food_custom %>% 
  dplyr::select(c(q515:q524)) %>%
  dplyr::mutate(across(where(is.character), as.numeric)) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response"
  ) %>% 
  dplyr::group_by(question) %>% 
  dplyr::summarise(
    N = n(),
    Min. = min(response),
    Mean = mean(response),
    Median = median(response),
    Max. = max(response),
    SD = sd(response),
    SE = sd(response)/(sqrt(n()))
  )
readr::write_excel_csv(food_custom_descriptive_statistics, "food_custom_descriptive_statistics.csv")



food_custom_corrr <- 
  food_custom %>% 
  dplyr::select(c(q515:q524)) %>%
  dplyr::mutate(across(where(is.character), as.numeric)) %>%
  corrr::correlate(., y = NULL,  use = "pairwise.complete.obs", method = "pearson") %>% 
  corrr::shave()
readr::write_excel_csv(food_custom_corrr, "food_custom_corrr.csv")




# Confirm internal consistency using omega
food_custom_psy <- 
  food_custom %>% 
  # select questions related to foodfobia
  dplyr::select(q515:q524) %>%
  dplyr::mutate(
    dplyr::across(
      everything(), 
      as.numeric
      )
    ) %>% 
  # N. of factors were adjusted in accordance with lowest BIC
  # We obtained the following results
  # omega_total: 0.78 -> consistent questions
  # BIC : 160.48
  psych::omega(nfactors = 6)
# PCA for compressing the variables
# We obtained 3 principal components;
# PC1. Novelty: Whether a new food is welcome or not.
# PC2. Experience: Whether a person has eaten a new food or not. 
# PC3. Tradition: Whether conventional ethnic foods are preferred or not   
# To fix the N. of PC, refer to the document below (p. 23).
# https://lecture.ecc.u-tokyo.ac.jp/~aiwata/biostat_basic/text4lec3.pdf
# It is so-called Kaizer-Guttman rule
# Stopping Rules in Principal Components Analysis: A Comparison of Heuristical and Statistical Approaches Author(s): Donald A. Jackson Source: Ecology, Dec., 1993, Vol. 74, No. 8 (Dec., 1993), pp. 2204-2214 
food_custom_pca <- 
  food_custom %>% 
  dplyr::select(q515:q524) %>% 
  dplyr::mutate(across(everything(), as.numeric)) %>% 
  prcomp(scale = TRUE, center = TRUE)
# check the PC scores
readr::write_excel_csv(as_tibble(food_custom_pca$rotation), "food_custom_pca_rotation.csv")
# check the PC scores
importance <- summary(food_custom_pca)
readr::write_excel_csv(as_tibble(importance$importance), "food_custom_pca_importance.csv")



# check cum score and SD.
# bind the PCA score to original data
food_custom_food_score <- 
  food_custom %>% 
  dplyr::bind_cols(
    tibble::as_tibble(
      food_custom_pca$x[,c(1,2,3)]
      ) %>% 
      data.table::setnames(
        c(
          "novelty","experience","tradition"
          )
        )
    ) %>% 
  dplyr::select(-c(q515:q524))
# 
# ----- table.1 ----
# Table Demographic status by gender
# code:
# age: 017
# gender: gender
# marial status: 018  
# family structure: 19
# occupation: 20
# income fluctuation: 21
# revenue: 22
# district: district 
# commune: commune
# village: village
# 
# make a special dataset for the table 1
food_custom_demography <- 
  food_custom %>% 
  # pick up necessary part
  dplyr::select(id, year, village, commune, district, province_en, province_kh, q016:q022) %>% 
  # change variables' names
  data.table::setnames(
    c(
      "id", 
      "year",
      "village", 
      "commune", 
      "district", 
      "province_en", 
      "province_kh",
      "gender",
      "age",
      "marital_status",
      "family_structure",
      "occupation",
      "revenue_status",
      "revenue_fluctuation"
    )
  ) %>% 
  # change data type
  dplyr::mutate(
    dplyr::across(
      everything(), 
      factor
      )
    ) %>%
  dplyr::mutate(age = as.numeric(as.character(age)))  %>% 
  # replace reply codes to names
  # to display the name. Otherwise, only the code number will be.
  dplyr::mutate(
    gender = dplyr::case_when(
      gender == "1" ~ "female",
      gender == "2" ~ "male",
      gender == "3"  ~ "prefer not to say",
      TRUE  ~ "NA"
    ),
    marital_status = dplyr::case_when(
      marital_status == 1 ~ "Single",
      marital_status == 2 ~ "Married with kids",
      marital_status == 3 ~ "Married without kids",
      marital_status == 4 ~ "Married with grandkids",
      marital_status == 5 ~ "Others",
      TRUE  ~ "NA"
    ),
    family_structure = dplyr::case_when(
      family_structure == 1 ~ "Single",
      family_structure == 2 ~ "Couple",
      family_structure == 3 ~ "Family",
      family_structure == 4 ~ "living together with 3 generations",
      TRUE  ~ "NA"
    ),
    occupation = dplyr::case_when(
      occupation == 1 ~ "Mainly agriculture",
      occupation == 2 ~ "Biased toward agriculture more than fishewry",
      occupation == 3 ~ "Biased toward fishery more than agriculture",
      occupation == 4 ~ "Mainly fishery",
      occupation == 5 ~ "Others",
      TRUE  ~ "NA"
    ),
    revenue_status = dplyr::case_when(
      revenue_status == 1 ~ "Stable",
      revenue_status == 2 ~ "Fluctuated a bit",
      revenue_status == 3 ~ "Large fluctuation",
      revenue_status == 4 ~ "Extreme fluctuation",
      revenue_status == 5 ~ "Unknown",
      TRUE  ~ "Unknown"
      # TRUE  ~ "NA"
    ),
    revenue_fluctuation = dplyr::case_when(
      revenue_fluctuation == 1 ~ "Large decline",
      revenue_fluctuation == 2 ~ "Small decline",
      revenue_fluctuation == 3 ~ "Small increase",
      revenue_fluctuation == 4 ~ "Large increase",
      revenue_fluctuation == 5 ~ "Unknown",
      TRUE  ~ "NA"
    )
  )
#
# make the table 1
food_custom_table01 <- 
  food_custom_demography %>% 
  group_by(commune, gender) %>% 
  dplyr::summarise(
    N = n(),
    Min. = min(age),
    Mean = mean(age),
    Median = median(age),
    Max. = max(age),
    SD = sd(age)
  )
readr::write_excel_csv(food_custom_table01, "food_custom_table01.csv")
# bind altogether
food_custom_total <- 
  food_custom %>% 
  dplyr::select(id, month, q100, q200, q300) %>%
  data.table::setnames(
    c("id","month","chopped_fish","sliced_fish","pufferfish")
    ) %>% 
  dplyr::left_join(
    food_custom_demography, 
    by = "id"
    ) %>% 
  dplyr::left_join(
    food_custom_food_score %>% dplyr::select(id, novelty, experience, tradition), 
    by = "id"
    ) %>% 
  dplyr::select(id, year, month, province_kh, province_en, district, commune, village, gender, age, marital_status, family_structure, occupation, revenue_status, revenue_fluctuation, chopped_fish, sliced_fish, pufferfish, novelty, experience, tradition) %>%
  dplyr::left_join(
    shp_khm, 
    by = c(
      "province_kh" = "NAME_1", 
      "district" = "NAME_2", 
      "commune" = "NAME_3", 
      "village" = "NAME_4"
      )
    ) %>% 
  dplyr::mutate(
    chopped_fish = dplyr::case_when(
      chopped_fish == "1" ~ "No", # No
      chopped_fish == "2" ~ "Yes", # Yes
      TRUE  ~ "NA"
      ),
    sliced_fish = dplyr::case_when(
      sliced_fish == "1" ~ "No", # No
      sliced_fish == "2" ~ "Yes", # Yes
      TRUE  ~ "NA"
      ),
    pufferfish = dplyr::case_when(
      pufferfish == "1" ~ "No", # No
      pufferfish == "2" ~ "Yes", # Yes
      TRUE  ~ "NA"
      )
    )
# make a table one
gtsummary::theme_gtsummary_mean_sd()
food_custom_total_tableone <- 
  food_custom_total %>% 
  dplyr::select(year, month, province_en, district, gender, age, marital_status, family_structure, occupation, revenue_status, revenue_fluctuation, chopped_fish, sliced_fish, pufferfish, novelty, experience, tradition) %>% 
  gtsummary::tbl_strata(
    strata = month, 
    .tbl_fun =
      ~ .x %>% 
      gtsummary::tbl_summary(
        by = year,
        type = list(chopped_fish ~ "categorical", sliced_fish ~ "categorical", pufferfish ~ "categorical"),
        include = c(district, gender, age, marital_status, family_structure, occupation, revenue_status, revenue_fluctuation, chopped_fish, sliced_fish, pufferfish, novelty, experience, tradition)
        # label = list(
        #   HICOV = "Any health insurance",
        #   ESRG = "Employment",
        #   EXPANSION = "Expansion"
        )
    )
# save the table
food_custom_total_tableone %>% gtsummary::as_tibble() %>% writexl::write_xlsx(., "food_custom_total_tableone.xlsx")

food_custom_long <- 
  food_custom_total %>% 
  dplyr::select(
    age, gender, revenue_fluctuation, novelty, experience, tradition, district, chopped_fish, sliced_fish, pufferfish
  ) %>% 
  tidyr::pivot_longer(
    cols = c(chopped_fish, sliced_fish, pufferfish),
    names_to = "dish_type",
    values_to = "yes_no"
  )
# 
# ----- bayesian.logit -----
# NOTE
# 1. This computation spends looong periods.
# Comment out when not in use.
# # compile the model
# food_custom_logistic_01 <-
#   brms::brm(
#     yes_no ~ dish_type + age + gender + revenue_fluctuation + novelty + experience + tradition + district,
#     data = food_custom_long,
#     family = bernoulli(link = "logit"),
#     warmup = 50000,
#     iter = 200000,
#     chains = 4,
#     cores= 4,
#     seed = 123,
#     refresh = 100,
#     backend = "cmdstanr"
#   )
# # print summary of the Bayesian regression results above
# summary(food_custom_logistic_01)
# # save the results
# readr::write_rds(food_custom_logistic_01, "food_custom_logistic_01.rds")
# # 
# # loo(food_custom_logistic_01)
# # plot(food_custom_logistic_01)
# # 
# # Bayesian regression by recipe
# # 2. Regression by dish recipe
# food_custom_logistic_02 <-
#   food_custom_long %>%
#   group_by(dish_type) %>%
#   nest() %>%
#   dplyr::mutate(
#     regression_results = purrr::map(
#       data,
#       ~
#         brms::brm(
#           yes_no ~ age + gender + revenue_fluctuation + novelty + experience + tradition + district,
#           data = .,
#           family = bernoulli(link = "logit"),
#           warmup = 50000,
#           iter = 200000,
#           chains = 4,
#           cores= 4,
#           seed = 123,
#           refresh = 100,
#           backend = "cmdstanr"
#         )
#     )
#   )
# # print summary of the logistic regression by recipe
# summary(food_custom_logistic_02$regression_results[[1]])
# summary(food_custom_logistic_02$regression_results[[2]])
# summary(food_custom_logistic_02$regression_results[[3]])
# readr::write_rds(food_custom_logistic_02, "food_custom_logistic_02.rds")
# 
# hoge <- readr::read_rds("food_custom_logistic_01.rds")
# summary(hoge)
# fuga <- readr::read_rds("food_custom_logistic_02.rds")
# fuga$regression_results
# 
# compile the model
# 3. Mixed model
# food_custom_logistic_03 <-
#   brms::brm(
#     yes_no ~ dish_type + age + gender + revenue_fluctuation + novelty + experience + tradition + district + (1 + revenue_fluctuation|district),
#     data = food_custom_long,
#     family = bernoulli(link = "logit"),
#     warmup = 50000,
#     iter = 200000,
#     chains = 4,
#     cores= 4,
#     seed = 123,
#     refresh = 100,
#     backend = "cmdstanr"
#   )
# # # print summary of the Bayesian regression results above
# summary(food_custom_logistic_03)
# # # save the results
# readr::write_rds(food_custom_logistic_03, "food_custom_logistic_03.rds")
# loo(food_custom_logistic_03, moment_match = TRUE)
# plot(food_custom_logistic_03)
# # 
# # 4. Mixed model by recipe
# food_custom_logistic_04 <-
#   brms::brm(
#     yes_no ~ dish_type + age + gender + revenue_fluctuation + novelty + experience + tradition + district + (1 |dish_type + district),
#     data = food_custom_long,
#     family = bernoulli(link = "logit"),
#     warmup = 500,
#     iter = 2000,
#     chains = 4,
#     cores= 4,
#     seed = 123,
#     refresh = 100,
#     backend = "cmdstanr"
#   )
# # print summary of the Bayesian regression results above
# summary(food_custom_logistic_04)
# # save the results
# readr::write_rds(food_custom_logistic_04, "food_custom_logistic_04.rds")
# # loo(food_custom_logistic_04)
# # plot(food_custom_logistic_04)
# # 4. Mixed model by recipe and commune
# food_custom_logistic_05 <-
#   brms::brm(
#     yes_no ~ dish_type + age + gender + revenue_fluctuation + novelty + experience + tradition + district + (1 + age + gender + revenue_fluctuation|dish_type + district),
#     data = food_custom_long,
#     family = bernoulli(link = "logit"),
#     warmup = 500,
#     iter = 2000,
#     chains = 4,
#     cores= 4,
#     seed = 123,
#     refresh = 100,
#     backend = "cmdstanr"
#   )
# # print summary of the Bayesian regression results above
# summary(food_custom_logistic_05)
# # save the results
# readr::write_rds(food_custom_logistic_05, "food_custom_logistic_05.rds")
# # loo(food_custom_logistic_05)
# # plot(food_custom_logistic_05)
# 6. by recipe with random effects
# food_custom_logistic_06 <-
#   food_custom_long %>%
#   group_by(dish_type) %>%
#   nest() %>%
#   dplyr::mutate(
#     regression_results = purrr::map(
#       data,
#       ~
#         brms::brm(
#           yes_no ~ age + gender + revenue_fluctuation + novelty + experience + tradition + district + (1 + revenue_fluctuation|district),
#           data = .,
#           family = bernoulli(link = "logit"),
#           warmup = 50000,
#           iter = 200000,
#           chains = 4,
#           cores= 4,
#           seed = 123,
#           refresh = 100,
#           backend = "cmdstanr"
#         )
#     )
#   )
# # print summary of the logistic regression by recipe
# summary(food_custom_logistic_06$regression_results[[1]])
# summary(food_custom_logistic_06$regression_results[[2]])
# summary(food_custom_logistic_06$regression_results[[3]])
# readr::write_rds(food_custom_logistic_06, "food_custom_logistic_06.rds")
# 
# ----- bayesian.logit.summary -----
# read data and make summary tables
food_custom_logistic_01 <- 
  readr::read_rds("food_custom_logistic_01.rds")
food_custom_logistic_02 <- 
  readr::read_rds("food_custom_logistic_02.rds")
summary(food_custom_logistic_01)
summary(food_custom_logistic_02$regression_results[[1]])
summary(food_custom_logistic_02$regression_results[[2]])
summary(food_custom_logistic_02$regression_results[[3]])
# save the summary table using MSExcel format
# model 1
gtsummary::tbl_regression(food_custom_logistic_01, intercept = TRUE) %>% 
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "food_custom_logistic_01_summary.xlsx")
# model 2 (chopped small fish)
gtsummary::tbl_regression(food_custom_logistic_02$regression_results[[1]], intercept = TRUE) %>% 
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "food_custom_logistic_02_01_summary.xlsx")
# model 2 (sliced large fish)
gtsummary::tbl_regression(food_custom_logistic_02$regression_results[[2]], intercept = TRUE) %>% 
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "food_custom_logistic_02_02_summary.xlsx")
# model 2 (pufferfish)
gtsummary::tbl_regression(food_custom_logistic_02$regression_results[[3]], intercept = TRUE) %>% 
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "food_custom_logistic_02_03_summary.xlsx")
# figure
plot_interval_smallfish <- 
  bayesplot::mcmc_intervals(
    food_custom_logistic_02$regression_results[[1]], 
    pars = vars(contains("b_")),
    point_est = c("mean"),
    prob = 0.5, 
    prob_outer = 0.95
    ) +
  labs(title = "Chopped small fish") +
  xlim(-4,4) +
  scale_y_discrete(
    labels = 
        c(
          "b_Intercept" = "Intercept",
          "b_age" = "Age",
          "b_gendermale" = "Male",
          "b_districtStuengTraeng" = "District \n (Stueng Traeng)",
          "b_districtSiemBouk" = "District \n (Siem Bouk)",
          "b_districtSambour" = "District \n (Sambour)",
          "b_districtPreaekPrasab" = "District \n (Preaek Prasab)",
          "b_districtKracheh" = "District \n (Kracheh)",
          "b_tradition" = "Tradition",
          "b_experience" = "Experience",
          "b_novelty" = "Novelty",
          "b_revenue_fluctuationUnknown" = "Revenue \n (Unknown)",
          "b_revenue_fluctuationSmallincrease" = "Revenue \n (Small increase)",
          "b_revenue_fluctuationLargeincrease" = "Revenue \n (Large increase)",
          "b_revenue_fluctuationSmalldecline" = "Revenue \n (Small decline)"
        )
    )
# 
# ----- bayesian.logit.plot -----
# sliced large fish 
plot_interval_largefish <- 
  bayesplot::mcmc_intervals(
    food_custom_logistic_02$regression_results[[2]], 
    pars = vars(contains("b_")),
    point_est = c("mean"),
    prob = 0.5, 
    prob_outer = 0.95
  ) +
  labs(title = "Sliced large fish") +
  xlim(-4,4) +
  scale_y_discrete(
    labels = 
      c(
        "b_Intercept" = "Intercept",
        "b_age" = "Age",
        "b_gendermale" = "Male",
        "b_districtStuengTraeng" = "District \n (Stueng Traeng)",
        "b_districtSiemBouk" = "District \n (Siem Bouk)",
        "b_districtSambour" = "District \n (Sambour)",
        "b_districtPreaekPrasab" = "District \n (Preaek Prasab)",
        "b_districtKracheh" = "District \n (Kracheh)",
        "b_tradition" = "Tradition",
        "b_experience" = "Experience",
        "b_novelty" = "Novelty",
        "b_revenue_fluctuationUnknown" = "Revenue \n (Unknown)",
        "b_revenue_fluctuationSmallincrease" = "Revenue \n (Small increase)",
        "b_revenue_fluctuationLargeincrease" = "Revenue \n (Large increase)",
        "b_revenue_fluctuationSmalldecline" = "Revenue \n (Small decline)"
      )
  )
# pufferfish
plot_interval_pufferfish <- 
  bayesplot::mcmc_intervals(
    food_custom_logistic_02$regression_results[[3]], 
    pars = vars(contains("b_")),
    point_est = c("mean"),
    prob = 0.5, 
    prob_outer = 0.95
  ) +
  labs(title = "Cooked pufferfish") +
  xlim(-4,4) +
  scale_y_discrete(
    labels = 
      c(
        "b_Intercept" = "Intercept",
        "b_age" = "Age",
        "b_gendermale" = "Male",
        "b_districtStuengTraeng" = "District \n (Stueng Traeng)",
        "b_districtSiemBouk" = "District \n (Siem Bouk)",
        "b_districtSambour" = "District \n (Sambour)",
        "b_districtPreaekPrasab" = "District \n (Preaek Prasab)",
        "b_districtKracheh" = "District \n (Kracheh)",
        "b_tradition" = "Tradition",
        "b_experience" = "Experience",
        "b_novelty" = "Novelty",
        "b_revenue_fluctuationUnknown" = "Revenue \n (Unknown)",
        "b_revenue_fluctuationSmallincrease" = "Revenue \n (Small increase)",
        "b_revenue_fluctuationLargeincrease" = "Revenue \n (Large increase)",
        "b_revenue_fluctuationSmalldecline" = "Revenue \n (Small decline)"
      )
  )
# combine the three figures altogether
patchwork::wrap_plots(plot_interval_smallfish, plot_interval_largefish, plot_interval_pufferfish, nrow = 2)
# save
ggsave(
  "plot_intervals.pdf",
  width = 200,
  height = 200,
  units = "mm"
)


# ----- Cambodia.map ---- 
# set spatial data up
# load administrative boundaries
# NOTE
# Before use, download the data from GADM
# (https://gadm.org/)
target_district <- 
  levels(factor(food_custom_long$district))
# province
adm_01 <- 
  sf::st_read("./KHM_adm/KHM_adm1.shp") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# district
adm_02 <- 
  sf::st_read("./KHM_adm/KHM_adm2.shp") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# commune
adm_03 <- 
  sf::st_read("./KHM_adm/KHM_adm3.shp") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# 
# obtain boundary box of the shapefiles above
# To make the box, refer to the following.
# https://stackoverflow.com/questions/67190762/how-to-retrieve-bbox-for-osmdata-from-spatial-feature
# 
# province
adm_01_target <- 
  adm_01 %>% 
  dplyr::filter(
    NAME_1 %in% c("Krâchéh", "Stœng Trêng")
  ) %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# district
adm_02_target <- 
  adm_02 %>% 
  dplyr::filter(
    NAME_1 %in% c("Krâchéh", "Stœng Trêng")
  ) %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# commune
adm_03_target <- 
  adm_03 %>% 
  dplyr::filter(
    NAME_1 %in% c("Krâchéh", "Stœng Trêng") 
  ) %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# for coverage
adm_01_ex_target <- 
  adm_01 %>% 
  dplyr::filter(
    !(NAME_1 %in% c("Krâchéh", "Stœng Trêng"))
  ) %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# 
adm_01_target_union <- 
  adm_01 %>% 
  dplyr::filter(
    NAME_1 %in% c("Krâchéh", "Stœng Trêng")
  ) %>% 
  sf::st_transform(4326) %>% 
  sf::st_bbox()
# 
# select Kratieh province by object
# NOTE
# An English-translated name of Kracheh, "Kracheh", is currently not available.
# Instead, use Khmer name
# Province-level object
# district-level data
adm_02_target <- 
  adm_02 %>% 
  dplyr::filter(
    NAME_1 %in% c("Krâchéh", "Stœng Trêng") & NAME_2 %in% target_district
  ) %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# make some layers such as street and waterway
# To make the layers, we use osmdata() library
# The library has functions to check features and tags.
# Before loading data for the layer, set them while checking them.
# In detail of the features and tags, refer to the following page.
# https://wiki.openstreetmap.org/wiki/Map_features
# features
osmdata::available_features()
# tags
osmdata::available_tags("natural")
osmdata::available_tags("water")
tags_waterway <- available_tags("waterway")
# 
# obtain features' data using osmdata()
# street
# "Street" refers to small roads excluding motorway and major road.
streets <- 
  adm_01_target_union %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "footway", 
      "residential", 
      "service", 
      "track",
      "residential", 
      "living_street",
      "service",
      "unclassified"
    )
  ) %>%
  osmdata::osmdata_sf()
streets
# road
road <- 
  adm_01_target_union %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "motorway", 
      "motorway_junction",
      "motorway_link",
      "primary", 
      "primary_link",
      "secondary", 
      "secondary_link",
      "tertiary",
      "tertiary_link",
      "trunk",
      "trunk_link"
    )
  ) %>%
  osmdata::osmdata_sf()
road
farm <- 
  adm_01_target_union %>%
  opq()%>%
  add_osm_feature(
    key = "landuse"#, 
    # value = c(
    #   "farmland", "farmyard","paddy"
    # )
  ) %>%
  osmdata_sf()
farm
# 
riverbank <- 
  adm_01_target_union %>%
  opq()%>%
  add_osm_feature(
    key = "natural", 
    value = c(
      "water"
    )
  ) %>%
  osmdata_sf()
riverbank
# canal
canal <- 
  adm_01_target_union %>%
  opq()%>%
  add_osm_feature(
    key = "waterway", 
    value = c(
      "canal"
    )
  ) %>%
  osmdata_sf()
canal
#
# Draw a multi-layered map
adm_01_target_union_multilayer_map <- 
  ggplot() +
  geom_sf() +
  # district-level administrative boundaries of Kratie province
  geom_sf(
    data = adm_02_target,
    inherit.aes = FALSE,
    color = "black",
    fill = "grey50",
    size = 0.1,
    linetype = "dotted",
    alpha = 0.4
  ) +
  geom_sf(
    data = riverbank$osm_multipolygons,
    inherit.aes = FALSE,
    color = "steelblue",
    fill = "steelblue",
    size = 0.2,
    alpha = 1.0
  ) +
  # canal
  geom_sf(
    data = canal$osm_lines,
    inherit.aes = FALSE,
    color = "steelblue",
    size = 0.2,
    alpha = 1.0
  ) +
  # # commune-level administrative boundaries of Kratie province
  # geom_sf(
  #   data = adm_03_target,
  #   inherit.aes = FALSE,
  #   color = "black",
  #   fill = NA,
  #   size = 0.5,
  #   linetype = "dotted",
  #   alpha = 1.0
  # ) +
  # district-level administrative boundaries of Kratie province
geom_sf(
  data = adm_02_target,
  inherit.aes = FALSE,
  color = "black",
  fill = NA,
  size = 2.0,
  linetype = "dashed",
  alpha = 1.0
) +
  # paint provinces other than Kratie in white
  geom_sf(
    data = adm_01_ex_target,
    inherit.aes = FALSE,
    color = NA,
    fill = "white",
    size = 20.0,
    alpha = 1.5
  ) +
  # province-level administrative boundaries
  geom_sf(
    data = adm_01_target,
    inherit.aes = FALSE,
    fill = NA,
    color = "black",
    size = 20.0,
    alpha = 1.0
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    caption = "\U00a9 OpenStreetMap contributors"
  ) +
  # geom_text(
  #   data = adm_02_target,
  #   aes(
  #     x = center_x,
  #     y = center_y,
  #     label = NAME_2,
  #     # adjust font size when necessary
  #     size = 1
  #   ),
  #   show.legend = FALSE,
  #   family = "Times"
# )+
geom_text(
  data = adm_02_target,
  aes(
    x = center_x,
    y = center_y,
    label = NAME_2,
    # adjust font size when necessary
    size = 1
  ),
  show.legend = FALSE,
  check_overlap = FALSE,
  family = "sans"
)+
  # fix boundary box
  coord_sf(xlim = c(105.5, 107.0),
           ylim = c(12.0, 14.0),
           expand = TRUE
  ) +
  theme_classic() +
  theme(
    plot.background = element_rect(fill = NA)
  ) +
  ggsn::scalebar(
    x.min = 105.6,
    x.max = 106.0,
    y.min =12.0,
    y.max =12.1,
    dist = 20, 
    dist_unit = "km",
    st.dist = 0.3, 
    st.size = 1, 
    height= 0.3, 
    transform = TRUE
  ) +
  # north arrow
  ggsn::north(
    x.min = 105.6,
    x.max = 105.7,
    y.min =12.05,
    y.max = 12.15,
    symbol = 8,
    scale = 1
  )
# print the map
adm_01_target_union_multilayer_map
# save the map
ggsave(
  "adm_01_kratie_union_multilayer_map.pdf",
  plot = adm_01_target_union_multilayer_map,
  width = 200,
  height = 200,
  units = "mm",
  device = cairo_pdf # important!!
)
# 
