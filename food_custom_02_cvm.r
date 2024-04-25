################################################
# food custom survey in Cambodia
# Part II Willing to Pay (WTP)
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
library(sjPlot)

# ----- read.data -----
# original data
food_custom_original <- 
  readxl::read_excel(
    path = "rawfish_eat_Cambodia_2020_2022.xlsx",
    sheet = "food_custom",
    col_names = TRUE
  ) %>% 
  dplyr::select(-q001, -q011) %>% 
  dplyr::mutate(
    id = factor(id), 
    year = factor(year),
    commune = factor(commune),
    district = factor(district),
    province_en = factor(province_en),
    province_kh = factor(province_kh)
  )
# pick up gastronomy behavior
# NOTE
# 1. The object entitled "food_custom_demography" does not work
# without reading the object from the other file. Run the code
# before use.
# (food_custom_01_regression.r line 202)
food_custom_yesno <- 
  food_custom_original %>% 
  dplyr::select(id,q016:q022, q100:q332)
# pick up complete cases using the ids above
food_custom_yes_frequency <- 
  food_custom_demography %>%
  # convert character variables into factor
  dplyr::mutate(
    dplyr::across(
      where(is.character), 
      factor
      )
    ) %>%
  # add the demographic information to the behavior pattern
  dplyr::left_join(
    food_custom_yesno, 
    by = "id"
      ) %>% 
  dplyr::mutate(
    dplyr::across(where(is.character), as.numeric)) %>% 
  # replace 999 into NA
  dplyr::mutate(
    dplyr::across(
      everything(), ~ replace(., . ==  999 , NA)
      )
    ) %>% 
  # pick up those who have eaten any of the 3 recipe
  # q100 chopped small fish
  # q200 sliced large fish
  # q300 pufferfish
  dplyr::filter(q100 == 2 | q200 == 2 | q300 == 2) %>% 
  # select necessary variables
  dplyr::select(id:q022, q111:q116, q211:q216,q311:q316) %>% 
  dplyr::select(-c(q016:q022)) %>% 
  tidyr::pivot_longer(
    cols = c(q111:q316),
    names_to = "title",
    values_to = "frequency"
  ) %>% 
  # convert the variables' names into items
  dplyr::mutate(
    recipe = dplyr::case_when(
      stringr::str_starts(title, "q1") ~ "chopped_fish",
      stringr::str_starts(title, "q2") ~ "sliced_fish",
      stringr::str_starts(title, "q3") ~ "pufferfish",
      TRUE  ~ "NA"
    ),
    season = dplyr::case_when(
      stringr::str_detect(title, "11$|14$") ~ "past_one_year",
      stringr::str_detect(title, "12$|15$") ~ "dry_season",
      stringr::str_detect(title, "13$|16$") ~ "rainy_season",
      TRUE  ~ "NA"
    ),
    occasion = dplyr::case_when(
      stringr::str_detect(title, "11$|12$|13$") ~ "daily_meal",
      stringr::str_detect(title, "14$|15$|16$") ~ "banquet",
      TRUE  ~ "NA"
      )
    ) %>% 
  dplyr::select(-title) %>% 
  dplyr::filter(season != "past_one_year") %>% 
  dplyr::mutate(dplyr::across(where(is.character), factor))
# 
# ----- yes.frequency -----
# figure
# descriptive statistics
# food_custom_yes_frequency_summary <- 
#   food_custom_yes_frequency %>% 
#   dplyr::group_by(recipe, occasion, season) %>% 
#   dplyr::summarise(
#     N = sum(!is.na(frequency)),
#     Min. = min(frequency, na.rm = TRUE),
#     Mean = mean(frequency, na.rm = TRUE),
#     Median = median(frequency, na.rm = TRUE),
#     Max. = max(frequency, na.rm = TRUE),
#     SD = sd(frequency, na.rm = TRUE),
#     SE = sd(frequency, na.rm = TRUE)/sqrt(sum(!is.na(frequency))),
#   )
# food_custom_yes_frequency_summary


# analysis
# NOTE
# 1. This process requires long computation period.
# Comment out when not in use.
#
# 1. normal linear regression with zero-inflated negative binomial distribution
# In detail, refer to the following page.
# https://discourse.mc-stan.org/t/zero-inflated-negative-binomial-model-with-random-effects/8935
# food_custom_frequency_01 <-
#   brms::brm(
#     frequency ~ occasion + recipe + season,
#     data = food_custom_yes_frequency,
#     family = zero_inflated_negbinomial(),
#     warmup = 50000,
#     iter = 200000,
#     chains = 4,
#     cores= 4,
#     seed = 123,
#     refresh = 100,
#     backend = "cmdstanr"
#   )
# # print summary of the Bayesian regression results above
# summary(food_custom_frequency_01)
food_custom_frequency_02 <- readr::read_rds("food_custom_frequency_02.rds")
summary(food_custom_frequency_02$regression_results[[1]])
summary(food_custom_frequency_02$regression_results[[2]])
summary(food_custom_frequency_02$regression_results[[3]])


# # save the results
# readr::write_rds(food_custom_frequency_01, "food_custom_frequency_01.rds")
# 2. typical fixed model by recipe to understand characteristics derived from he recipe.
# food_custom_frequency_02 <-
#   food_custom_yes_frequency %>%
#   group_by(recipe) %>%
#   nest() %>%
#   dplyr::mutate(
#     regression_results = purrr::map(
#       data,
#       ~
#         brms::brm(
#           frequency ~ occasion + season,
#           data = .,
#           family = zero_inflated_negbinomial(),
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
# print summary of the logistic regression by recipe
# summary(food_custom_frequency_02$regression_results[[1]])
# summary(food_custom_frequency_02$regression_results[[2]])
# summary(food_custom_frequency_02$regression_results[[3]])
# save the results
# readr::write_rds(food_custom_frequency_02, "food_custom_frequency_02.rds")
# summary(readr::read_rds(food_custom_frequency_01.rds))
# # save the results
# readr::write_rds(food_custom_frequency_02, "food_custom_frequency_02.rds")

# food_custom_frequency_01 <- 
#   readr::read_rds("food_custom_frequency_01.rds")
# food_custom_frequency_02 <- 
#   readr::read_rds("food_custom_frequency_02.rds")
# # save the summary table using MSExcel format
# # model 1
# gtsummary::tbl_regression(food_custom_frequency_01, intercept = TRUE) %>% 
#   gtsummary::as_tibble() %>% 
#   writexl::write_xlsx(., "food_custom_frequency_01_summary.xlsx")
# # model 2 (chopped small fish)
# gtsummary::tbl_regression(food_custom_frequency_02$regression_results[[1]], intercept = TRUE) %>% 
#   gtsummary::as_tibble() %>% 
#   writexl::write_xlsx(., "food_custom_frequency_02_01_summary.xlsx")
# # model 2 (sliced large fish)
# gtsummary::tbl_regression(food_custom_frequency_02$regression_results[[2]], intercept = TRUE) %>% 
#   gtsummary::as_tibble() %>% 
#   writexl::write_xlsx(., "food_custom_frequency_02_02_summary.xlsx")
# # model 2 (pufferfish)
# gtsummary::tbl_regression(food_custom_frequency_02$regression_results[[3]], intercept = TRUE) %>% 
#   gtsummary::as_tibble() %>% 
#   writexl::write_xlsx(., "food_custom_frequency_02_03_summary.xlsx")
# # 
# # 3. with random model
# # We assume that the recipe affecting the frequency might differ by occasion.
# food_custom_frequency_03 <-
#   brms::brm(
#     frequency ~ occasion + recipe + season + (recipe|occasion),
#     data = food_custom_yes_frequency,
#     family = zero_inflated_negbinomial(),
#     warmup = 50000,
#     iter = 200000,
#     chains = 4,
#     cores= 4,
#     seed = 123,
#     refresh = 100,
#     backend = "cmdstanr"
#   )
# # print summary of the Bayesian regression results above
# summary(food_custom_frequency_03)
# # save the results
# # readr::write_rds(food_custom_frequency_03, "food_custom_frequency_03.rds")
# 
# ----- ncp -----
# The contingent values by those who replied "yes" to any of the three recipe.
# make a data set
# NOTE
# 1. How to calculate NCP, Nature's contribution to people
# # According to our interview in March 2024, WTP per dish are as follows:
# 1. Chopped fish (Trei riel): 5.00USD/dish 
# 2. Large fish (Sashimi): 5.00USD/dish
# 3. Pufferfish (Deep fried): 1.00USD/dish
# # We measured cooked fish before / after fishing and obtained following results.
# # g per dish (before cooking = weight as cooking material)
# 1. (70.7+62.1+60.1+65.2+48.5+56.1+81.2)=443.9g for 1 dish
# 2. 2,035g/fish for 6 dishes 
# (339.1667g per dish)
# (Seemingly for 4 dishes. Then, 2035/4=508.75g per dish)
# 3. (153.4+84.3+79.1+115.1)=431.9g for 4 dishes
# (107.975g per dish)
# # dish per g
# # IMPORTANT
# ### 1. 1/443.9 = 0.0023
# ### 2. 1/339.1667 = 0.0029
# ### 3. 1/107.975 = 0.0093
# # # sharable N.of persons per dish (person per dish)
# # IMPORTANT
# ### 1. less than 3 persons per dish
# ### 2. less than 10 persons per dish 
# ### 3. 4 persons per dish (1 fish per person)
# # # g per person
# # 1. 443.9g/3pax = 147.97g/pax
# # 2. 2,035g/10pax = 203g/pax
# # 3. 431.9g/4pax = 107.975g/pax
# # Therefore, WTP/g((WTP/dish)*(dish/g))
# By multiplying the dish/gram, we are able to obtain WTP per gram.
# 1. 5*0.0023=0.0115USD/g
# 2. 5*0.0029=0.0145USD/g
# 3. 1*0.0093=0.0093USD/g
# # Hence, WTP/person ((WTP/dish)*(dish/person))
# By multiplying the dish/person, we are able to obtain WTP per gram.
# 1. 5*(1/6)=0.833
# 2. 5*(1/10)=0.5
# 3. 1*(1/1)=1
food_custom_yes_price <-
  # pass the data
  # NOTE
  # We make the data in the file entitled "food_custom_01_regression.r".
  # Read line 206- of the file before running.
  food_custom_demography %>% 
  # transform value type
  dplyr::mutate(dplyr::across(where(is.character), factor)) %>% 
  dplyr::left_join(
    food_custom_yesno, 
    by = "id"
  ) %>% 
  # transform value type
  # After merging, somehow the numeric variable are forced to be transformed 
  # into character. We need to convert it as before.
  dplyr::mutate(across(where(is.character), as.numeric)) %>% 
  # Replace responses indicating UNKNOWN (999) into NA
  dplyr::mutate(
    across(everything(), 
           ~ replace(., . ==  999 , NA)
           )
    ) %>% 
  # select those who have eaten any of the three dishes
  dplyr::filter(q100 == 2 | q200 == 2 | q300 == 2) %>% 
  dplyr::select(id:q022, q117:q118, q217:q218,q317:q318) %>% 
  dplyr::select(-c(q016:q022)) %>% 
  tidyr::pivot_longer(
    cols = c(q117:q318),
    names_to = "title",
    values_to = "price"
  ) %>% 
  dplyr::mutate(
    recipe = dplyr::case_when(
      stringr::str_starts(title, "q1") ~ "chopped_fish",
      stringr::str_starts(title, "q2") ~ "sliced_fish",
      stringr::str_starts(title, "q3") ~ "pufferfish",
      TRUE  ~ "NA"
    ),
    youandme = dplyr::case_when(
      stringr::str_detect(title, "17$") ~ "myself",
      stringr::str_detect(title, "18$") ~ "others",
      TRUE  ~ "NA"
    )
  ) %>% 
  dplyr::select(-title) %>% 
  dplyr::mutate(across(where(is.character), factor)) %>% 
  dplyr::mutate(
    # add 0.5 for logarithmic transforamtion. 
    # In detail, refer to the following paper.
    # http://cse.naro.affrc.go.jp/yamamura/Images/Transformation.PDF
    price_0.5 = price + 0.5,
    # calculate price (WTP) per/g
    price_per_kilogram = dplyr::case_when(
      recipe == "chopped_fish" ~ 1000*price/443.9,
      recipe == "sliced_fish" ~ 1000*price/508.75,
      recipe == "pufferfish" ~ 1000*price/107.975,
      TRUE ~ 99999
      ),
    # calculate price (WTP)/person
    price_per_person = dplyr::case_when(
      recipe == "chopped_fish" ~ price/6,
      recipe == "sliced_fish" ~ price/10,
      recipe == "pufferfish" ~ price/1,
      TRUE ~ 99999
      )
    )
# make descriptive statistics tables
# price
wtp_summary <- 
  food_custom_yes_price %>% 
  group_by(recipe, district, gender, youandme) %>% 
  dplyr::summarise(
    N = sum(!is.na(price)),
    Min = min(price, na.rm = TRUE),
    Mean = mean(price, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    SD = sd(price, na.rm = TRUE),
    SE = sd(price, na.rm = TRUE)/sqrt(n())
    )
readr::write_excel_csv(wtp_summary, "wtp_summary.csv")
# price per kilogram
wtp_per_kilogram_summary <- 
  food_custom_yes_price %>% 
  group_by(recipe, district, gender, youandme) %>% 
  dplyr::summarise(
    N = sum(!is.na(price_per_kilogram)),
    Min = min(price_per_kilogram, na.rm = TRUE),
    Mean = mean(price_per_kilogram, na.rm = TRUE),
    Median = median(price_per_kilogram, na.rm = TRUE),
    Max = max(price_per_kilogram, na.rm = TRUE),
    SD = sd(price_per_kilogram, na.rm = TRUE),
    SE = sd(price_per_kilogram, na.rm = TRUE)/sqrt(n())
  )
readr::write_excel_csv(
  wtp_per_kilogram_summary, 
  "wtp_summary_per_kilogram.csv"
  )
# figure
# density plot by district, gender, and position
# per kilogram
wtp_per_kilogram_density <- 
  food_custom_yes_price %>% 
  # dplyr::filter(price > 0) %>% 
  ggplot2::ggplot(
    aes(
      x = price_per_kilogram,
      color = recipe,
      fill = recipe
    ) 
  ) +
  geom_density(alpha=0.2) +
  scale_color_okabeito() +
  scale_fill_okabeito() +
  labs(
    x = "Price per weight (Unit: USD/kg)",
    y = "Density"
  ) +
  # xlim(0,7) +
  theme_classic() + 
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  ) + 
  facet_wrap(
    ~ district + gender + youandme,
    scales = "free_y",
    ncol = 4
  )
wtp_per_kilogram_density
ggsave(
  "wtp_per_kilogram_density.pdf",
  plot = wtp_per_kilogram_density,
  width = 210, 
  height = 280,
  units = "mm"
)
# bare price
wtp_density <- 
  food_custom_yes_price %>% 
  # dplyr::filter(price > 0) %>% 
  ggplot2::ggplot(
    aes(
      x = price,
      color = recipe,
      fill = recipe
      ) 
    ) +
    geom_density(alpha=0.2) +
  scale_color_okabeito() +
  scale_fill_okabeito() +
  labs(
    x = "Price per weight (Unit: USD/kg)",
    y = "Density"
  ) +
  theme_classic() + 
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  ) + 
  facet_wrap(
    ~ district + gender + youandme,
    scales = "free_y",
    ncol = 4
    )
wtp_density
ggsave(
  "wtp_density.pdf",
  plot = wtp_density,
  width = 210, 
  height = 280,
  units = "mm"
)
# 
# ----- bayesian.anova.wtp -----
# set prior
prior <-
  c(
    set_prior("student_t(3,0,10)", class = "b"),
    set_prior("student_t(3,0,10)", class = "Intercept")
    )
# run
# price per kilogram
bayesian_anova_wtp_per_kilogram <-
  brms::brm(
    price_per_kilogram ~ district + gender + recipe + youandme,
    data = food_custom_yes_price,
    # family = gaussian(link = "log"),
    family = student(link = "identity"),
    prior = prior,
    warmup = 50000,
    iter = 200000,
    chains = 4,
    cores= 4,
    seed = 123,
    refresh = 100,
    backend = "cmdstanr"
  )
# print summary of the Bayesian regression results above
summary(bayesian_anova_wtp_per_kilogram)
loo(bayesian_anova_wtp_per_kilogram)
readr::write_rds(
  bayesian_anova_wtp_per_kilogram, 
  "bayesian_anova_wtp_per_kilogram.rds"
  )
# bare price
bayesian_anova_wtp <-
  brms::brm(
    price ~ district + gender + recipe + youandme,
    data = food_custom_yes_price,
    # family = gaussian(link = "log"),
    family = student(link = "identity"),
    prior = prior,
    warmup = 50000,
    iter = 200000,
    chains = 4,
    cores= 4,
    seed = 123,
    refresh = 100,
    backend = "cmdstanr"
  )
# print summary of the Bayesian regression results above
summary(bayesian_anova_wtp)
loo(bayesian_anova_wtp)
readr::write_rds(
  bayesian_anova_wtp, 
  "bayesian_anova_wtp.rds"
  )
bayesian_anova_wtp <- readr::read_rds("bayesian_anova_wtp.rds")
bayesian_anova_wtp_per_kilogram <- readr::read_rds("bayesian_anova_wtp_per_kilogram.rds")


gtsummary::tbl_regression(bayesian_anova_wtp, intercept = TRUE) %>%
  gtsummary::as_tibble() %>%
  writexl::write_xlsx(., "bayesian_anova_wtp.xlsx")
gtsummary::tbl_regression(bayesian_anova_wtp_per_kilogram, intercept = TRUE) %>%
  gtsummary::as_tibble() %>%
  writexl::write_xlsx(., "bayesian_anova_wtp_per_kilogram.xlsx")



# plot the ANOVA results
# bare price
plot_bayesian_anova_wtp <- 
  bayesplot::mcmc_intervals(
    bayesian_anova_wtp, 
    pars = vars(contains("b_")),
    point_est = c("mean"),
    prob = 0.5, 
    prob_outer = 0.95
  ) +
  labs(
    title = "Willing to Pay (Unit: USD)",
    x = expression(mu),
    y = "Factors affecting the WTP"
  ) +
  xlim(-2,12) +
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
        "b_recipepufferfish" = "Pufferfish",
        "b_recipesliced_fish" = "Sliced fish",
        "b_youandmeothers" = "for Others"
      )
  )
# save the results
ggsave(
  "plot_bayesian_anova_wtp.pdf",
  plot = plot_bayesian_anova_wtp,
  width = 150,
  height = 150,
  units = "mm"
)
# WTP per kilogram
plot_bayesian_anova_wtp_per_kilogram <- 
  bayesplot::mcmc_intervals(
    bayesian_anova_wtp_per_kilogram, 
    pars = vars(contains("b_")),
    point_est = c("mean"),
    prob = 0.5, 
    prob_outer = 0.95
  ) +
  labs(
    title = "Willing to Pay (Unit: USD/kg)",
    x = expression(mu),
    y = "Factors affecting the WTP"
    ) +
  xlim(-2,12) +
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
        "b_recipepufferfish" = "Pufferfish",
        "b_recipesliced_fish" = "Sliced fish",
        "b_youandmeothers" = "for Others"
      )
  )
# save the results
ggsave(
  "plot_bayesian_anova_wtp_per_kilogram.pdf",
  plot = plot_bayesian_anova_wtp_per_kilogram,
  width = 150,
  height = 150,
  units = "mm"
)
# 
# ----- no.reason ----
food_custom_yesno <- 
  food_custom_original %>% 
  dplyr::select(id,q016:q022, q100:q332)
# pick up complete cases using the ids above
food_custom_no_reason_risk <- 
  food_custom_demography %>%
  # convert character variables into factor
  dplyr::mutate(
    dplyr::across(
      where(is.character), 
      factor
    )
  ) %>%
  # add the demographic information to the behavior pattern
  dplyr::left_join(
    food_custom_yesno, 
    by = "id"
  ) %>% 
  dplyr::mutate(
    dplyr::across(where(is.character), as.numeric)) %>% 
  # replace 999 into NA
  dplyr::mutate(
    dplyr::across(
      everything(), ~ replace(., . ==  999 , NA)
    )
  ) %>% 
  # pick up those who have eaten any of the 3 recipe
  # q100 chopped small fish
  # q200 sliced large fish
  # q300 pufferfish
  dplyr::filter(q100 == 1 | q200 == 1 | q300 == 1) %>% 
  # select necessary variables
  dplyr::select(id:q022, q131:q132, q231:q232,q331:q332) %>% 
  dplyr::select(-c(q016:q022)) %>% 
  tidyr::pivot_longer(
    cols = c(q131:q332),
    names_to = "title",
    values_to = "answer"
  ) %>% 
  # convert the variables' names into items
  dplyr::mutate(
    recipe = dplyr::case_when(
      stringr::str_starts(title, "q1") ~ "chopped_fish",
      stringr::str_starts(title, "q2") ~ "sliced_fish",
      stringr::str_starts(title, "q3") ~ "pufferfish",
      TRUE  ~ "NA"
    ),
    reason_risk = dplyr::case_when(
      stringr::str_detect(title, "31$") ~ "reason",
      stringr::str_detect(title, "32$") ~ "risk",
      TRUE  ~ "NA"
    )
  ) %>% 
  dplyr::select(-title) %>% 
  dplyr::mutate(dplyr::across(where(is.character), factor)) %>% 
  tidyr::drop_na(answer)
# risk
food_custom_no_reason <- 
  food_custom_no_reason_risk %>% 
  dplyr::filter(reason_risk == "reason") %>% 
  dplyr::mutate(
    answer = dplyr::case_when(
      answer == "1" ~ "harmful",
      answer == "2" ~ "taste",
      answer == "3" ~ "allergy",
      answer == "4" ~ "religion",
      answer == "5" ~ "texture",
      answer == "6" ~ "experience",
      answer == "7" ~ "others",
      TRUE  ~ "NA"
    ),
    answer = factor(answer, levels = c("allergy","experience","harmful","religion","taste","texture","others"))
  ) 
# count the N. of respondent by recipe and answer
food_custom_no_reason_summary <- 
  food_custom_no_reason %>% 
  na.omit() %>% 
  dplyr::group_by(recipe, answer) %>% 
  summarise(
    N = length(answer)
  ) %>% 
  ungroup() 
# If necessary, use the summary table below.
# > food_custom_no_reason_summary
# # A tibble: 3 × 8
# recipe       allergy experience harmful religion taste texture others
# <fct>          <int>      <int>   <int>    <int> <int>   <int>  <int>
# 1 chopped_fish    5          6      65       10   145       5     27
# 2 pufferfish      1          1     306       18    99       1     73
# 3 sliced_fish     4          5      94       12   187      12     60
# 

# Bayesian regression with negative binomial distribution
# for count data
# NOTE

# This process spends long computation period.
# Comment out when not in use.
# 1. population effect only
food_custom_reason_01 <-
  brms::brm(
    N ~ recipe + answer,
    data = food_custom_no_reason_summary,
    family = negbinomial(),
    warmup = 50000,
    iter = 200000,
    chains = 4,
    cores= 4,
    seed = 123,
    refresh = 100,
    backend = "cmdstanr"
  )
# print summary of the Bayesian regression results above
food_custom_reason_01 <- readr::read_rds("food_custom_reason_01.rds")
summary(food_custom_reason_01)
# save the results
readr::write_rds(food_custom_reason_01 , "food_custom_reason_01.rds")
food_custom_reason_02 <- readr::read_rds("food_custom_reason_02.rds")
# model 2
food_custom_reason_02 <-
  food_custom_no_reason_summary %>%
  group_by(recipe) %>%
  nest() %>%
  dplyr::mutate(
    regression_results = purrr::map(
      data,
      ~
        brms::brm(
          N ~ answer,
          data = .,
          family = negbinomial(),
          warmup = 50000,
          iter = 200000,
          chains = 4,
          cores= 4,
          seed = 123,
          refresh = 100,
          backend = "cmdstanr"
        )
    )
  )
# save the results
readr::write_rds(food_custom_reason_02, "food_custom_reason_02.rds")
# print summary of the logistic regression by recipe
summary(food_custom_reason_02$regression_results[[1]])
summary(food_custom_reason_02$regression_results[[2]])
summary(food_custom_reason_02$regression_results[[3]])

# summary table
food_custom_reason_01 <- 
  readr::read_rds("food_custom_reason_01.rds")
food_custom_reason_02 <- 
  readr::read_rds("food_custom_reason_02.rds")
# save the summary table using MSExcel format
# model 1
gtsummary::tbl_regression(food_custom_reason_01, intercept = TRUE) %>% 
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "food_custom_reason_01_summary.xlsx")
# mode 2
# model 2 (chopped small fish)
gtsummary::tbl_regression(food_custom_reason_02$regression_results[[1]], intercept = TRUE) %>% 
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "food_custom_reason_02_01_summary.xlsx")
# model 2 (sliced large fish)
gtsummary::tbl_regression(food_custom_reason_02$regression_results[[2]], intercept = TRUE) %>% 
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "food_custom_reason_02_02_summary.xlsx")
# model 2 (pufferfish)
gtsummary::tbl_regression(food_custom_reason_02$regression_results[[3]], intercept = TRUE) %>% 
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "food_custom_reason_02_03_summary.xlsx")
# 
# ----- market.fish.price -----
# # read data
# fish_price <-
#   readxl::read_excel(
#     path = "rawfish_eat_Cambodia_2020_2022.xlsx",
#     sheet = "fish_price",
#     col_names = TRUE
#   ) %>%
#   dplyr::mutate(
#     id = factor(id),
#     shop = factor(shop),
#     species = factor(species),
#     size = factor(size)
#   )
# # summary table
# fish_price_summary <-
#   fish_price %>%
#   dplyr::group_by(species) %>%
#   dplyr::summarise(
#     N = n(),
#     Min. = min(price_usd),
#     Mean = mean(price_usd),
#     Median = median(price_usd),
#     Max. = max(price_usd),
#     SD = sd(price_usd),
#     SE = sd(price_usd)/(sqrt(n()))
#   ) %>%
#   dplyr::arrange(desc(N)) %>%
#   dplyr::mutate(
#     proportional_abundance = (N/length(levels(fish_price$species)))^2
#   ) %>%
#   ungroup()
# # Simpson's D = 0.7470273
# 1-sum(fish_price_summary$proportional_abundance)
# # save the results
# # readr::write_excel_csv(fish_price_summary, "fish_price_summary.csv")
# 
# 
# fb_species <- 
#   fb_tbl("species") %>% 
#   dplyr::mutate(sci_name = paste(Genus, Species)) %>% 
#   dplyr::mutate(sci_name) 
# 
# 
# target_species <- fish_price_summary$species
# 
# setdiff(target_species, hoge$sci_name)
# 
# 
# fb_target_species <-
#   fb_species %>%
#   filter(sci_name %in% target_species) %>%
#   select(sci_name, FBname, Length, Weight)
# 
# readr::write_rds(fb_target_species, "fb_target_species.rds")
# 
# 
# 
# food_custom_yes_price_target_species <- 
#   food_custom_yes_price %>% 
#   dplyr::select(recipe, price) %>% 
#   tidyr::drop_na(price) %>% 
#   dplyr::filter(price>0)
# 
# 
# # fb_target_species_price <- 
# #   fb_target_species %>% 
# #   dplyr::left_join(fish_price_summary, by = c("sci_name" = "species"))
# 
# 
# fish_price_henicorhynchus <- fish_price %>% dplyr::filter(species == "Henicorhynchus siamensis")
# fish_price_labeo <- fish_price %>% dplyr::filter(species == "Labeo chrysophekadion")
# 
# 
# price_density_combined <- 
#   fish_price %>% 
#   ggplot(aes(x = log(price_usd+0.5))) + 
#   geom_density(size = 1.5) + 
#   scale_color_viridis(discrete = TRUE, option = "plasma") +
#   scale_fill_viridis(discrete = TRUE, option = "plasma") +
#   labs(x = "Price (Unit: USD, log Trans.)", y = "Density")+
#   geom_density(data = food_custom_yes_price_target_species, aes(x = log(price+0.5), color = recipe, fill = recipe, alpha = 0.1)) +
#   geom_segment(data = fish_price_henicorhynchus[1,], aes(x=log(price_usd+0.5),y=0,xend=log(price_usd+0.5),yend=0.43), color = "blue", linetype = "dotted") +
#   geom_segment(data = fish_price_henicorhynchus[2,], aes(x=log(price_usd+0.5),y=0,xend=log(price_usd+0.5),yend=0.54), color = "blue", linetype = "dotted") +
#   geom_segment(data = fish_price_henicorhynchus[3,], aes(x=log(price_usd+0.5),y=0,xend=log(price_usd+0.5),yend=0.68), color = "blue", linetype = "dotted") +
#   geom_segment(data = fish_price_labeo[1,], aes(x=log(price_usd+0.5),y=0,xend=log(price_usd+0.5),yend=0.25), color = "purple",linetype = "dashed") +
#   geom_segment(data = fish_price_labeo[2,], aes(x=log(price_usd+0.5),y=0,xend=log(price_usd+0.5),yend=0.45), color = "purple", linetype = "dashed") +
#   geom_segment(data = fish_price_labeo[3,], aes(x=log(price_usd+0.5),y=0,xend=log(price_usd+0.5),yend=0.15), color = "purple", linetype = "dashed") +
#   geom_segment(data = fish_price_labeo[4,], aes(x=log(price_usd+0.5),y=0,xend=log(price_usd+0.5),yend=0.16), color = "purple", linetype = "dashed") +
#   geom_segment(data = fish_price_labeo[5,], aes(x=log(price_usd+0.5),y=0,xend=log(price_usd+0.5),yend=0.45), color = "purple", linetype = "dashed") +
#   geom_segment(data = fish_price_labeo[6,], aes(x=log(price_usd+0.5),y=0,xend=log(price_usd+0.5),yend=0.32), color = "purple", linetype = "dashed") +
#   theme_classic() +
#   theme(
#     legend.position = "none"
#   )
# ggsave(
#   "price_density_combined.pdf",
#   plot = price_density_combined,
#   width = 200,
#   height = 200,
#   units = "mm"
# )
# 
# 
# fb_target_species_price %>% readr::write_excel_csv("fb_target_species_price.csv")
# 
# 
# fb_target_species_price$sci_name
# 
