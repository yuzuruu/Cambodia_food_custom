## ---- read.library ----
library(tidyverse)
library(broom.mixed)
library(forcats)
library(patchwork)
library(scales)
library(here)
library(cmdstanr)
library(brms)
library(rstan)

## ---- settings ----
theme_set(theme_minimal(base_size = 11))
# reasons
reason_labs <- 
  c(
    "dislike"  = "Dislike",
    "religion" = "Religion",
    "hardness" = "Hardness/effort",
    "other"    = "Other"
    )

# group color
# Okabe–Ito palette for color universal design
okabeito <- 
  c(
    blue   = "#0072B2",  # Dish
    orange = "#E69F00",  # Buyer
    sky    = "#56B4E9",  # Season
    green  = "#009E73",  # (spare)
    yellow = "#F0E442",  # (modest)
    vermil = "#D55E00",  # Disposition
    purple = "#CC79A7",  # Social
    black  = "#000000"   # Demographic
    )
pal_group <- 
  c(
    "Dish"        = okabeito[["blue"]],
    "Season"      = okabeito[["sky"]],      # secure than green
    "Social"      = okabeito[["purple"]],
    "Buyer"       = okabeito[["orange"]],
    "Demographic" = "#666666",              # dark grey
    "Disposition" = okabeito[["vermil"]],
    "Other"       = "#999999"
    )
# visible name of term 
term_map <- 
  c(
    "dish_typepufferfish"   = "Pufferfish (vs chopped)",
    "dish_typesliced_fish"  = "Sliced (vs chopped)",
    "gendermale"            = "Male",
    "gender2"               = "Male",              # 出力の表記ゆれ対策
    "age_z"                 = "Age (z)",
    "novelty_z"             = "Food neophobia",
    "experience_z"          = "Experience",
    "tradition_z"           = "Traditionalism",
    "seasondry"             = "Dry season (vs wet)",
    "occasionparty"         = "Social occasion (party)",
    "youandmeothers"        = "For others (vs self)"
    )
# names
group_map <- 
  c(
    "Pufferfish (vs chopped)" = "Dish",
    "Sliced (vs chopped)"     = "Dish",
    "Male"                    = "Demographic",
    "Age (z)"                 = "Demographic",
    "Food neophobia"          = "Disposition",
    "Experience"              = "Disposition",
    "Traditionalism"          = "Disposition",
    "Dry season (vs wet)"     = "Season",
    "Social occasion (party)" = "Social",
    "For others (vs self)"    = "Buyer"
    )
# 
## ---- helpers ----
# convert estimates
tidy_to_ratios <- 
  function(
    fit,
    drop_exact  = c("(Intercept)", "sigma"),
    drop_starts = c("Intercept[", "sd(")
    ) {
    broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>% 
      dplyr::filter(!(term %in% drop_exact |
               stringr::str_starts(term, fixed(drop_starts[1])) |
               stringr::str_starts(term, fixed(drop_starts[2])))) %>% 
    dplyr::transmute(
      term_raw   = term,
      # replace displayed names using strings (term) above
      term_label = dplyr::recode(
        # target of replacement
        term, 
        # open target of replacement and object incuding replacement option using !!!
        # in detail, refer to "tidy eval"
        !!!term_map, 
        # If no replacement, leave original contents
        .default = term
        ),
      group      = dplyr::recode(recode(term, !!!term_map, .default = term),
                          !!!group_map, .default = "Other"),
      estimate   = exp(estimate),
      conf.low   = exp(conf.low),
      conf.high  = exp(conf.high)
      )  %>% 
      # transform the term_label as factor
      dplyr::mutate(
        term_label = factor(
          term_label,
          levels = c(
            "Pufferfish (vs chopped)", "Sliced (vs chopped)",
            "Dry season (vs wet)",
            "Social occasion (party)", "For others (vs self)",
            "Male", "Age (z)", "Food neophobia", "Experience", "Traditionalism"
            )
          )
        ) %>% 
      # arrange in order
      dplyr::arrange(term_label)
    }
# helpers for reasons to avoid eating
tidy_reasons <- 
  function(fit_reasons) {
    broom.mixed::tidy(fit_reasons, effects = "fixed", conf.int = TRUE) %>% 
      dplyr::mutate(
      # 例: "mudislike_dish_typepufferfish" → reason_key="dislike", term_raw="dish_typepufferfish"
        reason_key = str_match(term, "^mu([^_]+)_")[,2],
        term_raw   = str_remove(term, "^mu[^_]+_")
        )  %>% 
      dplyr::filter(
        !is.na(reason_key), 
        !term_raw %in% c("Intercept")
        )  %>% 
      dplyr::mutate(
        term_label = dplyr::recode(term_raw, !!!term_map, .default = term_raw),
        group      = dplyr::recode(term_label, !!!group_map, .default = "Other"),
        reason_f   = factor(reason_key, levels = c("dislike","religion","hardness","other"),
                          labels = reason_labs),
        estimate   = exp(estimate),
        conf.low   = exp(conf.low),
        conf.high  = exp(conf.high)
        ) %>% 
      dplyr::mutate(term_label = factor(
        term_label,
        levels = c("Pufferfish (vs chopped)", "Sliced (vs chopped)",
                 "Male", "Age (z)", "Food neophobia", "Experience", "Traditionalism")
        )
        )  %>% 
      dplyr::arrange(reason_f, term_label)
    }
# helper function for drawing
plot_forest_groups <- 
  function(df, title, xlab) {
    ggplot2::ggplot(
      df, 
      aes(
        x = estimate, y = fct_rev(term_label),
        xmin = conf.low, xmax = conf.high, colour = group
        )
      ) +
      geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4) +
      geom_errorbarh(height = 0, linewidth = 0.7) +
      geom_point(size = 1.9) +
      scale_x_log10(labels = label_number(accuracy = 0.01)) +
      scale_colour_manual(values = pal_group, guide = guide_legend(title = NULL, nrow = 1)) +
      labs(title = title, x = xlab, y = NULL) +
      theme(legend.position = "bottom", plot.title.position = "plot")
    }
# helping function for reasons to avoid eating
plot_reasons_forest <- 
  function(df_reason, title = "Reasons for not eating") {
    ggplot2::ggplot(
      df_reason,
      aes(
        x = estimate, y = fct_rev(term_label),
        xmin = conf.low, xmax = conf.high, 
        colour = group
        )
      ) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4) +
    geom_errorbarh(height = 0, linewidth = 0.7) +
    geom_point(size = 1.9) +
    scale_x_log10(labels = label_number(accuracy = 0.01)) +
    scale_colour_manual(values = pal_group, guide = guide_legend(title = NULL, nrow = 1)) +
    labs(title = title,
         x = "Relative odds (exp(β))", y = NULL) +
    theme(legend.position = "bottom", plot.title.position = "plot") +
    facet_wrap(~ reason_f, ncol = 2, scales = "fixed")
}
# 
# summary(readr::read_rds(here::here("analysis","outputs","fit_participation.rds")))
# summary(readr::read_rds(here::here("analysis","outputs","fit_frequency_season_occ.rds")))
# summary(readr::read_rds(here::here("analysis","outputs","fit_reasons_categorical.rds")))

## ---- load.files ----
# participation
df_part   <- 
  tidy_to_ratios(
    readr::read_rds(here::here("analysis","outputs","fit_participation.rds"))
    )
# frequency
df_freq   <- 
  tidy_to_ratios(
    readr::read_rds(here::here("analysis","outputs","fit_frequency_season_occ.rds"))
  )
# WTP
df_wtp    <- 
  tidy_to_ratios(
    readr::read_rds(here::here("analysis","outputs","fit_wtp_lognormal.rds"))
  )
# 
## ---- plots ----
p1 <- plot_forest_groups(df_part, "Who participates?", "Odds ratio")
p2 <- plot_forest_groups(df_freq, "How often?",       "Incidence-rate ratio")
p3 <- plot_forest_groups(df_wtp,  "How much to pay?", "Price ratio")
# participation / frequency /WTP
fig_main <- p1 / p2 / p3 + patchwork::plot_layout(heights = c(1,1,1))
# save
ggsave(
  "fig/fig3_forest_three-panels.pdf", 
  plot = fig_main, 
  width = 140, 
  height = 280, 
  units = "mm"
  )
