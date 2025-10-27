# ###########################################################
# For interpretation of food custom survey results (New!)
# by Yuzuru Utsunomiya, Ph. D.
# First: 25th. October 2025
# Revise:                   
# 
# ###########################################################
# 
# Note
# This file 
## ---- read.library ----
library(tidyverse)
library(cmdstanr)
library(brms)

## ---- results ----
# participation
# whether a person eat the target dishes or not
# main effect
participation <- 
  readr::read_rds("./analysis/outputs/fit_participation.rds")
summary(participation)
# with interaction
participation_int <- 
  readr::read_rds("./analysis/outputs/fit_participation_interactions.rds")
summary(participation_int)
# 
# Frequency
# how often those who eat the target dishes per season 
# main effect
frequency <- 
  readr::read_rds("./analysis/outputs/fit_frequency_season_occ.rds")
summary(frequency)
# interactions
frequency_int <- 
  readr::read_rds("./analysis/outputs/fit_frequency_interactions.rds")
summary(frequency_int)
# 
# Nonreason
# Reason not to eat the dishes
# main effect
nonreason <- 
  readr::read_rds("./analysis/outputs/fit_reasons_categorical.rds")
summary(nonreason)
# interactions
nonreason_int <- 
  readr::read_rds("./analysis/outputs/fit_reasons_categorical_interactions.rds")
summary(nonreason_int)
# 
# WTP
# main effect
wtp <- 
  readr::read_rds("./analysis/outputs/fit_wtp_lognormal.rds")
summary(wtp)
# interaction
wtp_int <- 
  readr::read_rds("./analysis/outputs/fit_wtp_interactions_lognormal.rds")
summary(wtp_int)


