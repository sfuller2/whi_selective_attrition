##################################################
## Create super learner for predicting censoring
## Lung/LTFU
## Project: WHI
## Author: Sophia Fuller
## Date: 08/09/2023
## Updated: 09/16/2025
##################################################

## libraries
library(sl3)
library(here)
library(tidyverse)
library(earth)
library(nnls)
library(Rsolnp)
library(ranger)
library(polspline)
library(purrr)
library(future)
library(rstatix)
library(patchwork)


## data
data <- readRDS(file =  here::here("data/main_analysis/clean_data/cohort_allsites_long_n31815_20250916.rds"))

source(here("programs/R/sl_weight_calc.R"))

## take each dataset separately
lung <- data %>% filter(cancer == "Lung")
start_time <- proc.time()
for (i in 0:15) {
  print(paste("wave", i, sep = ": "))
  dat <- lung %>%
    filter((is.na(first_censor_wave) | wave_yr <= first_censor_wave) & wave_yr == i)

  # create condition to advance loop
  if(nrow(dat %>% filter(censor_ltfu == 1)) < 1) {
    print(paste("skipping wave", i, sep = ": "))
    next
  }

  # get denominators for stabilized weights
  #denom_preds <- long_sl_denom(dat, i, "ltfu", "lung")

  # calculate the numerators
  num_preds <- long_sl_num(dat, i, "ltfu", "lung")

  today <- str_remove_all(Sys.Date(), "-")

  #denom_file <- paste0("output/weights/lung_ltfu/", today, "_denom_wv_", paste0(i, ".rds"))
  num_file <- paste0("output/weights/lung_ltfu/", today, "_num_wv_", paste0(i, ".rds"))

  #saveRDS(denom_preds[1:3], file = here(denom_file))
  saveRDS(num_preds[1:3], file = here(num_file))


  # get the actual SL itself (so that we can look at variable importance)

  #filename1 <- paste0("output/r_objects/lung_ltfu/", today, "_sl_fit_denom_wv_", paste0(i, ".rds"))
  filename2 <- paste0("output/r_objects/lung_ltfu/", today, "_sl_fit_num_wv_", paste0(i, ".rds"))

  # save those
  #saveRDS(denom_preds[4], file = here(filename1))
  saveRDS(num_preds[4], file = here(filename2))

  #rm(denom_preds)
  rm(num_preds)
}

runtime_total <- proc.time() - start_time
print(runtime_total)
