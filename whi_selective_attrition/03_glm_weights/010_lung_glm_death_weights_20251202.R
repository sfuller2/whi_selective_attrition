################################################################################
## Create censoring due death weights (Lung)
## Sophia Fuller
## Date 12/02/2025
################################################################################


# setup -------------------------------------------------------------------
## libraries
library(tidyverse)
library(here)
library(glmnet)

#################################
## include on every R script
library(RhpcBLASctl)
blas_set_num_threads(1)
omp_set_num_threads(1)
#################################


## data
data <- readRDS(file =  here::here("data/main_analysis/clean_data/cohort_allsites_long_n31815_20250916.rds"))

data <- data %>%
  filter(cancer == "Lung")

covariates_base <- c("ccstat", "ageatindex", #"last_physfun",
                     # study specific
                     "ctflag", 'hrtarm', 'dmarm', 'cadarm', 'bmdflag',
                     #demographics
                     'racenih_cat', 'ethnicnih_cat', 'educ_impute2',
                     'income_impute', 'region_cat', 'marital_impute',
                     'anyins_impute', 'medicare_impute',
                     'medicaid_impute',
                     # #medical history
                     "mi_preindex", "angina_preindex", "diabtx_preindex",# "parkinsons_preindex", "hyst_preindex",
                     #repro history
                     'menarche_impute', 'gravid_impute',
                     'agefbir_impute', 'booph_impute',
                     'brstfdmo_impute',
                     'meno_impute',
                     #family medical history
                     "mirel_impute",
                     'cancmrel_impute',
                     'cancfrel_impute',
                     "strkrel_impute",
                     "bkbonrel_impute",
                     "diabrel_impute",
                     #lifestyle
                     'smoking_impute',
                     'alcswk_impute',
                     'socsupp_impute',

                     # clinical characteristics
                     'syst_impute', 'dias_impute', 'bmi_impute',

                     # cancer characteristics
                     'stage_cat', 'surgery_cat', 'chemo_cat', 'radiation_cat'#,
                     #'size_bin', 'grading_cat', 'lymph_involvement'
                     #'stage_cat:surgery_cat', 'stage_cat:chemo_cat', 'stage_cat:radiation_cat'
)


data2 <- data  %>%
  mutate(marital_impute = as.character(marital_impute),
         marital_impute = ifelse(marital_impute == "Currently partnered", 1, 0),
         medicaid_impute = ifelse(medicaid_impute == "No", 0,1),
         mirel_impute = ifelse(mirel_impute == "No", 0,1),
         strkrel_impute = ifelse(strkrel_impute == "No", 0,1),
         diab_impute = ifelse(diabrel_impute == "No", 0,1))

## model setup
outcome_death <- c("censor_death")

set.seed(1715)
##--------------- LUNG ------------------------


# List to store models
glm_models <- list()

# Loop over waves
for (w in 0:14) {

  print(paste("Wave:", w))
  # Filter data to include only participants not censored before wave w
  df_wave <- data2 %>%
    filter((is.na(first_censor_wave) | wave_yr <= first_censor_wave) & wave_yr == w)

  # check if wave has censored and uncensored individuals. if not, skip
  if(nrow(df_wave %>% filter(censor_death == 0)) == 0 | nrow(df_wave %>% filter(censor_death == 1)) == 0) {
    print(paste0("Only censored or uncensored at wave: ", w))
    next
  }

  mod_death <- as.formula(paste(outcome_death, paste0(covariates_base, collapse = "+"), sep = "~"))


  # x = df_wave[,covariates_base] %>% data.matrix()
  # y = df_wave$censor_death

  # Fit GLM for wave w
  # model <- glm(mod_death,
  #              data = df_wave,
  #              family = binomial())



  model = glm(formula = mod_death, family = "binomial", data = df_wave)
  # Store model
  glm_models[[paste0("wave_", w)]] <- model
}

today <- str_remove_all(Sys.Date(), "-")

saveRDS(glm_models, here(paste0("output/glm_weights/lung_death_weights_denom_", today, ".rds")))


# numerator -stabilized ---------------------------------------------------


mod_stable_death <- as.formula(paste(outcome_death, paste0(c("ccstat",
                                                             "ctflag",
                                                             #demographics
                                                             "ageatindex", 'racenih_cat', 'ethnicnih_cat', 'educ_impute2',
                                                             'income_impute', 'income_impute_flag', 'region_cat', 'marital_impute',
                                                             #'anyins_impute',
                                                             'medicaid_impute'), collapse = "+"), sep = "~"))


glm_models <- list()

# Loop over waves
for (w in 0:14) {

  print(paste("Wave:", w))
  # Filter data to include only participants not censored before wave w
  df_wave <- data2 %>%
    filter((is.na(first_censor_wave) | wave_yr <= first_censor_wave) & wave_yr == w)

  # check if wave has censored and uncensored individuals. if not, skip
  if(nrow(df_wave %>% filter(censor_death == 0)) == 0 | nrow(df_wave %>% filter(censor_death == 1)) == 0) {
    print(paste0("Only censored or uncensored at wave: ", w))
    next
  }

  # Fit GLM for wave w
  # model <- glm(mod_death,
  #              data = df_wave,
  #              family = binomial())



  model = glm(mod_stable_death, family = "binomial", data = df_wave)

  # Store model
  glm_models[[paste0("wave_", w)]] <- model
}

today <- str_remove_all(Sys.Date(), "-")

saveRDS(glm_models, here(paste0("output/glm_weights/lung_death_weights_num_", today, ".rds")))
