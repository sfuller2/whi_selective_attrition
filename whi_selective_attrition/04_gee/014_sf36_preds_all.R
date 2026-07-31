#####################################
## GEE results tables
## Sophie Fuller
## 06/07/2024
## Updated: 05/22/2026
#####################################
## libraries
library(tidyverse)
library(gtsummary)
library(here)
library(emmeans)
library(flextable)
library(gee)
library(geepack)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
set.seed(1715)

## load models
breast <- readRDS(here("output/r_objects/models/breast_gee_weighted_20251201.rds"))
lung <- readRDS(here("output/r_objects/models/lung_gee_weighted_20251201.rds"))
endo <- readRDS(here("output/r_objects/models/endo_gee_weighted_20251201.rds"))
cr <- readRDS(here("output/r_objects/models/cr_gee_weighted_20251201.rds"))

## SF-36 point estimates
breast_data <- breast$data

predictdata_nocancer <-data.frame(wave_yr = seq(0, 14),
                                  stage_cat="No cancer",
                                  ID=1,
                                  ageatindex = median(breast_data$ageatindex),
                                  ctflag = getmode(breast_data$ctflag),

                                  racenih_cat = getmode(breast_data$racenih_cat),
                                  ethnicnih_cat = getmode(breast_data$ethnicnih_cat),

                                  smoking_impute = getmode(breast_data$smoking_impute),
                                  alcswk_impute = median(breast_data$alcswk_impute), alcswk_impute_flag = 0,
                                  texpwk_impute = median(breast_data$texpwk_impute), texpwk_impute_flag = 0,
                                  socsupp_impute = median(breast_data$socsupp_impute),
                                  ahei_final = median(breast_data$ahei_final)) %>%
  mutate(wave_factor = fct(as.character(wave_yr), levels = paste0(seq(0:14) - 1)),
         wave_factor_grouped = case_when(
           wave_yr < 10 ~ as.character(wave_yr),
           wave_yr >= 10 ~ "10+",
           TRUE ~ NA_character_
         ),
         wave_factor_grouped = fct_relevel(fct(wave_factor_grouped),
                                           c("10+"), after = Inf)
  )


predictdata_local<-predictdata_nocancer
predictdata_local$stage_cat<-"Localized"

predictdata_regional<-predictdata_nocancer
predictdata_regional$stage_cat<-"Regional"

predictdata_distant<-predictdata_nocancer
predictdata_distant$stage_cat<-"Distant"


breast_predictdata_weighted<-rbind(predictdata_nocancer,predictdata_local,predictdata_regional, predictdata_distant)

breast_predictions<-predict(breast,breast_predictdata_weighted, type = "response", allow_new_levels = T, se.fit = T) %>%
  as.data.frame()

breast_predictdata_weighted$physfun <- breast_predictions$fit
breast_predictdata_weighted$lower_ci <- breast_predictdata_weighted$physfun - 1.96 * breast_predictions$se.fit
breast_predictdata_weighted$upper_ci <- breast_predictdata_weighted$physfun + 1.96 * breast_predictions$se.fit

breast_final <- breast_predictdata_weighted %>%
  select(wave_yr, stage_cat, physfun:upper_ci) %>%
  mutate(wave_yr = as.character(wave_yr)) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)),
         across(where(is.numeric), ~sprintf("%.1f", .x)),
         sf_ci = glue::glue("{physfun} ({lower_ci}, {upper_ci})")) %>%
  select(wave_yr, stage_cat, breast_preds = sf_ci)



## LUNG
lung_data <- lung$data

predictdata_nocancer <-data.frame(wave_yr = seq(0, 13),
                                  stage_cat="No cancer",
                                  ID=1,
                                  ageatindex = median(lung_data$ageatindex),
                                  ctflag = getmode(lung_data$ctflag),

                                  racenih_cat = getmode(lung_data$racenih_cat),
                                  ethnicnih_cat = getmode(lung_data$ethnicnih_cat),

                                  smoking_impute = getmode(lung_data$smoking_impute),
                                  alcswk_impute = median(lung_data$alcswk_impute), alcswk_impute_flag = 0,
                                  texpwk_impute = median(lung_data$texpwk_impute), texpwk_impute_flag = 0,
                                  socsupp_impute = median(lung_data$socsupp_impute),
                                  ahei_final = median(lung_data$ahei_final)) %>%
  mutate(wave_factor = fct(as.character(wave_yr), levels = paste0(seq(0:13) - 1)),
         wave_factor_grouped = case_when(
           wave_yr < 10 ~ as.character(wave_yr),
           wave_yr >= 10 ~ "10+",
           TRUE ~ NA_character_
         ),
         wave_factor_grouped = fct_relevel(fct(wave_factor_grouped),
                                           c("10+"), after = Inf)
  )


predictdata_local<-predictdata_nocancer
predictdata_local$stage_cat<-"Localized"

predictdata_regional<-predictdata_nocancer
predictdata_regional$stage_cat<-"Regional"

predictdata_distant<-predictdata_nocancer
predictdata_distant$stage_cat<-"Distant"


lung_predictdata_weighted<-rbind(predictdata_nocancer,predictdata_local,predictdata_regional, predictdata_distant)

lung_predictions<-predict(lung,lung_predictdata_weighted, type = "response", allow_new_levels = T, se.fit = T) %>%
  as.data.frame()

lung_predictdata_weighted$physfun <- lung_predictions$fit
lung_predictdata_weighted$lower_ci <- lung_predictdata_weighted$physfun - 1.96 * lung_predictions$se.fit
lung_predictdata_weighted$upper_ci <- lung_predictdata_weighted$physfun + 1.96 * lung_predictions$se.fit

lung_final <- lung_predictdata_weighted %>%
  select(wave_yr, stage_cat, physfun:upper_ci) %>%
  mutate(wave_yr = as.character(wave_yr)) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)),
         across(where(is.numeric), ~sprintf("%.1f", .x)),
         sf_ci = glue::glue("{physfun} ({lower_ci}, {upper_ci})")) %>%
  select(wave_yr, stage_cat, lung_preds = sf_ci)

## endo
endo_data <- endo$data

predictdata_nocancer <-data.frame(wave_yr = seq(0, 13),
                                  stage_cat="No cancer",
                                  ID=1,
                                  ageatindex = median(endo_data$ageatindex),
                                  ctflag = getmode(endo_data$ctflag),

                                  racenih_cat = getmode(endo_data$racenih_cat),
                                  ethnicnih_cat = getmode(endo_data$ethnicnih_cat),

                                  smoking_impute = getmode(endo_data$smoking_impute),
                                  alcswk_impute = median(endo_data$alcswk_impute), alcswk_impute_flag = 0,
                                  texpwk_impute = median(endo_data$texpwk_impute), texpwk_impute_flag = 0,
                                  socsupp_impute = median(endo_data$socsupp_impute),
                                  ahei_final = median(endo_data$ahei_final)) %>%
  mutate(wave_factor = fct(as.character(wave_yr), levels = paste0(seq(0:13) - 1)),
         wave_factor_grouped = case_when(
           wave_yr < 10 ~ as.character(wave_yr),
           wave_yr >= 10 ~ "10+",
           TRUE ~ NA_character_
         ),
         wave_factor_grouped = fct_relevel(fct(wave_factor_grouped),
                                           c("10+"), after = Inf)
  )


predictdata_local<-predictdata_nocancer
predictdata_local$stage_cat<-"Localized"

predictdata_regional<-predictdata_nocancer
predictdata_regional$stage_cat<-"Regional"

predictdata_distant<-predictdata_nocancer
predictdata_distant$stage_cat<-"Distant"


endo_predictdata_weighted<-rbind(predictdata_nocancer,predictdata_local,predictdata_regional, predictdata_distant)

endo_predictions<-predict(endo,endo_predictdata_weighted, type = "response", allow_new_levels = T, se.fit = T) %>%
  as.data.frame()

endo_predictdata_weighted$physfun <- endo_predictions$fit
endo_predictdata_weighted$lower_ci <- endo_predictdata_weighted$physfun - 1.96 * endo_predictions$se.fit
endo_predictdata_weighted$upper_ci <- endo_predictdata_weighted$physfun + 1.96 * endo_predictions$se.fit

endo_final <- endo_predictdata_weighted %>%
  select(wave_yr, stage_cat, physfun:upper_ci) %>%
  mutate(wave_yr = as.character(wave_yr)) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)),
         across(where(is.numeric), ~sprintf("%.1f", .x)),
         sf_ci = glue::glue("{physfun} ({lower_ci}, {upper_ci})")) %>%
  select(wave_yr, stage_cat, endo_preds = sf_ci)

## CR
cr_data <- cr$data

predictdata_nocancer <-data.frame(wave_yr = seq(0, 13),
                                  stage_cat="No cancer",
                                  ID=1,
                                  ageatindex = median(cr_data$ageatindex),
                                  ctflag = getmode(cr_data$ctflag),

                                  racenih_cat = getmode(cr_data$racenih_cat),
                                  ethnicnih_cat = getmode(cr_data$ethnicnih_cat),

                                  smoking_impute = getmode(cr_data$smoking_impute),
                                  alcswk_impute = median(cr_data$alcswk_impute), alcswk_impute_flag = 0,
                                  texpwk_impute = median(cr_data$texpwk_impute), texpwk_impute_flag = 0,
                                  socsupp_impute = median(cr_data$socsupp_impute),
                                  ahei_final = median(cr_data$ahei_final)) %>%
  mutate(wave_factor = fct(as.character(wave_yr), levels = paste0(seq(0:13) - 1)),
         wave_factor_grouped = case_when(
           wave_yr < 10 ~ as.character(wave_yr),
           wave_yr >= 10 ~ "10+",
           TRUE ~ NA_character_
         ),
         wave_factor_grouped = fct_relevel(fct(wave_factor_grouped),
                                           c("10+"), after = Inf)
  )


predictdata_local<-predictdata_nocancer
predictdata_local$stage_cat<-"Localized"

predictdata_regional<-predictdata_nocancer
predictdata_regional$stage_cat<-"Regional"

predictdata_distant<-predictdata_nocancer
predictdata_distant$stage_cat<-"Distant"


cr_predictdata_weighted<-rbind(predictdata_nocancer,predictdata_local,predictdata_regional, predictdata_distant)

cr_predictions<-predict(cr,cr_predictdata_weighted, type = "response", allow_new_levels = T, se.fit = T) %>%
  as.data.frame()

cr_predictdata_weighted$physfun <- cr_predictions$fit
cr_predictdata_weighted$lower_ci <- cr_predictdata_weighted$physfun - 1.96 * cr_predictions$se.fit
cr_predictdata_weighted$upper_ci <- cr_predictdata_weighted$physfun + 1.96 * cr_predictions$se.fit

cr_final <- cr_predictdata_weighted %>%
  select(wave_yr, stage_cat, physfun:upper_ci) %>%
  mutate(wave_yr = as.character(wave_yr)) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)),
         across(where(is.numeric), ~sprintf("%.1f", .x)),
         sf_ci = glue::glue("{physfun} ({lower_ci}, {upper_ci})")) %>%
  select(wave_yr, stage_cat, cr_preds = sf_ci)

## final results

final_table <- breast_final %>% left_join(lung_final, by = c("wave_yr", "stage_cat")) %>%
  left_join(endo_final, by = c("wave_yr", "stage_cat")) %>%
  left_join(cr_final, by = c("wave_yr", "stage_cat")) %>%
  mutate(wave = as.numeric(wave_yr)) %>%
  mutate(stage_cat = fct(stage_cat, levels = c("No cancer", "Localized", "Regional", "Distant"))) %>%
  filter(wave %in% c(0:5, 10, 13)) %>%
  arrange(wave, stage_cat) %>%
  select(wave, stage_cat:cr_preds)


final_table_out <- final_table %>%
  mutate(wave = NA_character_) %>%
  add_row(wave = "Index/diagnosis",
          .before =  1) %>%
  add_row(wave = "Year 1",
          .before =  6) %>%
  add_row(wave = "Year 2",
          .before =  11) %>%
  add_row(wave = "Year 3",
          .before =  16) %>%
  add_row(wave = "Year 4",
          .before =  21) %>%
  add_row(wave = "Year 5",
          .before =  26) %>%
  add_row(wave = "Year 10",
          .before =  31) %>%
  add_row(wave = "Year 13",
          .before =  36) %>%
  flextable() %>%
  set_header_labels(wave = "Wave",
                    stage_cat = "Cancer status/stage",
                    breast_preds = "Breast cancer",
                    lung_preds = "Lung cancer",
                    endo_preds = "Endometrial cancer",
                    cr_preds = "Colorectal cancer") %>%
  bold(i = c(1, 6, 11, 16, 21, 26, 31, 36),
       j = 1,
       part = "body") %>%
  bold(i = 1,
       part = "header") %>%
  width(j =c(1,2),
        width = 1.25,
        unit = "in") %>%
  width(j =c(3:6),
        width = 1.75,
        unit = "in") %>%
  align(j = 2:5,
        align = "center",
        part = "all")
final_table_out


final_table_out %>% save_as_docx(path = here(glue::glue("output/sf36_estimates.docx")))
