################################################
## Create weights for GEE
##  breast cancer
## Project: WHI
## Sophia Fuller
## 04/05/2024
## UPdated: 09/18/2025 with new predictions
################################################

## libraries
library(tidyverse)
library(here)

## data
#breast_preds_sl <- readRDS(here::here("data/main_analysis/data_weights/death_denom_preds_sl_20250925.rds"))


## ------------------ BREAST -------------------------

# denominators ------------------------------------------------------------

### death

# probabilities
death_wv0 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_0.rds")) %>% as.data.frame()
death_wv1 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_1.rds")) %>% as.data.frame()
death_wv2 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_2.rds")) %>% as.data.frame()
death_wv3 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_3.rds")) %>% as.data.frame()
death_wv4 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_4.rds")) %>% as.data.frame()
death_wv5 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_5.rds")) %>% as.data.frame()
death_wv6 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_6.rds")) %>% as.data.frame()
death_wv7 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_7.rds")) %>% as.data.frame()
death_wv8 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_8.rds")) %>% as.data.frame()
death_wv9 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_9.rds")) %>% as.data.frame()
death_wv10 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_10.rds")) %>% as.data.frame()
death_wv11 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_11.rds")) %>% as.data.frame()
death_wv12 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_12.rds")) %>% as.data.frame()
death_wv13 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_13.rds")) %>% as.data.frame()
death_wv14 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_14.rds")) %>% as.data.frame()
death_wv15 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_15.rds")) %>% as.data.frame()
death_wv16 <- readRDS(here("output/weights/breast_death/20250916_denom_wv_16.rds")) %>% as.data.frame()


all_death <- list(death_wv0, death_wv1, death_wv2, death_wv3, death_wv4, death_wv5,
                  death_wv6, death_wv7, death_wv8, death_wv9, death_wv10, death_wv11,
                  death_wv12, death_wv13, death_wv14, death_wv15, death_wv16) %>%
  reduce(full_join, by = c("commonid", "setnumber"))

## make long
death_long <- all_death %>%
  pivot_longer(
    cols = starts_with("denom_preds_"),
    names_to = "wave_yr",
    names_prefix = "denom_preds_wv",
    values_to = "prob",
    values_drop_na = TRUE
  )

death_long$wave_yr <- as.numeric(death_long$wave_yr)

breast_weights <- death_long

## add numerators for predictions
death_wv0 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_0.rds")) %>% as.data.frame()
death_wv1 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_1.rds")) %>% as.data.frame()
death_wv2 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_2.rds")) %>% as.data.frame()
death_wv3 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_3.rds")) %>% as.data.frame()
death_wv4 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_4.rds")) %>% as.data.frame()
death_wv5 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_5.rds")) %>% as.data.frame()
death_wv6 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_6.rds")) %>% as.data.frame()
death_wv7 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_7.rds")) %>% as.data.frame()
death_wv8 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_8.rds")) %>% as.data.frame()
death_wv9 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_9.rds")) %>% as.data.frame()
death_wv10 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_10.rds")) %>% as.data.frame()
death_wv11 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_11.rds")) %>% as.data.frame()
death_wv12 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_12.rds")) %>% as.data.frame()
death_wv13 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_13.rds")) %>% as.data.frame()
death_wv14 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_14.rds")) %>% as.data.frame()
death_wv15 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_15.rds")) %>% as.data.frame()
death_wv16 <- readRDS(here::here("output/weights/breast_death/20251126_num_wv_16.rds")) %>% as.data.frame()


all_death <- list(death_wv0, death_wv1, death_wv2, death_wv3, death_wv4, death_wv5,
                  death_wv6, death_wv7, death_wv8, death_wv9, death_wv10, death_wv11,
                  death_wv12, death_wv13, death_wv14, death_wv15, death_wv16) %>%
  reduce(full_join, by = c("commonid", "setnumber"))

## make long
death_long <- all_death %>%
  pivot_longer(
    cols = starts_with("num_preds_"),
    names_to = "wave_yr",
    names_prefix = "num_preds_wv",
    values_to = "num_prob",
    values_drop_na = TRUE
  )

death_long$wave_yr <- as.numeric(death_long$wave_yr)

## merge with dataset

breast_weights2 <- breast_weights %>% left_join(death_long, by = c("commonid", "setnumber", "wave_yr"))


# LTFU --------------------------------------------------------------------

# probabilities
ltfu_wv0 <- readRDS(here("output/weights/breast_ltfu/denom_wv_0.rds")) %>% as.data.frame()
ltfu_wv1 <- readRDS(here("output/weights/breast_ltfu/denom_wv_1.rds")) %>% as.data.frame()
ltfu_wv2 <- readRDS(here("output/weights/breast_ltfu/denom_wv_2.rds")) %>% as.data.frame()
ltfu_wv3 <- readRDS(here("output/weights/breast_ltfu/denom_wv_3.rds")) %>% as.data.frame()
ltfu_wv4 <- readRDS(here("output/weights/breast_ltfu/denom_wv_4.rds")) %>% as.data.frame()
ltfu_wv5 <- readRDS(here("output/weights/breast_ltfu/denom_wv_5.rds")) %>% as.data.frame()
ltfu_wv6 <- readRDS(here("output/weights/breast_ltfu/denom_wv_6.rds")) %>% as.data.frame()
ltfu_wv7 <- readRDS(here("output/weights/breast_ltfu/denom_wv_7.rds")) %>% as.data.frame()
ltfu_wv8 <- readRDS(here("output/weights/breast_ltfu/denom_wv_8.rds")) %>% as.data.frame()
ltfu_wv9 <- readRDS(here("output/weights/breast_ltfu/denom_wv_9.rds")) %>% as.data.frame()
ltfu_wv10 <- readRDS(here("output/weights/breast_ltfu/denom_wv_10.rds")) %>% as.data.frame()
ltfu_wv11 <- readRDS(here("output/weights/breast_ltfu/denom_wv_11.rds")) %>% as.data.frame()
ltfu_wv12 <- readRDS(here("output/weights/breast_ltfu/denom_wv_12.rds")) %>% as.data.frame()
ltfu_wv13 <- readRDS(here("output/weights/breast_ltfu/denom_wv_13.rds")) %>% as.data.frame()
ltfu_wv14 <- readRDS(here("output/weights/breast_ltfu/denom_wv_14.rds")) %>% as.data.frame()
ltfu_wv15 <- readRDS(here("output/weights/breast_ltfu/denom_wv_15.rds")) %>% as.data.frame()
ltfu_wv16 <- readRDS(here("output/weights/breast_ltfu/denom_wv_16.rds")) %>% as.data.frame()


all_ltfu <- list(ltfu_wv0, ltfu_wv1, ltfu_wv2, ltfu_wv3, ltfu_wv4, ltfu_wv5,
                  ltfu_wv6, ltfu_wv7, ltfu_wv8, ltfu_wv9, ltfu_wv10, ltfu_wv11,
                  ltfu_wv12, ltfu_wv13, ltfu_wv14, ltfu_wv15, ltfu_wv16) %>%
  reduce(full_join, by = c("commonid", "setnumber"))

## make long
ltfu_long <- all_ltfu %>%
  pivot_longer(
    cols = starts_with("denom_preds_"),
    names_to = "wave_yr",
    names_prefix = "denom_preds_wv",
    values_to = "ltfu_prob",
    values_drop_na = TRUE
  )

ltfu_long$wave_yr <- as.numeric(ltfu_long$wave_yr)

breast_weights3 <- breast_weights2 %>% left_join(ltfu_long, by = c("commonid", "setnumber", "wave_yr"))



## add ltfu
ltfu_wv0 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_0.rds")) %>% as.data.frame()
ltfu_wv1 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_1.rds")) %>% as.data.frame()
ltfu_wv2 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_2.rds")) %>% as.data.frame()
ltfu_wv3 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_3.rds")) %>% as.data.frame()
ltfu_wv4 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_4.rds")) %>% as.data.frame()
ltfu_wv5 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_5.rds")) %>% as.data.frame()
ltfu_wv6 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_6.rds")) %>% as.data.frame()
ltfu_wv7 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_7.rds")) %>% as.data.frame()
ltfu_wv8 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_8.rds")) %>% as.data.frame()
ltfu_wv9 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_9.rds")) %>% as.data.frame()
ltfu_wv10 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_10.rds")) %>% as.data.frame()
ltfu_wv11 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_11.rds")) %>% as.data.frame()
ltfu_wv12 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_12.rds")) %>% as.data.frame()
ltfu_wv13 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_13.rds")) %>% as.data.frame()
ltfu_wv14 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_14.rds")) %>% as.data.frame()
ltfu_wv15 <- readRDS(here::here("output/weights/breast_ltfu/20251126_num_wv_15.rds")) %>% as.data.frame()

all_ltfu <- list(ltfu_wv0, ltfu_wv1, ltfu_wv2, ltfu_wv3, ltfu_wv4, ltfu_wv5,
                 ltfu_wv6, ltfu_wv7, ltfu_wv8, ltfu_wv9, ltfu_wv10, ltfu_wv11,
                 ltfu_wv12, ltfu_wv13, ltfu_wv14, ltfu_wv15) %>%
  reduce(full_join, by = c("commonid", "setnumber"))

## make long
ltfu_long <- all_ltfu %>%
  pivot_longer(
    cols = starts_with("num_preds_"),
    names_to = "wave_yr",
    names_prefix = "num_preds_wv",
    values_to = "num_prob_ltfu",
    values_drop_na = TRUE
  )

ltfu_long$wave_yr <- as.numeric(ltfu_long$wave_yr)

breast_weights4 <- breast_weights3 %>% left_join(ltfu_long, by = c("commonid", "setnumber", "wave_yr"))

## ----------------- create weights-------------------------------

breast_weights4 <- breast_weights4 %>%
  mutate(denom_death_weight = 1/(1-prob),
         denom_ltfu_weight = 1/(1-ltfu_prob),
         stable_death_weight = (1-num_prob)/(1-prob),
         stable_ltfu_weight = (1-num_prob_ltfu)/(1-ltfu_prob),
         all_censor_weight = denom_death_weight*denom_ltfu_weight,
         all_censor_stable_weight = stable_death_weight*stable_ltfu_weight)


combined_weights_final <- breast_weights4 %>%
  arrange(setnumber, commonid, wave_yr) %>%
  group_by(setnumber, commonid) %>%
  mutate(
    ipcw = all_censor_stable_weight,  # assuming within_weight is the wave-specific IPCW
    cum_weight = cumprod(ipcw)  # running cumulative product across waves
  ) %>%
  ungroup() %>%
  mutate(ipcw = cum_weight) %>%
  select(-cum_weight) %>%
  group_by(wave_yr) %>%
  mutate(p01 = quantile(ipcw, p = 0.01, na.rm = T),
         p99 = quantile(ipcw, p = 0.99, na.rm = T)) %>%
  ungroup() %>%
  mutate(ipcw_trunc = case_when(
    ipcw < p01 ~ p01,
    ipcw > p99 ~ p99,
    TRUE ~ ipcw
  ))


# look at weights
check <- combined_weights_final %>%
  group_by(wave_yr) %>%
  summarize(
    min_nottrunc = min(ipcw, na.rm = T),
    min_trunc = min(ipcw_trunc, na.rm = T),
    #p01 = quantile(ipcw, p = 0.01, na.rm = T),
    q1 = quantile(ipcw_trunc, 0.25, na.rm = T),
    median = median(ipcw_trunc, na.rm = T),
    q3 = quantile(ipcw_trunc, 0.75, na.rm = T),
    #p99 = quantile(ipcw, p = 0.99, na.rm = T),
    max_nottrunc = max(ipcw, na.rm = T),
    max_trunc = max(ipcw_trunc, na.rm = T),
    mean = mean(ipcw_trunc, na.rm = T),
    sd = sd(ipcw_trunc, na.rm = T)
  ) %>% ungroup() %>%
  mutate(across(.cols = where(is.numeric),
                .fns = ~round(.x, 2)))

write.csv(check, here("output/weights/breast_weights_glance.csv"), row.names = F)

today <- str_remove_all(Sys.Date(), "-")

saveRDS(combined_weights_final, file = here::here(paste0("data/main_analysis/data_weights/breast_with_weights_sl_", today, ".rds")))


















breast_weights %>%
  group_by(wave_yr, ccstat) %>%
  dplyr::summarize(n = n(),
                   mean = mean(all_censor_weight, na.rm = T),
                   std = sd(all_censor_weight, na.rm = T),
                   min = min(all_censor_weight, na.rm = T),
                   max = max(all_censor_weight, na.rm = T)) %>%
  ungroup() %>%
  filter(wave_yr %in% c(1,5,10))

# updated 9/15/2025

breast_weights_final <- breast_weights %>%
  arrange(setnumber, commonid, wave_yr) %>%
  group_by(setnumber, commonid) %>%
  mutate(
    ipcw = all_censor_stable_weight,  # assuming within_weight is the wave-specific IPCW
    cum_weight = cumprod(ipcw)  # running cumulative product across waves
  ) %>%
  ungroup() %>%
  mutate(ipcw = cum_weight) %>%
  select(-cum_weight) %>%
  group_by(wave_yr) %>%
  mutate(p01 = quantile(ipcw, p = 0.01, na.rm = T),
         p99 = quantile(ipcw, p = 0.99, na.rm = T)) %>%
  ungroup() %>%
  mutate(ipcw_trunc = case_when(
    ipcw < p01 ~ p01,
    ipcw > p99 ~ p99,
    TRUE ~ ipcw
  )) %>%
  filter(wave_yr != 16)
#
#
# breast_weights_total <- breast_weights %>%
#   dplyr::group_by(commonid, setnumber) %>%
#   dplyr::mutate(total_all = prod(all_censor_weight, na.rm = T),
#                 total_stable = prod(all_censor_stable_weight, na.rm = T)) %>%
#   ungroup()

# check
# look at weights
breast_weights_final %>%
  group_by(wave_yr) %>%
  summarize(
    min_nottrunc = min(ipcw, na.rm = T),
    min_trunc = min(ipcw_trunc, na.rm = T),
    #p01 = quantile(ipcw, p = 0.01, na.rm = T),
    q1 = quantile(ipcw_trunc, 0.25, na.rm = T),
    median = median(ipcw_trunc, na.rm = T),
    q3 = quantile(ipcw_trunc, 0.75, na.rm = T),
    #p99 = quantile(ipcw, p = 0.99, na.rm = T),
    max_nottrunc = max(ipcw, na.rm = T),
    max_trunc = max(ipcw_trunc, na.rm = T),
    mean = mean(ipcw_trunc, na.rm = T),
    sd = sd(ipcw_trunc, na.rm = T)
  ) %>% ungroup() %>%
  print(n = 16)

saveRDS(breast_weights_final, file = here::here("data/main_analysis/data_weights/breast_with_weights_sl_20250926.rds"))


breast_weights_total %>%
  group_by(commonid, setnumber) %>%
  slice_head() %>%
  ungroup() %>%
  group_by(ccstat) %>%
  dplyr::summarize(n = n(),
                   n_miss = sum(is.na(total_stable)),
                   mean = mean(total_stable , na.rm = T),
                   std = sd(total_stable , na.rm = T),
                   min = min(total_stable , na.rm = T),
                   max = max(total_stable , na.rm = T)) %>%
  ungroup()

## truncate weights
breast_weights_trunc <- breast_weights_total %>%
  mutate(p01_weight = quantile(total_stable, 0.01, na.rm = T),
         p99_weight = quantile(total_stable, 0.99, na.rm = T),
         total_weight_trunc = case_when(
           total_stable < p01_weight ~ p01_weight,
           total_stable> p99_weight ~ p99_weight,
           TRUE ~ total_stable
         ),
         p01_weight_us = quantile(total_all, 0.01, na.rm = T),
         p99_weight_us = quantile(total_all, 0.99, na.rm = T),
         total_weight_trunc_us = case_when(
           total_all< p01_weight_us ~ p01_weight_us,
           total_all > p99_weight_us ~ p99_weight_us,
           TRUE ~ total_all
         ))


breast_weights_trunc %>%
  group_by(wave_yr) %>%
  dplyr::summarise(n = n(),
                   min = min(total_weight_trunc, na.rm = T),
                   q1 = quantile(total_weight_trunc, 0.25, na.rm = T),
                   median = median(total_weight_trunc, na.rm = T),
                   q3 = quantile(total_weight_trunc, 0.75,na.rm = T),
                   max = max(total_weight_trunc, na.rm = T))

breast_weights_trunc %>%
  group_by(wave_yr) %>%
  dplyr::summarise(n = n(),
                   min = min(total_weight_trunc_us, na.rm = T),
                   q1 = quantile(total_weight_trunc_us, 0.25, na.rm = T),
                   median = median(total_weight_trunc_us, na.rm = T),
                   q3 = quantile(total_weight_trunc_us, 0.75,na.rm = T),
                   max = max(total_weight_trunc_us, na.rm = T))



## ouptut dataset
saveRDS(breast_weights_trunc, file = here::here("data/main_analysis/data_weights/breast_with_weights_sl.rds"))

