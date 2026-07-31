################################################
## Create weights for GEE
##  endo cancer
## Project: WHI
## Sophia Fuller
## 04/05/2024
## UPdated: 11/26/2025 with new predictions
################################################

## libraries
library(tidyverse)
library(here)


# denominators ------------------------------------------------------------

### death

# probabilities
death_wv0 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_0.rds")) %>% as.data.frame()
death_wv1 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_1.rds")) %>% as.data.frame()
death_wv2 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_2.rds")) %>% as.data.frame()
death_wv3 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_3.rds")) %>% as.data.frame()
death_wv4 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_4.rds")) %>% as.data.frame()
death_wv5 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_5.rds")) %>% as.data.frame()
death_wv6 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_6.rds")) %>% as.data.frame()
death_wv7 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_7.rds")) %>% as.data.frame()
death_wv8 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_8.rds")) %>% as.data.frame()
death_wv9 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_9.rds")) %>% as.data.frame()
death_wv10 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_10.rds")) %>% as.data.frame()
death_wv11 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_11.rds")) %>% as.data.frame()
death_wv12 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_12.rds")) %>% as.data.frame()
death_wv13 <- readRDS(here("output/weights/endo_death/20251125_denom_wv_13.rds")) %>% as.data.frame()

all_death <- list(death_wv0, death_wv1, death_wv2, death_wv3, death_wv4, death_wv5,
                  death_wv6, death_wv7, death_wv8, death_wv9, death_wv10, death_wv11,
                  death_wv12, death_wv13) %>%
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

endo_weights <- death_long


### LTFU

# probabilities
ltfu_wv0 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_0.rds")) %>% as.data.frame()
ltfu_wv1 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_1.rds")) %>% as.data.frame()
ltfu_wv2 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_2.rds")) %>% as.data.frame()
ltfu_wv3 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_3.rds")) %>% as.data.frame()
ltfu_wv4 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_4.rds")) %>% as.data.frame()
ltfu_wv5 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_5.rds")) %>% as.data.frame()
ltfu_wv6 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_6.rds")) %>% as.data.frame()
ltfu_wv7 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_7.rds")) %>% as.data.frame()
ltfu_wv8 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_8.rds")) %>% as.data.frame()
ltfu_wv9 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_9.rds")) %>% as.data.frame()
ltfu_wv10 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_10.rds")) %>% as.data.frame()
ltfu_wv11 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_11.rds")) %>% as.data.frame()
ltfu_wv12 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_12.rds")) %>% as.data.frame()
ltfu_wv13 <- readRDS(here("output/weights/endo_ltfu/20251126_denom_wv_13.rds")) %>% as.data.frame()

all_ltfu <- list(ltfu_wv0, ltfu_wv1, ltfu_wv2, ltfu_wv3, ltfu_wv4, ltfu_wv5,
                 ltfu_wv6, ltfu_wv7, ltfu_wv8, ltfu_wv9, ltfu_wv10, ltfu_wv11,
                 ltfu_wv12, ltfu_wv13) %>%
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

endo_weights2 <- endo_weights %>% left_join(ltfu_long, by = c("commonid", "setnumber", "wave_yr"))


# numerators --------------------------------------------------------------

## death

## add numerators for predictions
death_wv0 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_0.rds")) %>% as.data.frame()
death_wv1 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_1.rds")) %>% as.data.frame()
death_wv2 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_2.rds")) %>% as.data.frame()
death_wv3 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_3.rds")) %>% as.data.frame()
death_wv4 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_4.rds")) %>% as.data.frame()
death_wv5 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_5.rds")) %>% as.data.frame()
death_wv6 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_6.rds")) %>% as.data.frame()
death_wv7 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_7.rds")) %>% as.data.frame()
death_wv8 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_8.rds")) %>% as.data.frame()
death_wv9 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_9.rds")) %>% as.data.frame()
death_wv10 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_10.rds")) %>% as.data.frame()
death_wv11 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_11.rds")) %>% as.data.frame()
death_wv12 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_12.rds")) %>% as.data.frame()
death_wv13 <- readRDS(here::here("output/weights/endo_death/20251126_num_wv_13.rds")) %>% as.data.frame()



all_death <- list(death_wv0, death_wv1, death_wv2, death_wv3, death_wv4, death_wv5,
                  death_wv6, death_wv7, death_wv8, death_wv9, death_wv10, death_wv11,
                  death_wv12, death_wv13) %>%
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

endo_weights3 <- endo_weights2 %>% left_join(death_long, by = c("commonid", "setnumber", "wave_yr"))


## ltfu

## add ltfu
ltfu_wv0 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_0.rds")) %>% as.data.frame()
ltfu_wv1 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_1.rds")) %>% as.data.frame()
ltfu_wv2 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_2.rds")) %>% as.data.frame()
ltfu_wv3 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_3.rds")) %>% as.data.frame()
ltfu_wv4 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_4.rds")) %>% as.data.frame()
ltfu_wv5 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_5.rds")) %>% as.data.frame()
ltfu_wv6 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_6.rds")) %>% as.data.frame()
ltfu_wv7 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_7.rds")) %>% as.data.frame()
ltfu_wv8 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_8.rds")) %>% as.data.frame()
ltfu_wv9 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_9.rds")) %>% as.data.frame()
ltfu_wv10 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_10.rds")) %>% as.data.frame()
ltfu_wv11 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_11.rds")) %>% as.data.frame()
ltfu_wv12 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_12.rds")) %>% as.data.frame()
ltfu_wv13 <- readRDS(here::here("output/weights/endo_ltfu/20251126_num_wv_13.rds")) %>% as.data.frame()

all_ltfu <- list(ltfu_wv0, ltfu_wv1, ltfu_wv2, ltfu_wv3, ltfu_wv4, ltfu_wv5,
                 ltfu_wv6, ltfu_wv7, ltfu_wv8, ltfu_wv9, ltfu_wv10, ltfu_wv11,
                 ltfu_wv12, ltfu_wv13) %>%
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

endo_weights4 <- endo_weights3 %>% left_join(ltfu_long, by = c("commonid", "setnumber", "wave_yr"))

## ----------------- create weights-------------------------------

endo_weights4  <- endo_weights4  %>%
  mutate(denom_death_weight = 1/(1-prob),
         denom_ltfu_weight = 1/(1-ltfu_prob),
         stable_death_weight = (1-num_prob)/(1-prob),
         stable_ltfu_weight = (1-num_prob_ltfu)/(1-ltfu_prob),
         all_censor_weight = denom_death_weight*denom_ltfu_weight,
         all_censor_stable_weight = stable_death_weight*stable_ltfu_weight)


combined_weights_final <- endo_weights4  %>%
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
    mean_nottruc = mean(ipcw, na.rm = T),
    sd = sd(ipcw_trunc, na.rm = T)
  ) %>% ungroup()%>%
  mutate(across(.cols = where(is.numeric),
                .fns = ~round(.x, 2)))

write.csv(check, here("output/weights/endo_weights_glance.csv"), row.names = F)

today <- str_remove_all(Sys.Date(), "-")

saveRDS(combined_weights_final, file = here::here(paste0("data/main_analysis/data_weights/endo_with_weights_sl_", today, ".rds")))
