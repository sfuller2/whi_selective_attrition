
# libraries
library(tidyverse)
library(plyr)
library(here)
library(ggpubr)




#### BREAST CANCER #####
breast <- readRDS(here::here("data/main_analysis/data_weights/breast_with_weights_sl.rds")) %>%
  mutate(ethnicnih_cat = factor(ethnicnih_cat, levels = c("Not Hispanic/Latino", "Hispanic/Latino")),
         stage_cat = factor(stage_cat, levels = c("No cancer", "Localized", "Regional", "Distant")),
         ccstat2 = factor(ccstat, levels = c("Control", "Case"))) %>%
  arrange(commonid, setnumber, wave_yr) %>%
  group_by(commonid, setnumber) %>%
  dplyr::mutate(id2 = cur_group_id()) %>%
  ungroup() %>%
  filter(!is.na(ahei_final))

casecontrol_temp_br <- breast
casecontrol_temp_br$TimeFromIndex<-round(casecontrol_temp_br$wave_yr)

## unadjusted SF36
SF36overTime<-ddply(casecontrol_temp_br,.(TimeFromIndex,stage_cat),function(x) data.frame(SF36mean=mean(x$physfun, na.rm = T),n=nrow(x)))
breast_unadj<-ggplot(SF36overTime,aes(TimeFromIndex,SF36mean,col=stage_cat,fill=stage_cat,group=stage_cat))+
  geom_line()+
  ylab("Physical Function")+
  xlab("Years Since Index")+
  scale_x_continuous(limits = c(0, 15),breaks = seq(0, 15,by=1))+
  scale_y_continuous(limits = c(40, 100), breaks = seq(40, 100, by = 20)) +
  scale_color_brewer("Stage",palette = "Set1")+
  scale_fill_brewer("Stage",palette = "Set1")+
  ggtitle("A. Breast")+
  theme_classic()+
  theme(text = element_text(size = 10),
        legend.position = "none")

breast_unadj

num_cont <- breast %>%
  group_by(wave_yr, stage_cat) %>%
  dplyr::summarize(count = n(), censor = sum(censor, na.rm = T)) %>%
  group_by(stage_cat) %>%
  mutate(cumsum_censor = cumsum(censor)) %>%
  mutate(
    prop = case_when(
      stage_cat == "No cancer" ~ round(count/(12407 + 112), 2), # add wave 1 plus number censored in wave 0
      stage_cat == "Localized" ~ round(count/(2038+5), 2),
      stage_cat == "Regional" ~ round(count/(540+3), 2),
      stage_cat == "Distant" ~ round(count/(44+4), 2)
    )) %>%
  ungroup()

num_cont <- num_cont %>%
  mutate(prop = case_when(
    wave_yr == 0 ~ 1,
    TRUE ~ prop
  ))


breast_prop <- num_cont %>% ggplot(aes(wave_yr, prop, col = stage_cat)) +
  geom_line()+
  ylab("Proportion remaining in cohort")+
  xlab("Years Since Index")+
  scale_x_continuous(limits = c(0,15),breaks = seq(0,15,by=1))+
  scale_color_brewer("Stage",palette = "Set1")+
  scale_fill_brewer("Stage",palette = "Set1")+
  ggtitle("A. Breast")+
  theme_classic()+
  theme(text = element_text(size = 10))


breast_prop

#### LUNG CANCER #####
lung <- readRDS(here::here("data/main_analysis/data_weights/lung_with_weights_sl.rds")) %>%
  mutate(ethnicnih_cat = factor(ethnicnih_cat, levels = c("Not Hispanic/Latino", "Hispanic/Latino")),
         stage_cat = factor(stage_cat, levels = c("No cancer", "Localized", "Regional", "Distant")),
         ccstat2 = factor(ccstat, levels = c("Control", "Case"))) %>%
  arrange(commonid, setnumber, wave_yr) %>%
  group_by(commonid, setnumber) %>%
  dplyr::mutate(id2 = cur_group_id()) %>%
  ungroup() %>%
  filter(!is.na(ahei_final))

casecontrol_temp_lung <- lung
casecontrol_temp_lung$TimeFromIndex<-round(casecontrol_temp_lung$wave_yr)

## unadjusted SF36
SF36overTime<-ddply(casecontrol_temp_lung,.(TimeFromIndex,stage_cat),function(x) data.frame(SF36mean=mean(x$physfun, na.rm = T),n=nrow(x)))
lung_unadj<-ggplot(SF36overTime,aes(TimeFromIndex,SF36mean,col=stage_cat,fill=stage_cat,group=stage_cat))+
  geom_line()+
  ylab("Physical Function")+
  xlab("Years Since Index")+
  scale_x_continuous(limits = c(0, 15),breaks = seq(0, 15,by=1))+
  scale_y_continuous(limits = c(40, 100), breaks = seq(40, 100, by = 20)) +
  scale_color_brewer("Stage",palette = "Set1")+
  scale_fill_brewer("Stage",palette = "Set1")+
  ggtitle("B. Lung")+
  theme_classic()+
  theme(text = element_text(size = 10),
        legend.position = "none")

lung_unadj

num_cont <- lung %>%
  group_by(wave_yr, stage_cat) %>%
  dplyr::summarize(count = n(), censor = sum(censor, na.rm = T)) %>%
  group_by(stage_cat) %>%
  mutate(cumsum_censor = cumsum(censor)) %>%
  mutate(
    prop = case_when(
      stage_cat == "No cancer" ~ round(count/(4195+ 39), 2),
      stage_cat == "Localized" ~ round(count/(266+4), 2),
      stage_cat == "Regional" ~ round(count/(160+8), 2),
      stage_cat == "Distant" ~ round(count/(139+47), 2)
    )) %>%
  ungroup()

num_cont <- num_cont %>%
  mutate(prop = case_when(
    wave_yr == 0 ~ 1,
    TRUE ~ prop
  ))


lung_prop <- num_cont %>% ggplot(aes(wave_yr, prop, col = stage_cat)) +
  geom_line()+
  ylab("Proportion remaining in cohort")+
  xlab("Years Since Index")+
  scale_x_continuous(limits = c(0,15),breaks = seq(0,15,by=1))+
  scale_color_brewer("Stage",palette = "Set1")+
  scale_fill_brewer("Stage",palette = "Set1")+
  ggtitle("B. Lung")+
  theme_classic()+
  theme(text = element_text(size = 10))

lung_prop



#### COLORECTAL CANCER #####
cr <- readRDS(here::here("data/main_analysis/data_weights/cr_with_weights_sl.rds")) %>%
  mutate(ethnicnih_cat = factor(ethnicnih_cat, levels = c("Not Hispanic/Latino", "Hispanic/Latino")),
         stage_cat = factor(stage_cat, levels = c("No cancer", "Localized", "Regional", "Distant")),
         ccstat2 = factor(ccstat, levels = c("Control", "Case"))) %>%
  arrange(commonid, setnumber, wave_yr) %>%
  group_by(commonid, setnumber) %>%
  dplyr::mutate(id2 = cur_group_id()) %>%
  ungroup() %>%
  filter(!is.na(ahei_final))

casecontrol_temp_cr <- cr
casecontrol_temp_cr$TimeFromIndex<-round(casecontrol_temp_cr$wave_yr)

## unadjusted SF36
SF36overTime<-ddply(casecontrol_temp_cr,.(TimeFromIndex,stage_cat),function(x) data.frame(SF36mean=mean(x$physfun, na.rm = T),n=nrow(x)))
cr_unadj<-ggplot(SF36overTime,aes(TimeFromIndex,SF36mean,col=stage_cat,fill=stage_cat,group=stage_cat))+
  geom_line()+
  ylab("Physical Function")+
  xlab("Years Since Index")+
  scale_x_continuous(limits = c(0, 15),breaks = seq(0, 15,by=1))+
  scale_y_continuous(limits = c(20, 100), breaks = seq(40, 100, by = 20)) +
  scale_color_brewer("Stage",palette = "Set1")+
  scale_fill_brewer("Stage",palette = "Set1")+
  ggtitle("C. Colorectal")+
  theme_classic()+
  theme(text = element_text(size = 10),
        legend.position = "none")

num_cont <- cr %>%
  group_by(wave_yr, stage_cat) %>%
  dplyr::summarize(count = n(), censor = sum(censor, na.rm = T)) %>%
  group_by(stage_cat) %>%
  mutate(cumsum_censor = cumsum(censor)) %>%
  mutate(
    prop = case_when(
      stage_cat == "No cancer" ~ round(count/(3245 + 37), 2), # add wave 1 plus number censored in wave 0
      stage_cat == "Localized" ~ round(count/(300+1), 2),
      stage_cat == "Regional" ~ round(count/(268+4), 2),
      stage_cat == "Distant" ~ round(count/(50+7), 2)
    )) %>%
  ungroup()

num_cont <- num_cont %>%
  mutate(prop = case_when(
    wave_yr == 0 ~ 1,
    TRUE ~ prop
  ))


cr_prop <- num_cont %>% ggplot(aes(wave_yr, prop, col = stage_cat)) +
  geom_line()+
  ylab("Proportion remaining in cohort")+
  xlab("Years Since Index")+
  scale_x_continuous(limits = c(0,15),breaks = seq(0,15,by=1))+
  scale_color_brewer("Stage",palette = "Set1")+
  scale_fill_brewer("Stage",palette = "Set1")+
  ggtitle("C. Colorectal")+
  theme_classic()+
  theme(text = element_text(size = 10))

cr_prop

#### ENDOMETRIAL CANCER #####
endo <- readRDS(here::here("data/main_analysis/data_weights/endo_with_weights_sl.rds")) %>%
  mutate(ethnicnih_cat = factor(ethnicnih_cat, levels = c("Not Hispanic/Latino", "Hispanic/Latino")),
         stage_cat = factor(stage_cat, levels = c("No cancer", "Localized", "Regional", "Distant")),
         ccstat2 = factor(ccstat, levels = c("Control", "Case"))) %>%
  arrange(commonid, setnumber, wave_yr) %>%
  group_by(commonid, setnumber) %>%
  dplyr::mutate(id2 = cur_group_id()) %>%
  ungroup() %>%
  filter(!is.na(ahei_final))

casecontrol_temp_endo <- endo
casecontrol_temp_endo$TimeFromIndex<-round(casecontrol_temp_endo$wave_yr)

## unadjusted SF36
SF36overTime<-ddply(casecontrol_temp_endo,.(TimeFromIndex,stage_cat),function(x) data.frame(SF36mean=mean(x$physfun, na.rm = T),n=nrow(x)))
endo_unadj<-ggplot(SF36overTime,aes(TimeFromIndex,SF36mean,col=stage_cat,fill=stage_cat,group=stage_cat))+
  geom_line()+
  ylab("Physical Function")+
  xlab("Years Since Index")+
  scale_x_continuous(limits = c(0, 15),breaks = seq(0, 15,by=1))+
  scale_y_continuous(limits = c(20, 100), breaks = seq(40, 100, by = 20)) +
  scale_color_brewer("Stage",palette = "Set1")+
  scale_fill_brewer("Stage",palette = "Set1")+
  ggtitle("D. Endometrial")+
  theme_classic()+
  theme(text = element_text(size = 10),
        legend.position = "none")

endo_unadj


endo_prop <- num_cont %>% ggplot(aes(wave_yr, prop, col = stage_cat)) +
  geom_line()+
  ylab("Proportion remaining in cohort")+
  xlab("Years Since Index")+
  scale_x_continuous(limits = c(0,15),breaks = seq(0,15,by=1))+
  scale_color_brewer("Stage",palette = "Set1")+
  scale_fill_brewer("Stage",palette = "Set1")+
  ggtitle("D. Endometrial")+
  theme_classic()+
  theme(text = element_text(size = 10))

endo_prop

unadj_plots <- ggarrange(breast_unadj, lung_unadj, cr_unadj, endo_unadj, nrow = 2, ncol = 2, align = "h", legend = "top", common.legend = T)
unadj_plots


prop_plots <- ggarrange(breast_prop, lung_prop, cr_prop, endo_prop, nrow = 2, ncol = 2, align = "h", legend = "top", common.legend = T)
prop_plots

ggsave(plot = unadj_plots, filename = here::here("output/plots/final/003_unadj_physfun_plots.jpg"),
       height = 6, width = 10, units = "in", dpi = 600)

ggsave(plot = prop_plots, filename = here::here("output/plots/final/004_prop_atrisk_plots.jpg"),
       height = 6, width = 10, units = "in", dpi = 600)
