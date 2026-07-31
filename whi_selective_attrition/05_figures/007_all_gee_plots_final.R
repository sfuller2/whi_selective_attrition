#################################################
## Create final plot figure
## WHI
## Sophia Fuller
################################################


## libraries
library(tidyverse)
library(here)


## data
load(here("output/plots/final_20251201/001_breast_geeglm_plot.RData"))
plot_breast <- plot_br
load(here("output/plots/final_20251201/002_lung_geeglm_plot.RData"))
plot_lung <- plot_br
load(here("output/plots/final_20251201/003_cr_geeglm_plot.RData"))
load(here("output/plots/final_20251201/004_endo_geeglm_plot.RData"))

plot_endo <- plot_endo + guides(color = "none", linetype = "none")



ggpubr::ggarrange(plotlist = list(plot_breast, plot_lung, plot_cr, plot_endo),
          ncol = 2, nrow = 2, common.legend =  T, legend = "top")


ggsave(filename = here("output/plots/final_20251202/all_gees.png"), dpi = 600,
       width = 12, height = 8, units = "in")


## with CIs
load(here("output/plots/final_20251201/001b_breast_geeglm_cionly_plot.RData"))
breast_ci <- plot_ci + theme(legend.position = "top")
load(here("output/plots/final_20251201/002b_lung_geeglm_cionly_plot.RData"))
lung_ci <- plot_ci + labs(title = "Lung")
load(here("output/plots/final_20251201/003b_cr_geeglm_cionly_plot.RData"))
cr_ci <- plot_ci
load(here("output/plots/final_20251201/004b_endo_geeglm_cionly_plot.RData"))
endo_ci <- plot_ci


ci_all <- ggpubr::ggarrange(plotlist = list(breast_ci, lung_ci, cr_ci, endo_ci),
                  ncol = 2, nrow = 2, common.legend = T, legend = "top")


ggsave(plot = ci_all, filename = here("output/plots/final_20251202/all_gees_cis.png"), dpi = 600,
       width = 12, height = 8, units = "in")


## CIs and risk table
load(here("output/plots/final_20251201/001c_breast_atrisk.RData"))
load(here("output/plots/final_20251201/002c_lung_atrisk.RData"))
load(here("output/plots/final_20251201/003c_cr_atrisk.RData"))
load(here("output/plots/final_20251201/004c_endo_atrisk.RData"))

stage_legend <- ggpubr::get_legend(breast_ci)

# interm
breast_both <- ggpubr::ggarrange(plotlist = list(breast_ci, breast_atrisk),
                                 ncol = 1, nrow = 2, common.legend = T,
                                 legend = "none", align = "v", heights = c(2, 1))
lung_both <- ggpubr::ggarrange(plotlist = list(lung_ci, lung_atrisk + theme(
  axis.title = element_text(size=10),
  axis.text = element_text(size=10),
  #axis.text.y = element_blank(),
  text = element_text(size = 10),
  legend.position = "none")),
                                 ncol = 1, nrow = 2, common.legend = T,
                                 legend = "none", align = "v", heights = c(2, 1))
cr_both <- ggpubr::ggarrange(plotlist = list(cr_ci, cr_atrisk),
                                 ncol = 1, nrow = 2, common.legend = T,
                                 legend = "none", align = "v", heights = c(2, 1))
endo_both <- ggpubr::ggarrange(plotlist = list(endo_ci, endo_atrisk),
                                 ncol = 1, nrow = 2, common.legend = T,
                                 legend = "none", align = "v", heights = c(2, 1))

ci_atrisk_all <- ggpubr::ggarrange(plotlist = list(breast_both, lung_both, cr_both, endo_both),
                            ncol = 2, nrow = 2,  legend.grob = stage_legend, legend = "top")
ci_atrisk_all

ggsave(plot = ci_atrisk_all, filename = here("output/plots/final_20251202/all_gees_cis_atrisk.png"), dpi = 600,
       width = 12, height = 8, units = "in")

