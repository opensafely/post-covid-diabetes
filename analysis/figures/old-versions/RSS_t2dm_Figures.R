## =============================================================================
## Project: Post covid events
##
## Purpose: Construct figures to illustrate results from cox model analysis
## 
## Authors: Kurt Taylor
## 
## Content: 
## 0. Load relevant libraries and read data/arguments
## 
## =============================================================================

# 0. Libraries ------------------------------------------------------------

packages <- c("dplyr", "scales", "ggplot2", "readr", "data.table", "tidyverse",
              "vcd", "gridExtra", "cowplot", "grid", "png")
lapply(packages, require, character.only=T)
rm(list = ls())

# 1. Set directories ------------------------------------------------------

dir <- ("~/Library/CloudStorage/OneDrive-UniversityofBristol/ehr_postdoc/projects/post-covid-diabetes")
setwd(dir)

results_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/OS-outputs-01-08-2022/model/")
output_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/preliminary-results-circulation-jul22/combined-results-report/results-folder-for-report/")

# ------------------------------------######## ------------------------------------#######
# FIGURE 1: GENERATE MAIN COX FIGURE FOR ALL THREE COHORTS ---------------------------------------
# ------------------------------------######## ------------------------------------#######

# Firstly for vax / unvax
source("analysis/figures/cox-figure-scripts/fig1-all-cohorts-RSS.R")

prevaxlist <- main_figures_1("prevax", "reduced")
prevaxcomplist <- main_figures_1("prevax_compare", "reduced")
vaxlist <- main_figures_1("vax", "reduced")
unvaxlist <- main_figures_1("unvax", "reduced")

png(paste0(output_dir,"RSS_T2DM_Main_Figure.png"),
    units = "mm", width=220, height=80, res = 1000)
ggpubr::ggarrange(prevaxlist$t2dm, vaxlist$t2dm, unvaxlist$t2dm, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  font.label = list(size = 10, color = "black", face = "bold", family = NULL))
dev.off() 

png(paste0(output_dir,"RSS_T2DM_Main_Figure_3panel.png"),
    units = "mm", width=220, height=80, res = 1000)
ggpubr::ggarrange(prevaxcomplist$t2dm, vaxlist$t2dm, unvaxlist$t2dm, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  font.label = list(size = 10, color = "black", face = "bold", family = NULL))
dev.off() 

# ------------------------------------######## ------------------------------------#######
# FIGURE 2: TYPE-2 DIABETES HOSPITALISED  --------------------------------------------------------------
# ------------------------------------######## ------------------------------------#######

# Firstly for vax / unvax
source("analysis/figures/cox-figure-scripts/hosp-figures-T2DM-RSS.R")

prevax_hosp <- hosp_fig("prevax")
prevaxcomp_hosp <- hosp_fig("prevax_compare")
vax_hosp <- hosp_fig("vax")
unvax_hosp <- hosp_fig("unvax")

png(paste0(output_dir,"RSS_T2DM_Hosp_Figure.png"),
    units = "mm", width=200, height=100, res = 1000)
ggpubr::ggarrange(prevax_hosp, vax_hosp, unvax_hosp, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  font.label = list(size = 10, color = "black", face = "bold", family = NULL))
dev.off() 

png(paste0(output_dir,"RSS_T2DM_Hosp_Figure_3panel.png"),
    units = "mm", width=220, height=80, res = 1000)
ggpubr::ggarrange(prevaxcomp_hosp, vax_hosp, unvax_hosp, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  font.label = list(size = 10, color = "black", face = "bold", family = NULL))
dev.off() 

# ------------------------------------######## ------------------------------------#######
# FIGURE 3: TYPE-2 DIABETES SUBGROUPS --------------------------------------------------------------
# ------------------------------------######## ------------------------------------#######
# col per cohort, row per subgroup (max time points available for all cats in subgroup), exclude overall (6x3 panel figure)

source("analysis/figures/cox-figure-scripts/subgroup-figures-T2DM-RSS.R")

prevax_subgroup <- subgroup_fig("prevax")
prevaxcomp_subgroup <- subgroup_fig("prevax_compare")
vax_subgroup <- subgroup_fig("vax")
unvax_subgroup <- subgroup_fig("unvax")

png(paste0(output_dir,"RSS_T2DM_Subgroup_Figure.png"),
    units = "mm", width=200, height=100, res = 1000)
ggpubr::ggarrange(prevax_subgroup, vax_subgroup, unvax_subgroup, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  font.label = list(size = 10, color = "black", face = "bold", family = NULL))
dev.off() 

png(paste0(output_dir,"RSS_T2DM_Subgroup_Figure_3panel.png"),
    units = "mm", width=200, height=100, res = 1000)
ggpubr::ggarrange(prevaxcomp_subgroup, vax_subgroup, unvax_subgroup, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  font.label = list(size = 10, color = "black", face = "bold", family = NULL))
dev.off() 

# END 