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

results_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/generated-figures/")
output_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/generated-figures/")

# ------------------------------------######## ------------------------------------#######
# BUILD FLOW CHART FIGURE ---------------------------------------
# ------------------------------------######## ------------------------------------#######

# prevax
prevax_flow <- readPNG(paste0(output_dir, "cohort_flow_prevax.png"))
prevax_flow <- rasterGrob(prevax_flow)

# vax
vax_flow <- readPNG(paste0(output_dir, "cohort_flow_vax.png"))
vax_flow <- rasterGrob(vax_flow)

# unvax
unvax_flow <- readPNG(paste0(output_dir, "cohort_flow_unvax.png"))
unvax_flow <- rasterGrob(unvax_flow)

# build and save figure

png(paste0(output_dir,"cohort_flow_multipanel.png"),
    units = "mm", width=200, height=130, res = 1000)
ggpubr::ggarrange(prevax_flow, vax_flow, unvax_flow, 
                  labels = c("Pre-vaccination", "Vaccinated", "Unvaccinated"),
                  hjust = -0.1,
                  font.label = list(size = 10),
                  ncol=3)
dev.off()

# ------------