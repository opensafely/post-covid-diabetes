# 0. Libraries ------------------------------------------------------------

packages <- c("dplyr", "scales", "ggplot2", "readr", "data.table", "tidyverse",
              "vcd", "gridExtra", "cowplot", "grid", "png", "plyr")
lapply(packages, require, character.only=T)
rm(list = ls())

dir <- ("~/Library/CloudStorage/OneDrive-UniversityofBristol/ehr_postdoc/projects/post-covid-diabetes")
setwd(dir)

results_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/model/")
output_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/generated-figures/supplementary/")

# ------------------------------------######## ------------------------------------#######
# FIGURE 3: TYPE-2 DIABETES SUBGROUPS --------------------------------------------------------------
# ------------------------------------######## ------------------------------------#######
# col per cohort, row per subgroup (max time points available for all cats in subgroup), exclude overall (6x3 panel figure)

source("analysis/figures/supplementary/FIG-S7-t2dm-subgroups-function.R")

prevax_subgroup <- subgroup_fig("prevax")
vax_subgroup <- subgroup_fig("vax")
unvax_subgroup <- subgroup_fig("unvax")

png(paste0(output_dir,"t2dm-subgroups_extended.png"),
    units = "mm", width=470, height=380, res = 1000)
ggpubr::ggarrange(prevax_subgroup, vax_subgroup, unvax_subgroup, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  font.label = list(size = 26, color = "black", face = "bold", family = NULL)) 
dev.off() 

tiff(paste0(output_dir,"t2dm-subgroups_extended.tiff"),
    units = "mm", width=470, height=380, res = 200)
ggpubr::ggarrange(prevax_subgroup, vax_subgroup, unvax_subgroup, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  font.label = list(size = 26, color = "black", face = "bold", family = NULL)) 
dev.off() 

# END 