
packages <- c("dplyr", "scales", "ggplot2", "readr", "data.table", "tidyverse",
              "vcd", "gridExtra", "cowplot", "grid", "png")
lapply(packages, require, character.only=T)
rm(list = ls())

hosp <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/preliminary-results-circulation-jul22/combined-results-report/results-folder-for-report/Figure2_covid_pheno_HOSP_all_cohorts_TEST.png")
hosp <- rasterGrob(hosp)

nonhosp <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/preliminary-results-circulation-jul22/combined-results-report/results-folder-for-report/Figure2_covid_pheno_NON_HOSP_all_cohorts_TEST.png")
nonhosp <- rasterGrob(nonhosp)

png(paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/preliminary-results-circulation-jul22/combined-results-report/results-folder-for-report/Figure2_Diabetes_JS.png"),
    units = "mm", width=160, height=110, res = 1000)
grid.arrange(arrangeGrob(hosp,top=textGrob("Hospitalised", gp = gpar(fontsize = 5)),   
                         ncol=1),
             arrangeGrob(nonhosp,top=textGrob("Non-Hospitalised", gp = gpar(fontsize = 5)),
                         ncol=1), ncol = 2)
dev.off()