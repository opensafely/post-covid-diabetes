# combine histograms showing days since COVID diagnosis for t2dm events

# libraries ---------------------------------------------------------------

packages <- c("dplyr", "scales", "ggplot2", "readr", "data.table", "tidyverse",
              "vcd", "gridExtra", "cowplot", "grid", "png")

lapply(packages, require, character.only=T)

rm(list = ls())

# read histograms ---------------------------------------------------------

# PREVAX 

all_covid_prevax <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/days_t2dm_diag_post_covid_histogram_to_release_prevax.png")
all_covid_prevax <- rasterGrob(all_covid_prevax)

hosp_covid_prevax <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/days_t2dm_diag_post_hosp_covid_histogram_to_release_prevax.png")
hosp_covid_prevax <- rasterGrob(hosp_covid_prevax)

non_hosp_covid_prevax <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/days_t2dm_diag_post_non_hosp_covid_histogram_to_release_prevax.png")
non_hosp_covid_prevax <- rasterGrob(non_hosp_covid_prevax)

# VAX

all_covid_vax <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/days_t2dm_diag_post_covid_histogram_to_release_vax.png")
all_covid_vax <- rasterGrob(all_covid_vax)

hosp_covid_vax <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/days_t2dm_diag_post_hosp_covid_histogram_to_release_vax.png")
hosp_covid_vax <- rasterGrob(hosp_covid_vax)

non_hosp_covid_vax <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/days_t2dm_diag_post_non_hosp_covid_histogram_to_release_vax.png")
non_hosp_covid_vax <- rasterGrob(non_hosp_covid_vax)

# UNVAX

all_covid_unvax <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/days_t2dm_diag_post_covid_histogram_to_release_unvax.png")
all_covid_unvax <- rasterGrob(all_covid_unvax)

hosp_covid_unvax <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/days_t2dm_diag_post_hosp_covid_histogram_to_release_unvax.png")
hosp_covid_unvax <- rasterGrob(hosp_covid_unvax)

non_hosp_covid_unvax <- readPNG("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/days_t2dm_diag_post_non_hosp_covid_histogram_to_release_unvax.png")
non_hosp_covid_unvax <- rasterGrob(non_hosp_covid_unvax)

# COMBINE AND PLOT --------------------------------------------------------

png(paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/generated-figures/histograms_combined.png"),
    units = "mm", width=160, height=110, res = 1000)
grid.arrange(arrangeGrob(all_covid_prevax,
                         hosp_covid_prevax,
                         non_hosp_covid_prevax,
                         top=textGrob("Pre-vaccination cohort", gp = gpar(fontsize = 5)),   
                         ncol=1),
             arrangeGrob(all_covid_vax,
                         hosp_covid_vax,
                         non_hosp_covid_vax,
                         top=textGrob("Vaccinated cohort", gp = gpar(fontsize = 5)),   
                         ncol=1),
             arrangeGrob(all_covid_unvax,
                         hosp_covid_unvax,
                         non_hosp_covid_unvax,
                         top=textGrob("Unvaccinated cohort", gp = gpar(fontsize = 5)),   
                         ncol=1),
             ncol = 3)
dev.off()

