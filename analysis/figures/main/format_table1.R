# CREATE COMBINED (3 COHORT) TABLE 1A FOR POST-COVID MANUSCRIPTS
# NEEDS: Output from "Stage2_missing_table1.R script

###############################################
# 0. Load relevant libraries and read in data #
###############################################

library(readr)
library(dplyr)
library(data.table)
library(tidyverse)

fs::dir_create(here::here("output", "review", "descriptives"))

###############################################
# 1. CLEAN TABLE 1 FUNCTION
###############################################

clean_table_1 <- function(df) {
  total_n <- df[df$Covariate_level == "All", "Whole_population"]
  df <- df %>% 
    mutate_at(c("Whole_population","COVID_exposed","COVID_hospitalised", "COVID_non_hospitalised"), as.numeric) %>%
    mutate(COVID_risk_per_100k = (COVID_exposed/Whole_population)*100000) %>%
    # add percentages
    mutate(perc = (Whole_population / total_n) * 100) %>%
    mutate(perc = round(perc, digits = 1)) %>%
    mutate_at(c("Whole_population", "COVID_exposed", "COVID_hospitalised", "COVID_non_hospitalised", "COVID_risk_per_100k"), round, 0) %>%
    mutate(Whole_population = paste0(format(Whole_population, big.mark = ",", scientific = FALSE), " (", perc, ")")) %>%
    mutate(COVID_exposed = str_trim(format(COVID_exposed, big.mark = ",", scientific = FALSE)),
           COVID_risk_per_100k = str_trim(format(COVID_risk_per_100k, big.mark = ",", scientific = FALSE))) %>%
    mutate("Number diagnosed with COVID-19 (risk per 100,000)" = paste0(COVID_exposed, " (", COVID_risk_per_100k, ")"),
           "Covariate Level" = Covariate_level,
           "Whole Population" = Whole_population) %>%
    dplyr::select("Covariate", "Covariate Level", "Whole Population", "Number diagnosed with COVID-19 (risk per 100,000)") %>%
    mutate(across(where(is.character), str_trim)) %>%
    # Tidy mumeric vars
    mutate(`Number diagnosed with COVID-19 (risk per 100,000)` = ifelse(`Covariate Level` == "Mean",
                                                                        gsub(r"{\s*\([^\)]+\)}","",as.character(`Number diagnosed with COVID-19 (risk per 100,000)`)),
                                                                        as.character(`Number diagnosed with COVID-19 (risk per 100,000)`)),
           `Whole Population` = ifelse(`Covariate Level` == "Mean",
                                       gsub(r"{\s*\([^\)]+\)}","",as.character(`Whole Population`)),
                                       as.character(`Whole Population`)))
}

# GET OUTCOME GROUPS

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>% filter(active == TRUE)
outcome_groups <- unique(active_analyses$outcome_group)

# only run table 1 for diabetes outcome group - comment this line out if table 1 is needed for all subgroups.
outcome_groups <- "diabetes"
# READ IN AND FORMAT THE TABLE 1 FOR EACH OUTCOME GROUP

for(group in outcome_groups){
  table1_prevax <- read.csv(paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/descriptive/Table1_prevax_without_covid_history_",group,".csv"))
  table1_vax <- read.csv(paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/descriptive/Table1_vax_without_covid_history_",group,".csv"))
  table1_vax <- table1_vax[ order(match(table1_vax$Covariate_level, table1_prevax$Covariate_level)), ]
  table1_unvax <- read.csv(paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/descriptive/Table1_unvax_without_covid_history_",group,".csv"))
  
  table1_prevax_format <- clean_table_1(table1_prevax)
  table1_vax_format <- clean_table_1(table1_vax)
  table1_unvax_format <- clean_table_1(table1_unvax)
  
  # CONSTRUCT MAIN TABLE 1
  
  colnames(table1_prevax_format)[3:4] <- paste(colnames(table1_prevax_format)[3:4], "prevax", sep = "_")
  colnames(table1_vax_format)[3:4] <- paste(colnames(table1_vax_format)[3:4], "vax", sep = "_")
  colnames(table1_unvax_format)[3:4] <- paste(colnames(table1_unvax_format)[3:4], "unvax", sep = "_")
  
  table1_merged <- full_join(table1_prevax_format, table1_vax_format)
  table1_merged <- full_join(table1_merged, table1_unvax_format)
  
  # RENAME COLUMN NAMES 
  
  table1_merged <- table1_merged %>%
    dplyr::rename("Prevaccination Cohort (1 Jan 2020 to 18 June 2021)" = "Whole Population_prevax",
                  "Prevaccination N Diagnosed with COVID-19 (risk per 100,000)" = "Number diagnosed with COVID-19 (risk per 100,000)_prevax",
                  "Vaccinated Cohort (1 June to 14 Dec 2021)" = "Whole Population_vax",
                  "Vaccinated N Diagnosed with COVID-19 (risk per 100,000)" = "Number diagnosed with COVID-19 (risk per 100,000)_vax",
                  "Unvaccinated Cohort (1 June to 14 Dec 2021)" = "Whole Population_unvax",
                  "Unvaccinated N Diagnosed with COVID-19 (risk per 100,000)" = "Number diagnosed with COVID-19 (risk per 100,000)_unvax")
  
  
  
  # SAVE TABLE 1
  
  write.csv(table1_merged, paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/generated-figures/Table1_Formatted_Diabetes.csv"), row.names = FALSE)
}
