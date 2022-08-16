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
  df <- df %>% 
    mutate_at(c("Whole_population","COVID_exposed","COVID_hospitalised", "COVID_non_hospitalised"), as.numeric) %>%
    mutate(COVID_risk_per_100k = (COVID_exposed/Whole_population)*100000) %>%
    mutate_if(is.numeric, round, 0) %>%
    mutate("Number diagnosed with COVID-19 (risk per 100,000)" = paste0(COVID_exposed, " (", COVID_risk_per_100k, ")"),
           "Covariate Level" = Covariate_level,
           "Whole Population" = Whole_population) %>%
    dplyr::select("Covariate", "Covariate Level", "Whole Population", "Number diagnosed with COVID-19 (risk per 100,000)")
}

# GET OUTCOME GROUPS

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>% filter(active == TRUE)
outcome_groups <- unique(active_analyses$outcome_group)

# READ IN AND FORMAT THE TABLE 1 FOR EACH OUTCOME GROUP

for(group in outcome_groups){
  table1_prevax <- read.csv(paste0("output/review/descriptives/Table1_prevax_without_covid_history_",group,".csv"))
  table1_vax <- read.csv(paste0("output/review/descriptives/Table1_vax_without_covid_history_",group,".csv"))
  table1_unvax <- read.csv(paste0("output/review/descriptives/Table1_unvax_without_covid_history_",group,".csv"))
  
  table1_prevax_format <- clean_table_1(table1_prevax)
  table1_vax_format <- clean_table_1(table1_vax)
  table1_unvax_format <- clean_table_1(table1_unvax)

  # CONSTRUCT MAIN TABLE 1
  
  colnames(table1_prevax_format)[3:4] <- paste(colnames(table1_prevax_format)[3:4], "prevax", sep = "_")
  colnames(table1_vax_format)[3:4] <- paste(colnames(table1_vax_format)[3:4], "vax", sep = "_")
  colnames(table1_unvax_format)[3:4] <- paste(colnames(table1_unvax_format)[3:4], "unvax", sep = "_")
  
  table1_merged <- full_join(table1_vax_format, table1_unvax_format)
  table1_merged <- full_join(table1_merged, table1_prevax_format)
  
  # SAVE TABLE 1
  
  write.csv(table1_merged, paste0("output/review/descriptives/Table1_",group,"_Formatted_To_Release.csv"), row.names = FALSE)
}
  