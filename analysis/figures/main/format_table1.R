# CREATE COMBINED (3 COHORT) TABLE 1A FOR POST-COVID MANUSCRIPTS
# NEEDS: Output from "Stage2_missing_table1.R script

###############################################
# 0. Load relevant libraries and read in data #
###############################################

library(readr)
library(dplyr)
library(data.table)
library(tidyverse)

release <- "/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/"  

# Data

table1_prevax <- read.csv(paste0(release,"descriptive/Table1_prevax_without_covid_history_diabetes.csv"))
table1_vax <- read.csv(paste0(release,"descriptive/Table1_vax_without_covid_history_diabetes.csv"))
table1_unvax <- read.csv(paste0(release,"descriptive/Table1_unvax_without_covid_history_diabetes.csv"))

# Additional variables
cohort_prevax <- "prevax"
table1_prevax$cohort <- cohort_prevax
total_prevax <- filter(table1_prevax, Covariate=="All")$Whole_population
table1_prevax$total <- total_prevax

cohort_vax <- "vax"
table1_vax$cohort <- cohort_vax
total_vax <- filter(table1_vax, Covariate=="All")$Whole_population
table1_vax$total <- total_vax

cohort_unvax <- "unvax"
table1_unvax$cohort <- cohort_unvax
total_unvax <- filter(table1_unvax, Covariate=="All")$Whole_population
table1_unvax$total <- total_unvax

df <- rbind(table1_prevax, table1_vax, table1_unvax)
###############################################
# Calculate column percentages -------------------------------------------------

df$Npercent <- paste0(df$Whole_population,ifelse(df$Covariate_level=="All","",
                                      paste0(" (",round(100*(df$Whole_population / df$total),1),"%)")))

df <- df[,c("Covariate","Covariate_level","Npercent","COVID_exposed", "cohort")]
colnames(df) <- c("Characteristic","Subcharacteristic","N (%)","COVID-19 diagnoses", "cohort")

# Pivot table ------------------------------------------------------------------
print("Pivot table")

df <- tidyr::pivot_wider(df, 
                         names_from = "cohort",
                         values_from = c("N (%)","COVID-19 diagnoses"))

# Tidy table -------------------------------------------------------------------
print("Tidy table")

df <- df[,c("Characteristic","Subcharacteristic",
            paste0(c("N (%)","COVID-19 diagnoses"),"_prevax"),
            paste0(c("N (%)","COVID-19 diagnoses"),"_vax"),
            paste0(c("N (%)","COVID-19 diagnoses"),"_unvax"))]

# Save file=------------------------------------------------------------

write.csv(df, paste0(release, "generated-tables/Table1.csv"), row.names = FALSE)

