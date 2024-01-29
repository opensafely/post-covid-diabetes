# Load libraries ---------------------------------------------------------------
print('Load libraries')

library(readr)
library(dplyr)
library(magrittr)

# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Specify arguments ------------------------------------------------------------
print('Specify arguments')

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  cohort <- "prevax"
} else {
  cohort <- args[[1]]
}

# Load table 2 -----------------------------------------------------------------
print('Load table 2')

table2 <- readr::read_csv(paste0("output/table2_",cohort,".csv"))

# Perform redaction ------------------------------------------------------------
print('Perform redaction')

table2$sample_size_midpoint6 <- roundmid_any(as.numeric(table2$sample_size), threshold)
table2$day0_events_midpoint6 <- roundmid_any(as.numeric(table2$day0_events), threshold)
table2$total_exposed_midpoint6 <- roundmid_any(as.numeric(table2$total_exposed), threshold)
table2$unexposed_events_midpoint6 <- roundmid_any(as.numeric(table2$unexposed_events), threshold)
table2$exposed_events_midpoint6 <- roundmid_any(as.numeric(table2$exposed_events), threshold)
table2$total_events_midpoint6_derived <- table2$unexposed_events_midpoint6 + table2$exposed_events_midpoint6

table2 <- table2[,c("name",
                    "cohort",
                    "exposure",
                    "outcome",
                    "analysis",
                    "unexposed_person_days",
                    "unexposed_events_midpoint6",
                    "exposed_person_days",
                    "exposed_events_midpoint6",
                    "total_person_days",
                    "total_events_midpoint6_derived",
                    "day0_events_midpoint6",
                    "total_exposed_midpoint6",
                    "sample_size_midpoint6",
                    "fup_median_iqr")]

# Save Table 2 -----------------------------------------------------------------
print('Save rounded Table 2')

write.csv(table2, paste0("output/table2_",cohort,"_midpoint6.csv"), row.names = FALSE)