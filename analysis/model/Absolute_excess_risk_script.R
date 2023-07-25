# Load libraries ---------------------------------------------------------------
print('Load libraries')

library(tidyverse)
library(purrr)
library(data.table)
library(tidyverse)

# Specify parameters -----------------------------------------------------------
print('Specify parameters')

release <- "~/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/"
analysis <- "main"

# Source function --------------------------------------------------------------
print('Source function')

source("analysis/model/Absolute_excess_risk_function.R")

# Load model output ------------------------------------------------------------
print('Load model output')

input <- read_csv(paste0(release,"model/master_hr_file.csv"))

# DIABETES SPECIFIC ------------------------------------------------------------
print('DIABETES SPECIFIC')

input <- input[input$time_points == "reduced" & 
                 ((input$event == "t2dm" & input$cohort %in% c("vax","unvax")) | 
                    (input$event == "t2dm_extended_follow_up" & input$cohort %in% c("prevax"))),]

input$event <- "t2dm"
input[,c("time_points","source")] <- NULL

input <- dplyr::rename(input, 
                       "analysis" = "subgroup",
                       "outcome" = "event",
                       "hr" = "estimate")


# Format and restrict to relevant models ---------------------------------------
print('Restrict to relevant models')

input <- input[,c("outcome","cohort","analysis","model","term","hr")]

input <- input[stringr::str_detect(input$term, "^days") &
                 input$analysis==analysis &
                 input$model=="mdl_max_adj" & 
                 input$hr!="[Redacted]" & 
                 !is.na(input$hr),]

# Add start and end for time periods to model output ---------------------------
print('Add start and end for time periods to model output')

input$time_period_start <- as.numeric(gsub("_.*", "",gsub("days", "",input$term)))
input$time_period_end <- as.numeric(gsub(".*_", "",input$term))

# Load AER input ---------------------------------------------------------------
print('Load AER input')

aer_input <- read.csv(paste0(release,"descriptive/table2_prevax_diabetes.csv"))
aer_input <- aer_input[aer_input$event=="out_date_t2dm_extended_follow_up",]

for (cohort in c("vax","unvax")) {
  tmp <- read.csv(paste0(release,paste0("descriptive/table2_",cohort,"_diabetes.csv")))
  tmp <- tmp[tmp$event=="out_date_t2dm",]
  aer_input  <- rbind(aer_input, tmp)
}

aer_input$analysis <- analysis

aer_input <- dplyr::rename(aer_input, 
                           "cohort" = "cohort_to_run",
                           "outcome" = "event",
                           "unexposed_events" = "unexposed_event_count",
                           "total_exposed" = "total_covid19_cases",
                           "sample_size" = "N_population_size")

aer_input$outcome <- gsub("out_date_","",aer_input$outcome)

aer_input <- aer_input[startsWith(aer_input$subgroup, "aer_"),]
aer_input$outcome <- gsub("_extended_follow_up","",aer_input$outcome)

aer_input$aer_sex <- gsub("_.*","",gsub("aer_","",aer_input$subgroup))
aer_input$aer_age <- gsub("aer_.*ale_","",aer_input$subgroup)
  
aer_input <- aer_input[,c("aer_sex", 
                          "aer_age", 
                          "analysis",
                          "outcome", 
                          "cohort",
                          "unexposed_person_days",
                          "unexposed_events",
                          "total_exposed", 
                          "sample_size")]

# Run AER function -------------------------------------------------------------
print('Run AER function')

lifetables_compiled <- NULL

for (i in 1:nrow(aer_input)) {
  
  tmp <- lifetable(model_output = input,
                   aer_input = aer_input[i,])
  
  lifetables_compiled <- rbind(lifetables_compiled, tmp)
  
}

# Calculate prevax weightings --------------------------------------------------
print('Calculate prevax weightings')

prevax_weightings <- aer_input[aer_input$cohort=="prevax",
                               c("analysis",
                                 "outcome",
                                 "aer_sex", 
                                 "aer_age", 
                                 "sample_size")]

prevax_weightings$weight <- prevax_weightings$sample_size/sum(prevax_weightings$sample_size)
prevax_weightings$sample_size <- NULL

# Calculate overall AER --------------------------------------------------------
print('Calculate overall AER')

lifetable_overall <- lifetables_compiled[,c("analysis","outcome","cohort","days",
                                            "aer_age","aer_sex",
                                            "cumulative_difference_absolute_excess_risk")]

lifetable_overall <- merge(lifetable_overall, prevax_weightings,
                           by=c("analysis","outcome","aer_sex","aer_age"))

lifetable_overall <- lifetable_overall %>% 
  dplyr::group_by(analysis, outcome, cohort, days) %>%
  dplyr::mutate(cumulative_difference_absolute_excess_risk = weighted.mean(cumulative_difference_absolute_excess_risk,weight)) %>%
  dplyr::ungroup() %>%
  dplyr::select(analysis, outcome, cohort, days, cumulative_difference_absolute_excess_risk) %>%
  unique

lifetable_overall$aer_sex <- "overall"
lifetable_overall$aer_age <- "overall"

# Compile aer_group and overall life tables -------------------------------------
print('Compile aer_group and overall life tables')

lifetables_compiled <- lifetables_compiled[,c("analysis","outcome","cohort","days",
                                              "aer_age","aer_sex",
                                              "cumulative_difference_absolute_excess_risk")]

lifetables_compiled <- rbind(lifetables_compiled, lifetable_overall)

# Save compiled life tables ----------------------------------------------------
print('Save compiled life tables')

write.csv(lifetables_compiled, 
          "output/lifetables_compiled.csv", 
          row.names = FALSE)