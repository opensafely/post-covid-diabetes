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

input <- input[,c("event","cohort","subgroup","model","time_points","term","estimate")]

input <- dplyr::rename(input, 
                       "analysis" = "subgroup")

# Restrict to relevant models --------------------------------------------------
print('Restrict to relevant models')

input <- input[str_detect(input$term, "^days") &
                 input$analysis==analysis &
                 input$model=="mdl_max_adj" & 
                 input$estimate!="[Redacted]" & 
                 !is.na(input$estimate),]

# Load AER input ---------------------------------------------------------------
print('Load AER input')

aer_input <- read.csv(paste0(release,"descriptive/table2_prevax_diabetes.csv"))
aer_input <- aer_input[aer_input$event=="out_date_t2dm_extended_follow_up",]

for (cohort in c("vax","unvax")) {
  tmp <- read.csv(paste0(release,paste0("descriptive/table2_",cohort,"_diabetes.csv")))
  tmp <- tmp[tmp$event=="out_date_t2dm",]
  aer_input  <- rbind(aer_input, tmp)
}

aer_input <- dplyr::rename(aer_input, 
                           "cohort" = "cohort_to_run")

aer_input <- aer_input[,c("subgroup", 
                          "event", 
                          "cohort",
                          "unexposed_person_days",
                          "unexposed_event_count",
                          "total_covid19_cases", 
                          "N_population_size")]

aer_input$event <- gsub("out_date_","",aer_input$event)

aer_input$analysis <- analysis

# DIABETES SPECIFIC ------------------------------------------------------------
print('DIABETES SPECIFIC')

input <- input[input$time_points == "reduced" & 
                 ((input$event == "t2dm" & input$cohort %in% c("vax","unvax")) | 
                    (input$event == "t2dm_extended_follow_up" & input$cohort %in% c("prevax"))),]

input$event <- "t2dm"

aer_input <- aer_input[startsWith(aer_input$subgroup, "aer_"),]
aer_input$event <- gsub("_extended_follow_up","",aer_input$event)

# Merge and format AER input and model output ----------------------------------
print('Merge and format AER input and model output')

results <- merge(input, aer_input, by=c("event","cohort","analysis"))

results <- results %>% 
  mutate(across(c(estimate, 
                  unexposed_person_days, 
                  unexposed_event_count, 
                  total_covid19_cases), 
                as.numeric))

# Run AER function -------------------------------------------------------------
print('Run AER function')

AER_compiled_results <- NULL

for (i in 1:nrow(results)) {
  
  tmp <- excess_risk(event_of_interest = results$event[i],
                     cohort_of_interest = results$cohort[i],
                     model_of_interest = results$model[i],
                     subgroup_of_interest = results$subgroup[i],
                     time_point_of_interest = results$time_points[i],
                     input = results)
  
  AER_compiled_results <- rbind(AER_compiled_results, tmp)
  
}

# Calculate prevax weightings --------------------------------------------------
print('Calculate prevax weightings')

prevax_weightings <- aer_input[aer_input$cohort=="prevax",
                               c("event", 
                                 "cohort", 
                                 "subgroup", 
                                 "N_population_size")]

prevax_weightings$weight <- prevax_weightings$N_population_size/sum(prevax_weightings$N_population_size)

# Calculate overall AER --------------------------------------------------------
print('Calculate overall AER')

AER_overall <- AER_compiled_results[,c("days", 
                                       "event", 
                                       "cohort", 
                                       "model",
                                       "subgroup", 
                                       "time_points", 
                                       "cumulative_difference_absolute_excess_risk")]

AER_overall <- merge(AER_overall, 
                     prevax_weightings[,c("event","subgroup","weight")], 
                     by=c("event","subgroup"))

AER_overall <- AER_overall %>% 
  dplyr::group_by(days, event, cohort,time_points) %>%
  dplyr::mutate(cumulative_difference_absolute_excess_risk = weighted.mean(cumulative_difference_absolute_excess_risk,weight)) %>%
  dplyr::ungroup() %>%
  dplyr::select(days, event, cohort, model, time_points, cumulative_difference_absolute_excess_risk)

AER_overall$subgroup <- "aer_overall"

# Compile group and overall AER ------------------------------------------------
print('Compile group and overall AER')

AER_compiled_results <- plyr::rbind.fill(AER_compiled_results, AER_overall)

# Save -------------------------------------------------------------------------
print('Save')

write.csv(AER_compiled_results, 
          "output/AER_compiled_results.csv", 
          row.names = FALSE)