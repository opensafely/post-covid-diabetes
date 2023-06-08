## =============================================================================
# Additional analysis specific to post-covid diabetes project
# Using data generated in the "generate_study_population_diabetes_analysis" study definition
# Aim: Investigate how many of those diagnosed with type 2 diabetes following a covid-19 infection were still being treated or had elevated HbA1c levels
## =============================================================================

############################################
# Load relevant libraries and read in data #
############################################

#Load libraries using pacman
pacman::p_load(dplyr,tictoc,readr,stringr,tidyr,ggplot2,jsonlite,here,arrow,lubridate)

#clear memory
rm(list=ls())

# Create persistent diabetes outcomes
# Creates 4 additional outcomes
# out_date_t2dm_follow which is Persistent diabetes
# out_date_t2dm_follow_extended_follow_up which is Persistent diabetes with extended follow up
  
# out_date_t2dm_4months_follow_up_no_cox_only_table_2 which is type 2 diabetes where patients have 4 months of follow up
# out_date_t2dm_4months_follow_up_no_cox_only_table_2_extended_follow_up which is type 2 diabetes where patients have 4 months of follow up using etended follow up end dates
# The above two outcomes are ONLY for table 2 where event counts are calculated and are NOT to be run with the cox model
  
tmp <- arrow::read_feather(file = paste0("output/input_prevax_diabetes_analysis.feather"))
tmp <- tmp %>% select(patient_id,out_count_insulin_snomed_4mnths, out_count_antidiabetic_drugs_snomed_4mnths, 
                        out_count_nonmetform_drugs_snomed_4mnths,out_num_max_hba1c_mmol_4mnths)

input <- readr::read_rds(file.path("output", paste0("input_prevax_stage1_diabetes.rds")))
input[c("has_4mth_follow_up","has_4mth_follow_up_extended","out_date_t2dm_follow","out_date_t2dm_follow_extended_follow_up" )] <- NULL
input <- input %>% left_join(tmp, by = c("patient_id"="patient_id"))
rm(tmp)

input$cohort_end_date <- as.Date("2021-06-18")
input$cohort_end_date_extended <- as.Date("2021-12-14")

input$follow_up_end <- apply(input[,c("death_date", "dereg_date", "cohort_end_date")],1, min,na.rm=TRUE)
input$follow_up_end_extended <- apply(input[,c("death_date", "dereg_date", "cohort_end_date_extended")],1, min,na.rm=TRUE)

# Get those with 4 months follow up (those with an end date >= 4 months from t2dm)
input <- input %>% 
  rowwise() %>%
  mutate(start_end_diff = as.numeric(difftime(follow_up_end, out_date_t2dm, units = "days"))) %>%
  ungroup() %>%
  mutate(start_end_diff_months = start_end_diff/30.417) %>%
  mutate(has_4mth_follow_up = ifelse(start_end_diff_months < 4 | is.na(start_end_diff_months), FALSE, TRUE))

input <- input %>% 
  rowwise() %>%
  mutate(start_end_diff_extended = as.numeric(difftime(follow_up_end_extended, out_date_t2dm_extended_follow_up, units = "days"))) %>%
  ungroup() %>%
  mutate(start_end_diff_months_extended = start_end_diff_extended/30.417) %>%
  mutate(has_4mth_follow_up_extended = ifelse(start_end_diff_months_extended < 4 | is.na(start_end_diff_months_extended), FALSE, TRUE))

# Those that were followed up and still being prescribed medication or had elevated HbA1c

input <- input %>%
  # create total N prescriptions variable
  rowwise() %>%
  mutate(total_prescriptions = sum(out_count_insulin_snomed_4mnths, out_count_antidiabetic_drugs_snomed_4mnths, out_count_nonmetform_drugs_snomed_4mnths)) %>%
  ungroup() %>%
  mutate(persistent_diabetes = ifelse(has_4mth_follow_up == TRUE & (out_num_max_hba1c_mmol_4mnths >= 47.5), TRUE,
                                      ifelse(has_4mth_follow_up == TRUE & (total_prescriptions >= 2), TRUE, FALSE)))

input <- input %>%
  # create total N prescriptions variable
  rowwise() %>%
  mutate(total_prescriptions_extended = sum(out_count_insulin_snomed_4mnths, out_count_antidiabetic_drugs_snomed_4mnths, out_count_nonmetform_drugs_snomed_4mnths)) %>%
  ungroup() %>%
  mutate(persistent_diabetes_extended_follow_up = ifelse(has_4mth_follow_up_extended == TRUE & (out_num_max_hba1c_mmol_4mnths >= 47.5), TRUE,
                                                         ifelse(has_4mth_follow_up_extended == TRUE & (total_prescriptions_extended >= 2), TRUE, FALSE)))

input$out_date_t2dm_follow <- as.Date(NA)
input$out_date_t2dm_follow[which(input$persistent_diabetes == TRUE)] <- input$out_date_t2dm[which(input$persistent_diabetes == TRUE)]

input$out_date_t2dm_follow_extended_follow_up <- as.Date(NA)
input$out_date_t2dm_follow_extended_follow_up[which(input$persistent_diabetes_extended_follow_up == TRUE)] <- input$out_date_t2dm[which(input$persistent_diabetes_extended_follow_up == TRUE)]

input[c("out_count_insulin_snomed_4mnths", "out_count_antidiabetic_drugs_snomed_4mnths",
        "out_count_nonmetform_drugs_snomed_4mnths","out_num_max_hba1c_mmol_4mnths","start_end_diff","start_end_diff_months",
        "start_end_diff_extended","start_end_diff_months_extended","total_prescriptions","total_prescriptions_extended",
        "persistent_diabetes","persistent_diabetes_extended_follow_up","cohort_end_date","cohort_end_date_extended",
        "follow_up_end","follow_up_end_extended")] <- NULL

# SAVE input file with new diabetes outcome added

saveRDS(input, file = file.path("output", paste0("input_prevax_stage1_diabetes.rds"))) 
  


