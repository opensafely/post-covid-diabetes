## =============================================================================
# Additional analysis specific to post-covid diabetes project
# Using data generated in the "generate_study_population_diabetes_analysis" study definition
# Aim: Investigate how many of those diagnosed with type 2 diabetes following a covid-19 infection were still being treated or had elevated HbA1c levels
## =============================================================================

############################################
# Load relevant libraries and read in data #
############################################

#Load libraries using pacman
pacman::p_load(dplyr,tictoc,readr,stringr,tidyr,ggplot2,jsonlite,here,arrow)

#clear memory
rm(list=ls())

# Specify command arguments ----------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort_name <- "prevax"
} else {
  cohort_name <- args[[1]]
}

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review", "descriptives"))

# STUDY END DATES

study_dates <- jsonlite::fromJSON("output/study_dates.json")

if (cohort_name %in% c("vax","unvax"))
{
  #These are the study start and end dates for the Delta era
  cohort_start_date <- as.Date(study_dates$delta_date)
  cohort_end_date <- as.Date(study_dates$omicron_date)
}else if (cohort_name == "prevax") {
  cohort_start_date <- as.Date(study_dates$pandemic_start)
  cohort_end_date <- as.Date(study_dates$all_eligible)
}

###############################################
# DEFINE FUNCTION -----------------------------
###############################################

diabetes_post_hoc <- function(cohort_name){
  
# Load relevant data
input <- arrow::read_feather(file = paste0("output/input_",cohort_name,"_diabetes_analysis.feather"))

# summarise df
summary(input)

# Restrict data only to those that had a diagnosis of type 2 diabetes following a COVID-19 infection

input_4 <- input %>%
  dplyr::filter( !is.na(out_date_t2dm) &  !is.na(exp_date_covid19_confirmed)) %>%
  # keep those where t2dm is after infection
  rowwise() %>%
  mutate(keep = ifelse(out_date_t2dm < exp_date_covid19_confirmed, FALSE, TRUE)) %>%
  ungroup() %>%
  dplyr::filter(keep == TRUE) %>%
  dplyr::select(-c(keep))

# summarise df
summary(input_4)

# calculate new end date

input_4$cohort_end_date <- cohort_end_date
input_4$end_date <- apply(input_4[,c("death_date", "dereg_date", "cohort_end_date")],1, min,na.rm=TRUE)
input_4$end_date <- as.Date(input_4$end_date)

# Get N with 4 months follow up (those with an end date >= 4 months from t2dm)

input_4 <- input_4 %>% 
  rowwise() %>%
  mutate(start_end_diff = as.numeric(difftime(end_date, out_date_t2dm, units = "days"))) %>%
  ungroup() %>%
  mutate(start_end_diff_months = start_end_diff/30.417) %>%
  mutate(follow_4mth = ifelse(start_end_diff_months >= 4, TRUE, FALSE))
  
  # ADD 12 MONTHS

# summarise df
summary(input_4)

# N of those that were followed up and still being prescribed medication or had elevated HbA1c

input_4 <- input_4 %>%
  # create total N prescriptions variable
  rowwise() %>%
  mutate(total_prescriptions = sum(out_count_insulin_snomed_4mnths, out_count_antidiabetic_drugs_snomed_4mnths, out_count_nonmetform_drugs_snomed_4mnths)) %>%
  ungroup() %>%
  mutate(N_follow_prescribe = ifelse(follow_4mth == TRUE & (out_num_max_hba1c_mmol_4mnths >= 47.5), TRUE,
                                     ifelse(follow_4mth == TRUE & (total_prescriptions >= 2), TRUE, FALSE)))

# summarise df
summary(input_4)

# GET RESULTS FOR COVID HOSP / NON-HOSP

input_hosp_4 <- input_4 %>%
  dplyr::filter(sub_cat_covid19_hospital == "hospitalised")

input_nonhosp_4 <- input_4 %>%
  dplyr::filter(sub_cat_covid19_hospital == "non_hospitalised")

# COMPLETE RESULTS TABLE for output
# Make a results df
# A table with the following columns (as per protocol): 
# (i) N type 2 diabetes cases following any COVID-19 infection
# (ii) hosp COVID-19 infection
# (iii) non-hosp COVID-19 infection
# (iv) N that were included in the 4-month follow-up analysis and 
# (v) N of those that were followed up and still being prescribed medication or had elevated HbA1c.  

results <- setNames(data.frame(matrix(ncol = 9, nrow = 1)), c("N_t2dm_any", "N_t2dm_hosp", "N_t2dm_non_hosp",
                                                              "N_any_COVID_included_4mth", "N_any_COVID_still_treated",
                                                              "N_hosp_COVID_included_4mth", "N_hosp_COVID_still_treated",
                                                              "N_non_hosp_COVID_included_4mth", "N_non_hosp_COVID_still_treated"))

results$N_t2dm_any <- nrow(input_4)
results$N_t2dm_hosp <- sum(input_4$sub_cat_covid19_hospital=="hospitalised")
results$N_t2dm_non_hosp <- sum(input_4$sub_cat_covid19_hospital=="non_hospitalised")
results$N_any_COVID_included_4mth <- sum(input_4$follow_4mth==TRUE)
results$N_any_COVID_still_treated <- sum(input_4$N_follow_prescribe==TRUE)
results$N_hosp_COVID_included_4mth <- sum(input_hosp_4$follow_4mth==TRUE)
results$N_hosp_COVID_still_treated <- sum(input_hosp_4$N_follow_prescribe==TRUE)
results$N_non_hosp_COVID_included_4mth <- sum(input_nonhosp_4$follow_4mth==TRUE)
results$N_non_hosp_COVID_still_treated <- sum(input_nonhosp_4$N_follow_prescribe==TRUE)
results$cohort <- cohort_name

# ADD PERCENTAGES ACCORDING TO PROTOCOL

N_4 <- results$N_any_COVID_included_4mth
N_4_h <- results$N_hosp_COVID_included_4mth
N_4_nh <- results$N_non_hosp_COVID_included_4mth

results$N_any_COVID_included_4mth <- paste0(results$N_any_COVID_included_4mth, " (",round((results$N_any_COVID_included_4mth / results$N_t2dm_any)*100, digits = 2) ,")")
results$N_any_COVID_still_treated <- paste0(results$N_any_COVID_still_treated, " (",round((results$N_any_COVID_still_treated / N_4)*100, digits = 2) ,")")

results$N_hosp_COVID_included_4mth <- paste0(results$N_hosp_COVID_included_4mth, " (",round((results$N_hosp_COVID_included_4mth / results$N_t2dm_hosp)*100, digits = 2) ,")")
results$N_hosp_COVID_still_treated <- paste0(results$N_hosp_COVID_still_treated, " (",round((results$N_hosp_COVID_still_treated / N_4_h)*100, digits = 2) ,")")

results$N_non_hosp_COVID_included_4mth <- paste0(results$N_non_hosp_COVID_included_4mth, " (",round((results$N_non_hosp_COVID_included_4mth / results$N_t2dm_non_hosp)*100, digits = 2) ,")")
results$N_non_hosp_COVID_still_treated <- paste0(results$N_non_hosp_COVID_still_treated, " (",round((results$N_non_hosp_COVID_still_treated / N_4_nh)*100, digits = 2) ,")")

# SAVE

readr::write_csv(results, paste0("output/review/descriptives/diabetes_posthoc_analysis_res_4mnths_",cohort_name,".csv"))

# REPEAT ABOVE BUT FOR 12 MONTHS INSTEAD OF 4 MONTHS FOR PREVAX ONLY ------------------------------

if (cohort_name == "prevax"){

  # Restrict data only to those that had a diagnosis of type 2 diabetes following a COVID-19 infection
  
  input_12 <- input %>%
    dplyr::filter( !is.na(out_date_t2dm) &  !is.na(exp_date_covid19_confirmed)) %>%
    # keep those where t2dm is after infection
    rowwise() %>%
    mutate(keep = ifelse(out_date_t2dm < exp_date_covid19_confirmed, FALSE, TRUE)) %>%
    ungroup() %>%
    dplyr::filter(keep == TRUE) %>%
    dplyr::select(-c(keep))
  
  # summarise df
  summary(input_12)
  
  # calculate new end date
  
  input_12$cohort_end_date <- cohort_end_date
  input_12$end_date <- apply(input_12[,c("death_date", "dereg_date", "cohort_end_date")],1, min,na.rm=TRUE)
  input_12$end_date <- as.Date(input_12$end_date)
  
  # Get N with 12 months follow up (those with an end date >= 12 months from t2dm)
  
  input_12 <- input_12 %>% 
    rowwise() %>%
    mutate(start_end_diff = as.numeric(difftime(end_date, out_date_t2dm, units = "days"))) %>%
    ungroup() %>%
    mutate(start_end_diff_months = start_end_diff/30.417) %>%
    mutate(follow_12mth = ifelse(start_end_diff_months >= 12, TRUE, FALSE))
  
  # summarise df
  summary(input_12)
  
  # N of those that were followed up and still being prescribed medication or had elevated HbA1c
  
  input_12 <- input_12 %>%
    # create total N prescriptions variable
    rowwise() %>%
    mutate(total_prescriptions = sum(out_count_insulin_snomed_12mnths, out_count_antidiabetic_drugs_snomed_12mnths, out_count_nonmetform_drugs_snomed_12mnths)) %>%
    ungroup() %>%
    mutate(N_follow_prescribe = ifelse(follow_12mth == TRUE & (out_num_max_hba1c_mmol_12mnths >= 47.5), TRUE,
                                       ifelse(follow_12mth == TRUE & (total_prescriptions >= 2), TRUE, FALSE)))
  
  # summarise df
  summary(input_12)
  
  # GET RESULTS FOR COVID HOSP / NON-HOSP
  
  input_hosp_12 <- input_12 %>%
    dplyr::filter(sub_cat_covid19_hospital == "hospitalised")
  
  input_nonhosp_12 <- input_12 %>%
    dplyr::filter(sub_cat_covid19_hospital == "non_hospitalised")
  
  # COMPLETE RESULTS TABLE for output
  # Make a results df
  # A table with the following columns (as per protocol): 
  # (i) N type 2 diabetes cases following any COVID-19 infection
  # (ii) hosp COVID-19 infection
  # (iii) non-hosp COVID-19 infection
  # (iv) N that were included in the 4-month follow-up analysis and 
  # (v) N of those that were followed up and still being prescribed medication or had elevated HbA1c.  
  
  results <- setNames(data.frame(matrix(ncol = 9, nrow = 1)), c("N_t2dm_any", "N_t2dm_hosp", "N_t2dm_non_hosp",
                                                                "N_any_COVID_included_12mth", "N_any_COVID_still_treated",
                                                                "N_hosp_COVID_included_12mth", "N_hosp_COVID_still_treated",
                                                                "N_non_hosp_COVID_included_12mth", "N_non_hosp_COVID_still_treated"))
  
  results$N_t2dm_any <- nrow(input_12)
  results$N_t2dm_hosp <- sum(input_12$sub_cat_covid19_hospital=="hospitalised")
  results$N_t2dm_non_hosp <- sum(input_12$sub_cat_covid19_hospital=="non_hospitalised")
  results$N_any_COVID_included_12mth <- sum(input_12$follow_12mth==TRUE)
  results$N_any_COVID_still_treated <- sum(input_12$N_follow_prescribe==TRUE)
  results$N_hosp_COVID_included_12mth <- sum(input_hosp_12$follow_12mth==TRUE)
  results$N_hosp_COVID_still_treated <- sum(input_hosp_12$N_follow_prescribe==TRUE)
  results$N_non_hosp_COVID_included_12mth <- sum(input_nonhosp_12$follow_12mth==TRUE)
  results$N_non_hosp_COVID_still_treated <- sum(input_nonhosp_12$N_follow_prescribe==TRUE)
  results$cohort <- cohort_name
  
  # ADD PERCENTAGES ACCORDING TO PROTOCOL
  
  N_12 <- results$N_any_COVID_included_12mth
  N_12_h <- results$N_hosp_COVID_included_12mth
  N_12_nh <- results$N_non_hosp_COVID_included_12mth
  
  results$N_any_COVID_included_12mth <- paste0(results$N_any_COVID_included_12mth, " (",round((results$N_any_COVID_included_12mth / results$N_t2dm_any)*100, digits = 2) ,")")
  results$N_any_COVID_still_treated <- paste0(results$N_any_COVID_still_treated, " (",round((results$N_any_COVID_still_treated / N_12)*100, digits = 2) ,")")
  
  results$N_hosp_COVID_included_12mth <- paste0(results$N_hosp_COVID_included_12mth, " (",round((results$N_hosp_COVID_included_12mth / results$N_t2dm_hosp)*100, digits = 2) ,")")
  results$N_hosp_COVID_still_treated <- paste0(results$N_hosp_COVID_still_treated, " (",round((results$N_hosp_COVID_still_treated / N_12_h)*100, digits = 2) ,")")
  
  results$N_non_hosp_COVID_included_12mth <- paste0(results$N_non_hosp_COVID_included_12mth, " (",round((results$N_non_hosp_COVID_included_12mth / results$N_t2dm_non_hosp)*100, digits = 2) ,")")
  results$N_non_hosp_COVID_still_treated <- paste0(results$N_non_hosp_COVID_still_treated, " (",round((results$N_non_hosp_COVID_still_treated / N_12_nh)*100, digits = 2) ,")")
  
  # SAVE
  
  readr::write_csv(results, paste0("output/review/descriptives/diabetes_posthoc_analysis_res_12mnths_",cohort_name,".csv"))
  
}

}

# RUN FUNCTION WITH COMMAND ARGS

diabetes_post_hoc(cohort_name)

# END