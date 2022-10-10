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

input <- input %>%
  dplyr::filter( !is.na(out_date_t2dm) &  !is.na(exp_date_covid19_confirmed)) %>%
  # keep those where t2dm is after infection
  rowwise() %>%
  mutate(keep = ifelse(out_date_t2dm < exp_date_covid19_confirmed, FALSE, TRUE)) %>%
  ungroup() %>%
  dplyr::filter(keep == TRUE) %>%
  dplyr::select(-c(keep))

# summarise df
summary(input)

# calculate new end date

input$cohort_end_date <- cohort_end_date
input$end_date <- apply(input[,c("death_date", "dereg_date", "cohort_end_date")],1, min,na.rm=TRUE)
input$end_date <- as.Date(input$end_date)

# Get N with 4 months follow up (those with an end date >= 4 months from t2dm)

input <- input %>% 
  rowwise() %>%
  mutate(start_end_diff = as.numeric(difftime(end_date, out_date_t2dm, units = "days"))) %>%
  ungroup() %>%
  mutate(start_end_diff_months = start_end_diff/30.417) %>%
  mutate(follow_4mth = ifelse(start_end_diff_months >= 4, TRUE, FALSE))

# summarise df
summary(input)

# N of those that were followed up and still being prescribed medication or had elevated HbA1c

input <- input %>%
  # create total N prescriptions variable
  rowwise() %>%
  mutate(total_prescriptions = sum(out_count_insulin_snomed, out_count_antidiabetic_drugs_snomed, out_count_nonmetform_drugs_snomed)) %>%
  ungroup() %>%
  mutate(N_follow_prescribe = ifelse(follow_4mth == TRUE & (out_num_max_hba1c_mmol_4mnths >= 47.5), TRUE,
                                     ifelse(follow_4mth == TRUE & (total_prescriptions >= 2), TRUE, FALSE)))

# summarise df
summary(input)

# COMPLETE RESULTS TABLE for output
# Make a results df
# A table with the following columns (as per protocol): 
# (i) N type 2 diabetes cases following any COVID-19 infection
# (ii) hosp COVID-19 infection
# (iii) non-hosp COVID-19 infection
# (iv) N that were included in the 4-month follow-up analysis and 
# (v) N of those that were followed up and still being prescribed medication or had elevated HbA1c.  

results <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("N_t2dm_any", "N_t2dm_hosp", "N_t2dm_non_hosp", "N_included_4mth", "N_still_treated"))

results[1,1] <- nrow(input)
results[1,2] <- sum(input$sub_cat_covid19_hospital=="hospitalised")
results[1,3] <- sum(input$sub_cat_covid19_hospital=="non_hospitalised")
results[1,4] <- sum(input$follow_4mth==TRUE)
results[1,5] <- sum(input$N_follow_prescribe==TRUE)
results$cohort <- cohort_name

# SAVE

readr::write_csv(results, paste0("output/review/descriptives/diabetes_posthoc_analysis_res_",cohort_name,".csv"))

}

# RUN FUNCTION WITH COMMAND ARGS

if (cohort_name == "all") {
  diabetes_post_hoc("prevax")
  diabetes_post_hoc("vax")
  diabetes_post_hoc("unvax")
} else{
  diabetes_post_hoc(cohort_name)
}

# END