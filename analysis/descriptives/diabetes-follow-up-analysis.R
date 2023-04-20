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

if (cohort_name %in% c("vax","unvax")){
  #These are the study start and end dates for the Delta era
  cohort_start_date <- as.Date(study_dates$delta_date)
  cohort_end_date <- as.Date(study_dates$omicron_date)
}else if (cohort_name == "prevax") {
  cohort_start_date <- as.Date(study_dates$pandemic_start)
  cohort_end_date <- as.Date(study_dates$all_eligible)
  cohort_end_date_extended <- as.Date("2021-12-14")
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
# This should then give the same number as the Table 2 event counts

input_4 <- input %>%
  dplyr::filter( !is.na(out_date_t2dm) &  !is.na(exp_date_covid19_confirmed)) %>%
  # keep those where t2dm is after infection
  rowwise() %>%
  mutate(keep = ifelse((out_date_t2dm >= index_date_copy) & (out_date_t2dm >= exp_date_covid19_confirmed) & (out_date_t2dm <= t2dm_follow_up_end), TRUE, FALSE)) %>%
  ungroup() %>%
  dplyr::filter(keep == TRUE) %>%
  dplyr::select(-c(keep))

# summarise df
summary(input_4)

# New end date to check follow up 

input_4$cohort_end_date <- cohort_end_date
input_4$end_date <- apply(input_4[,c("death_date", "dereg_date", "cohort_end_date")],1, min,na.rm=TRUE)
input_4$end_date <- as.Date(input_4$end_date)

# Get N with 4 months follow up (those with a new end date >= 4 months from t2dm)

input_4 <- input_4 %>% 
  rowwise() %>%
  mutate(start_end_diff = as.numeric(difftime(end_date, out_date_t2dm, units = "days"))) %>%
  ungroup() %>%
  mutate(start_end_diff_months = start_end_diff/30.417) %>%
  mutate(follow_4mth = ifelse(start_end_diff_months >= 4, TRUE, FALSE))

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

########################################################################
# HISTOGRAM PLOTS ---------------------------------------------------------
########################################################################

# Type 2 diabetes, histograms x axis time-since covid, y-axis no. of events for each cohort, main and stratified by hospitalised status 
# Get time since diagnosis variable 

input_hist <- input_4 %>% 
  mutate(days_t2dm_diag_post_covid = difftime(out_date_t2dm, exp_date_covid19_confirmed, units = "days")) %>%
  mutate(days_t2dm_diag_post_covid = as.numeric(days_t2dm_diag_post_covid))

# Do it for hospitalised as well 

input_hist_hosp <- input_hist %>% 
  dplyr::filter(sub_cat_covid19_hospital == "hospitalised")

# non hosp

input_hist_nonhosp <- input_hist %>% 
  dplyr::filter(sub_cat_covid19_hospital == "non_hospitalised")

# PLOT MAIN

ggplot(input_hist, aes(x=days_t2dm_diag_post_covid)) + geom_histogram(binwidth=1, colour="gray2") +
  ylab("Number of type 2 diabetes events") + xlab("Days between date of confirmed COVID-19 and type 2 diabetes diagnosis") +
  ggtitle("Number of type 2 diabetes events by days since COVID-19 diagnosis (main)") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) 
ggplot2::ggsave(paste0("output/review/descriptives/days_t2dm_diag_post_covid_histogram_", cohort_name,".png"), height = 200, width = 300, unit = "mm", dpi = 600, scale = 1)

# PLOT HOSPITALISED

ggplot(input_hist_hosp, aes(x=days_t2dm_diag_post_covid)) + geom_histogram(binwidth=1, colour="gray2") +
  ylab("Number of type 2 diabetes events") + xlab("Days between date of confirmed COVID-19 and type 2 diabetes diagnosis") +
  ggtitle("Number of type 2 diabetes events by days since COVID-19 diagnosis (hospitalised)") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) 
ggplot2::ggsave(paste0("output/review/descriptives/days_t2dm_diag_post_hosp_covid_histogram_",cohort_name,".png"), height = 200, width = 300, unit = "mm", dpi = 600, scale = 1)

# PLOT NON-HOSPITALISED

ggplot(input_hist_nonhosp, aes(x=days_t2dm_diag_post_covid)) + geom_histogram(binwidth=1, colour="gray2") +
  ylab("Number of type 2 diabetes events") + xlab("Days between date of confirmed COVID-19 and type 2 diabetes diagnosis") +
  ggtitle("Number of type 2 diabetes events by days since COVID-19 diagnosis (non-hospitalised)") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) 
ggplot2::ggsave(paste0("output/review/descriptives/days_t2dm_diag_post_non_hosp_covid_histogram_",cohort_name,".png"), height = 200, width = 300, unit = "mm", dpi = 600, scale = 1)


# Add plots with minimum counts set to 6 ----------------------------------

input_hist_plot <- input_hist %>%
  dplyr::count(bin = floor(days_t2dm_diag_post_covid)) %>%
  mutate(n = pmax(6, n))

input_hist_hosp_plot <- input_hist %>% 
  dplyr::filter(sub_cat_covid19_hospital == "hospitalised") %>%
  dplyr::count(bin = floor(days_t2dm_diag_post_covid)) %>%
  mutate(n = pmax(6, n))

input_hist_nonhosp_plot <- input_hist %>% 
  dplyr::filter(sub_cat_covid19_hospital == "non_hospitalised") %>%
  dplyr::count(bin = floor(days_t2dm_diag_post_covid)) %>%
  mutate(n = pmax(6, n))

ggplot(input_hist_plot, aes(bin, n)) +
  geom_col() +
  ylab("Number of type 2 diabetes events") + xlab("Days between date of confirmed COVID-19 and type 2 diabetes diagnosis") +
  ggtitle("Number of type 2 diabetes events by days since COVID-19 diagnosis (main)") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) 
ggplot2::ggsave(paste0("output/review/descriptives/days_t2dm_diag_post_covid_histogram_to_release_",cohort_name,".png"), height = 200, width = 300, unit = "mm", dpi = 600, scale = 1)

ggplot(input_hist_hosp_plot, aes(bin, n)) +
  geom_col() +
  ylab("Number of type 2 diabetes events") + xlab("Days between date of confirmed COVID-19 and type 2 diabetes diagnosis") +
  ggtitle("Number of type 2 diabetes events by days since COVID-19 diagnosis (hospitalised)") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) 
ggplot2::ggsave(paste0("output/review/descriptives/days_t2dm_diag_post_hosp_covid_histogram_to_release_",cohort_name,".png"), height = 200, width = 300, unit = "mm", dpi = 600, scale = 1)

ggplot(input_hist_nonhosp_plot, aes(bin, n)) +
  geom_col() +
  ylab("Number of type 2 diabetes events") + xlab("Days between date of confirmed COVID-19 and type 2 diabetes diagnosis") +
  ggtitle("Number of type 2 diabetes events by days since COVID-19 diagnosis (non-hospitalised)") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) 
ggplot2::ggsave(paste0("output/review/descriptives/days_t2dm_diag_post_non_hosp_covid_histogram_to_release_",cohort_name,".png"), height = 200, width = 300, unit = "mm", dpi = 600, scale = 1)

###################################################################################################
# REPEAT ABOVE BUT FOR 12 MONTHS INSTEAD OF 4 MONTHS FOR PREVAX ONLY ------------------------------
###################################################################################################

if (cohort_name == "prevax"){

  # Restrict data only to those that had a diagnosis of type 2 diabetes following a COVID-19 infection
  
  input_12 <- input %>%
    dplyr::filter( !is.na(out_date_t2dm) &  !is.na(exp_date_covid19_confirmed)) %>%
    # keep those where t2dm is after infection
    rowwise() %>%
    mutate(keep = ifelse((out_date_t2dm >= cohort_start_date) & (out_date_t2dm >= exp_date_covid19_confirmed) & (out_date_t2dm <= t2dm_follow_up_end), TRUE, FALSE)) %>%
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
  
  ####################################################################################
  # Redefine diabetes in the prevax cohort ----------------------------------
  # We are simply removing those with diabetes that were not still being treated after 4 months
  ####################################################################################
  
  # do what we did above but for all (not just those following COVID)
  # calculate new end date
  
  # Creates 4 additional outcomes
  # out_date_t2dm_follow which is Persistent diabetes
  # out_date_t2dm_follow_extended_follow_up which is Persistent diabetes with extended follow up
  
  # out_date_t2dm_4months_follow_up_no_cox_only_table_2 which is type 2 diabetes where patients have 4 months of follow up
  # out_date_t2dm_4months_follow_up_no_cox_only_table_2_extended_follow_up which is type 2 diabetes where patients have 4 months of follow up using etended follow up end dates
  # The above two outcomes are ONLY for table 2 where event counts are calculated and are NOT to be run with the cox model
  
  input$cohort_end_date <- cohort_end_date
  input$end_date <- apply(input[,c("death_date", "dereg_date", "cohort_end_date")],1, min,na.rm=TRUE)
  input$end_date <- as.Date(input$end_date)
  
  input$cohort_end_date_extended <- cohort_end_date_extended
  input$end_date_extended <- apply(input[,c("death_date", "dereg_date", "cohort_end_date_extended")],1, min,na.rm=TRUE)
  input$end_date_extended <- as.Date(input$end_date_extended)
  
  # Get N with 4 months follow up (those with an end date >= 4 months from t2dm)
  
  input <- input %>% 
    rowwise() %>%
    mutate(start_end_diff = as.numeric(difftime(end_date, out_date_t2dm, units = "days"))) %>%
    ungroup() %>%
    mutate(start_end_diff_months = start_end_diff/30.417) %>%
    mutate(follow_4mth = ifelse(start_end_diff_months >= 4, TRUE, FALSE))
  
  input <- input %>% 
    rowwise() %>%
    mutate(start_end_diff_extended = as.numeric(difftime(end_date_extended, out_date_t2dm_extended_follow_up, units = "days"))) %>%
    ungroup() %>%
    mutate(start_end_diff_months_extended = start_end_diff_extended/30.417) %>%
    mutate(follow_4mth_extended = ifelse(start_end_diff_months_extended >= 4, TRUE, FALSE))
  
  # summarise df
  summary(input)
  
  # N of those that were followed up and still being prescribed medication or had elevated HbA1c
  
  input <- input %>%
    # create total N prescriptions variable
    rowwise() %>%
    mutate(total_prescriptions = sum(out_count_insulin_snomed_4mnths, out_count_antidiabetic_drugs_snomed_4mnths, out_count_nonmetform_drugs_snomed_4mnths)) %>%
    ungroup() %>%
    mutate(N_follow_prescribe = ifelse(follow_4mth == TRUE & (out_num_max_hba1c_mmol_4mnths >= 47.5), TRUE,
                                       ifelse(follow_4mth == TRUE & (total_prescriptions >= 2), TRUE, FALSE)))
  
  input <- input %>%
    # create total N prescriptions variable
    rowwise() %>%
    mutate(total_prescriptions_extended = sum(out_count_insulin_snomed_4mnths, out_count_antidiabetic_drugs_snomed_4mnths, out_count_nonmetform_drugs_snomed_4mnths)) %>%
    ungroup() %>%
    mutate(N_follow_prescribe_extended = ifelse(follow_4mth_extended == TRUE & (out_num_max_hba1c_mmol_4mnths >= 47.5), TRUE,
                                       ifelse(follow_4mth_extended == TRUE & (total_prescriptions_extended >= 2), TRUE, FALSE)))
  

  # read in main input file 
  
  input_main <- readr::read_rds(file.path("output", paste0("input_prevax_stage1_diabetes.rds")))
  input_main$out_date_t2dm_follow <- NULL
  input_main$out_date_t2dm_follow_extended_follow_up <- NULL
  input_main$out_date_t2dm_4months_follow_up_no_cox_only_table_2 <- NULL
  input_main$out_date_t2dm_4months_follow_up_no_cox_only_table_2_extended_follow_up <- NULL
    
  # get list of IDs that will be t2dm cases post covid that are not being treated after 4 months (i.e., suspected stress/steroid induced cases)
  
  remove <- input %>%
    dplyr::filter(N_follow_prescribe == FALSE)
  remove_ids <- remove$patient_id
  
  remove_extended <- input %>%
    dplyr::filter(N_follow_prescribe_extended == FALSE)
  remove_ids_extended <- remove_extended$patient_id
  
  remove_4months_follow_up <- input %>%
    dplyr::filter(follow_4mth == FALSE)
  remove_ids_4months_follow_up <- remove_4months_follow_up$patient_id
  
  remove_4months_follow_up_extended <- input %>%
    dplyr::filter(follow_4mth_extended == FALSE)
  remove_ids_4months_follow_up_extended <- remove_4months_follow_up_extended$patient_id
  
  # and now remove them 
  
  input_new_t2dm_cases <- input_main[ ! input_main$patient_id %in% remove_ids, ]
  input_new_t2dm_cases_extended <- input_main[ ! input_main$patient_id %in% remove_ids_extended, ]
  
  input_new_t2dm_cases_4months_follow_up <- input_main[ ! input_main$patient_id %in% remove_ids_4months_follow_up, ]
  input_new_t2dm_cases_4months_follow_up_extended <- input_main[ ! input_main$patient_id %in% remove_ids_4months_follow_up_extended, ]
  
  # rename t2dm variable to t2dm_follow 
  
  input_new_t2dm_cases <- input_new_t2dm_cases %>%
    dplyr::rename(out_date_t2dm_follow = out_date_t2dm,) %>%
    dplyr::select(patient_id, out_date_t2dm_follow)
  
  input_new_t2dm_cases_extended <- input_new_t2dm_cases_extended %>%
    dplyr::rename(out_date_t2dm_follow_extended_follow_up = out_date_t2dm_extended_follow_up,) %>%
    dplyr::select(patient_id, out_date_t2dm_follow_extended_follow_up)
  
  input_new_t2dm_cases_4months_follow_up <- input_new_t2dm_cases_4months_follow_up %>%
    dplyr::rename(out_date_t2dm_4months_follow_up_no_cox_only_table_2 = out_date_t2dm,) %>%
    dplyr::select(patient_id, out_date_t2dm_4months_follow_up_no_cox_only_table_2)
  
  input_new_t2dm_cases_4months_follow_up_extended <- input_new_t2dm_cases_4months_follow_up_extended %>%
    dplyr::rename(out_date_t2dm_4months_follow_up_no_cox_only_table_2_extended_follow_up = out_date_t2dm_extended_follow_up,) %>%
    dplyr::select(patient_id, out_date_t2dm_4months_follow_up_no_cox_only_table_2_extended_follow_up)
  
  # merge and save input file back ready for cox analysis - the only change made is the addition of out_date_t2dm_follow variable 
  
  input_main <- merge(input_main, input_new_t2dm_cases, all.x = TRUE)
  input_main <- merge(input_main, input_new_t2dm_cases_extended, all.x = TRUE)
  
  input_main <- merge(input_main, input_new_t2dm_cases_4months_follow_up, all.x = TRUE)
  input_main <- merge(input_main, input_new_t2dm_cases_4months_follow_up_extended, all.x = TRUE)
  
  input_main <- input_main %>%
    mutate(across(c(contains("_date")),
                  ~ floor_date(as.Date(., format="%Y-%m-%d"), unit = "days")))
  
  # SAVE input file with new diabetes outcome added
  
  saveRDS(input_main, file = file.path("output", paste0("input_prevax_stage1_diabetes.rds")))
  
}

}

# RUN FUNCTION WITH COMMAND ARGS

diabetes_post_hoc(cohort_name)

# END