##################################################################################
# 
# Description: This script reads in the input data and prepares it for data cleaning.
#
# Input: output/input.feather
# Output: output/
#
# Author(s): Rachel Denholm,  Kurt Taylor
#
# Date last updated: 
#
##################################################################################

# Load libraries ---------------------------------------------------------------

library(magrittr)
library(tidyverse)
library(lubridate)

# FILE PATHS

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review"))

# Define parameters ------------------------------------------------------------

## Study start date
study_start <- "2020-01-01"

# Create spine dataset ---------------------------------------------------------

dfspine <- arrow::read_feather(file = "output/input_prelim.feather",
                          col_select = c("patient_id",
                                         "death_date",
                                         "vax_date_covid_1"))

print("Spine dataset (input prelim feather) read in successfully")
print(paste0(nrow(df), " rows in spine dataset"))

## Load dataset
df <- arrow::read_feather(file = "output/input_prevax.feather")

## merge with spine 

df <- merge(df,dfspine, by = "patient_id")

# QC for consultation variable
# max to 365 (average of one per day)

print("Consultation variable before QC")
summary(df$cov_num_consulation_rate)

df <- df %>%
  mutate(cov_num_consulation_rate = replace(cov_num_consulation_rate, cov_num_consulation_rate > 365, 365))

print("Consultation variable after QC")
summary(df$cov_num_consulation_rate)

# Combine BMI variables to create one history of obesity variable ---------------

df <- df %>%
  mutate(cov_bin_obesity = ifelse(cov_bin_obesity == TRUE | cov_cat_bmi_groups == "Obese", TRUE, FALSE)) %>%
  dplyr::select(- cov_num_bmi)

# Format columns -----------------------------------------------------
# dates, numerics, factors, logicals

df <- df %>%
  dplyr::rename(tmp_out_max_hba1c_mmol_mol_date = tmp_out_num_max_hba1c_date,
         tmp_out_bmi_date_measured = cov_num_bmi_date_measured) %>%
  mutate(across(contains('_date'), ~ as.Date(as.character(.)))) %>%
  mutate(across(contains('_birth_year'), ~ format(as.Date(.), "%Y"))) %>%
  mutate(across(contains('_num'), ~ as.numeric(.))) %>%
  mutate(across(contains('_cat'), ~ as.factor(.))) %>%
  mutate(across(contains('_bin'), ~ as.logical(.)))

print("Columns formatted successfully")

# Define COVID-19 severity --------------------------------------------------------------

df <- df %>%
  mutate(sub_cat_covid19_hospital = 
           ifelse(!is.na(exp_date_covid19_confirmed) &
                    !is.na(sub_date_covid19_hospital) &
                    sub_date_covid19_hospital - exp_date_covid19_confirmed >= 0 &
                    sub_date_covid19_hospital - exp_date_covid19_confirmed < 29, "hospitalised",
                  ifelse(!is.na(exp_date_covid19_confirmed), "non_hospitalised", 
                         ifelse(is.na(exp_date_covid19_confirmed), "no_infection", NA)))) %>%
  mutate(across(sub_cat_covid19_hospital, factor))

# Define diabetes outcome (using Sophie Eastwood algorithm) ----------------------------

# Create vars for diabetes outcomes -------------------------------------------------------------

# vars could not be created in common vars file

df <- df %>% mutate(tmp_out_count_t2dm = tmp_out_count_t2dm_snomed + tmp_out_count_t2dm_hes,
                    tmp_out_count_t1dm = tmp_out_count_t1dm_snomed + tmp_out_count_t1dm_hes)

print("Diabetes count variables created successfully")

# remove biologically implausible TC/HDL ratio values: https://doi.org/10.1093/ije/dyz099
# Remove TC < 1.75 or > 20 
# remove HDL < 0.4 or > 5
df <- df %>%
  mutate(tmp_cov_num_cholesterol = replace(tmp_cov_num_cholesterol, tmp_cov_num_cholesterol < 1.75 | tmp_cov_num_cholesterol > 20, NA),
         tmp_cov_num_hdl_cholesterol = replace(tmp_cov_num_hdl_cholesterol, tmp_cov_num_hdl_cholesterol < 0.4 | tmp_cov_num_hdl_cholesterol > 5, NA)) %>%
  mutate(cov_num_tc_hdl_ratio = tmp_cov_num_cholesterol / tmp_cov_num_hdl_cholesterol) %>%
  mutate(cov_num_tc_hdl_ratio = replace(cov_num_tc_hdl_ratio, cov_num_tc_hdl_ratio > 50 | cov_num_tc_hdl_ratio < 1, NA))

# replace NaN and Inf with NA's (probably only an issue with dummy data)
df$cov_num_tc_hdl_ratio[is.nan(df$cov_num_tc_hdl_ratio)] <- NA
df$cov_num_tc_hdl_ratio[is.infinite(df$cov_num_tc_hdl_ratio)] <- NA

print("Cholesterol ratio variable created successfully and QC'd")
summary(df$cov_num_tc_hdl_ratio)

# define variables needed for diabetes algorithm 

df <- df %>% 
  mutate(tmp_out_year_first_diabetes_diag = format(tmp_out_date_first_diabetes_diag,"%Y")) %>%
  mutate(tmp_out_year_first_diabetes_diag = as.integer(tmp_out_year_first_diabetes_diag),
         age_1st_diag = tmp_out_year_first_diabetes_diag - qa_num_birth_year) %>%
  mutate(age_1st_diag = replace(age_1st_diag, which(age_1st_diag < 0), NA)) %>% # assign negative ages to NA)
  mutate(age_under_35_30_1st_diag = ifelse(!is.na(age_1st_diag) &
                                             (age_1st_diag < 35 & 
                                                (cov_cat_ethnicity == 1 | cov_cat_ethnicity == 2  | cov_cat_ethnicity == 5)) | 
                                             (age_1st_diag < 30), "Yes", "No")) %>%
  # HBA1C date var - earliest date for only those with >=47.5
  mutate(hba1c_date_step7 = as_date(case_when(tmp_out_num_max_hba1c_mmol_mol >= 47.5 ~ pmin(tmp_out_max_hba1c_mmol_mol_date, na.rm = TRUE))),
         # process codes - this is taking the first process code date in those individuals that have 5 or more process codes
         over5_pocc_step7 = as_date(case_when(tmp_out_count_poccdm_snomed >= 5 ~ pmin(out_date_poccdm, na.rm = TRUE))))

print("COVID-19 and diabetes variables needed for algorithm created successfully")

# Define diabetes outcome (using Sophie Eastwood algorithm) ----------------------------

scripts_dir <- "analysis/preprocess"
source(file.path(scripts_dir,"diabetes_algorithm.R"))
df <- diabetes_algo(df)
print("Diabetes algorithm run successfully")
print(paste0(nrow(df), " rows in df line 418 after diabetes algo"))

# Restrict columns and save analysis dataset ---------------------------------

df1 <- df %>% 
  dplyr::select(- vax_jcvi_age_1, - vax_jcvi_age_2) %>% #  remove JCVI variables
  # select patient id, death date and variables: subgroups, exposures, outcomes, covariates, quality assurance and vaccination
  # need diabetes "step" variables for flowchart (diabetes_flowchart.R)
  dplyr::select(patient_id, death_date,
                contains(c("sub_", "exp_", "out_", "cov_", "qa_", "vax_", "step"))) %>%
  dplyr::select(-contains("df_out_")) %>%
  dplyr::select(-contains("tmp_"))

# Describe data --------------------------------------------------------------

sink(paste0("output/not-for-review/describe_input_prevax_stage0.txt"))
print(Hmisc::describe(df1))
sink()

# SAVE

saveRDS(df1, file = paste0("output/input_prevax.rds"))

print("Dataset saved successfully")

# Restrict columns and save Venn diagram input dataset -----------------------

# df2 <- df %>% 
#   dplyr::select(patient_id,
#                 starts_with(c("out_")))

# SAVE
## create folders for outputs
# fs::dir_create(here::here("output", "venn"))
saveRDS(df, file = paste0("output/venn_prevax.rds"))

print("Venn dataset saved successfully")

# END