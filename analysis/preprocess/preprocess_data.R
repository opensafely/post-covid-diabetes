# Load libraries ---------------------------------------------------------------
tictoc::tic()
library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)

# Specify command arguments ----------------------------------------------------
args <- commandArgs(trailingOnly=TRUE)
print(length(args))
if(length(args)==0){
  # use for interactive testing
  cohort_name <- "vax"
} else {
  cohort_name <- args[[1]]
}

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review"))


# Read cohort dataset ---------------------------------------------------------- 

df <- arrow::read_feather(file = paste0("output/input_",cohort_name,".feather") )

message(paste0("Dataset has been read successfully with N = ", nrow(df), " rows"))

#Add death_date from prelim data
prelim_data <- read_csv("output/index_dates.csv") %>%
  select(c(patient_id,death_date))
df <- df %>% inner_join(prelim_data,by="patient_id")

message("Death date added!")


# Format columns ---------------------------------------------------------------
# dates, numerics, factors, logicals

df <- df %>%
  mutate(across(c(contains("_date")),
                ~ floor_date(as.Date(., format="%Y-%m-%d"), unit = "days")),
         across(contains('_birth_year'),
                ~ format(as.Date(.), "%Y")),
         across(contains('_num') & !contains('date'), ~ as.numeric(.)),
         across(contains('_cat'), ~ as.factor(.)),
         across(contains('_bin'), ~ as.logical(.)))


# Overwrite vaccination information for dummy data and vax cohort only --

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations") &&
   cohort_name %in% c("vax")) {
  source("analysis/preprocess/modify_dummy_vax_data.R")
  message("Vaccine information overwritten successfully")
}


# Describe data ----------------------------------------------------------------

sink(paste0("output/not-for-review/describe_",cohort_name,".txt"))
print(Hmisc::describe(df))
sink()

message ("Cohort ",cohort_name, " description written successfully!")

#Combine BMI variables to create one history of obesity variable ---------------

df$cov_bin_obesity <- ifelse(df$cov_bin_obesity == TRUE | 
                               df$cov_cat_bmi_groups=="Obese",TRUE,FALSE)
df[,c("cov_num_bmi")] <- NULL

# QC for consultation variable--------------------------------------------------
#max to 365 (average of one per day)
df <- df %>%
  mutate(cov_num_consulation_rate = replace(cov_num_consulation_rate, 
                                            cov_num_consulation_rate > 365, 365))


#COVID19 severity --------------------------------------------------------------

df <- df %>%
  mutate(sub_cat_covid19_hospital = 
           ifelse(!is.na(exp_date_covid19_confirmed) &
                    !is.na(sub_date_covid19_hospital) &
                    sub_date_covid19_hospital - exp_date_covid19_confirmed >= 0 &
                    sub_date_covid19_hospital - exp_date_covid19_confirmed < 29, "hospitalised",
                  ifelse(!is.na(exp_date_covid19_confirmed), "non_hospitalised", 
                         ifelse(is.na(exp_date_covid19_confirmed), "no_infection", NA)))) %>%
  mutate(across(sub_cat_covid19_hospital, factor))
df <- df[!is.na(df$patient_id),]
df[,c("sub_date_covid19_hospital")] <- NULL

message("COVID19 severity determined successfully")

# REPO SPECIFIC PROCESSING ------------------------------------------------

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
  mutate(hba1c_date_step7 = as_date(case_when(tmp_out_num_max_hba1c_mmol_mol >= 47.5 ~ pmin(tmp_out_num_max_hba1c_date, na.rm = TRUE))),
         # process codes - this is taking the first process code date in those individuals that have 5 or more process codes
         over5_pocc_step7 = as_date(case_when(tmp_out_count_poccdm_snomed >= 5 ~ pmin(out_date_poccdm, na.rm = TRUE))))

print("COVID-19 and diabetes variables needed for algorithm created successfully")

# Define diabetes outcome (using Sophie Eastwood algorithm) ----------------------------

scripts_dir <- "analysis/preprocess"
source(file.path(scripts_dir,"diabetes_algorithm.R"))
df <- diabetes_algo(df)
print("Diabetes algorithm run successfully")
print(paste0(nrow(df), " rows in df after diabetes algo"))

# Restrict columns and save analysis dataset ---------------------------------

df1 <- df%>% select(patient_id,"death_date",starts_with("index_date_"),
                    starts_with("end_date_"),
                    contains("sub_"), # Subgroups
                    contains("exp_"), # Exposures
                    contains("out_"), # Outcomes
                    contains("cov_"), # Covariates
                    contains("qa_"), # Quality assurance
                    #contains("step"), # diabetes steps
                    contains("vax_date_eligible"), # Vaccination eligibility
                    contains("vax_date_"), # Vaccination dates and vax type 
                    #contains("vax_cat_")# Vaccination products
)


df1[,colnames(df)[grepl("tmp_",colnames(df))]] <- NULL

saveRDS(df1, file = paste0("output/input_",cohort_name,".rds"))

message(paste0("Input data saved successfully with N = ", nrow(df1), " rows"))

# Describe data --------------------------------------------------------------

sink(paste0("output/not-for-review/describe_input_",cohort_name,"_stage0.txt"))
print(Hmisc::describe(df1))
sink()

# Restrict columns and save Venn diagram input dataset -----------------------

df2 <- df %>% select(starts_with(c("patient_id","tmp_out_date","out_date")))

# Describe data --------------------------------------------------------------

sink(paste0("output/not-for-review/describe_venn_",cohort_name,".txt"))
print(Hmisc::describe(df2))
sink()

saveRDS(df2, file = paste0("output/venn_",cohort_name,".rds"))

message("Venn diagram data saved successfully")
tictoc::toc()