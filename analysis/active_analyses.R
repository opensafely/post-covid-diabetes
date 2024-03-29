library(tidyverse)

# Create output directory ------------------------------------------------------

fs::dir_create(here::here("lib"))

# Create empty data frame ------------------------------------------------------

df <- data.frame(active = logical(),
                 outcome = character(),
                 outcome_variable = character(),
                 covariates = character(),
                 model = character(),
                 cohort	= character(),
                 main = character(),
                 covid_history = character(),
                 covid_pheno_hospitalised = character(),
                 covid_pheno_non_hospitalised = character(),
                 agegp_18_39 = character(),
                 agegp_40_59 = character(),
                 agegp_60_79 = character(),
                 agegp_80_110 = character(),
                 sex_Male = character(),
                 sex_Female = character(),
                 ethnicity_White = character(),
                 ethnicity_Mixed = character(),
                 ethnicity_South_Asian = character(),
                 ethnicity_Black = character(),
                 ethnicity_Other = character(),
                 ethnicity_Missing = character(),
                 prior_history_TRUE = character(),
                 prior_history_FALSE = character(),
                 aer_Female_18_39 = character(),
                 aer_Female_40_59 = character(),
                 aer_Female_60_79 = character(),
                 aer_Female_80_110 = character(),
                 aer_Male_18_39 = character(),
                 aer_Male_40_59 = character(),
                 aer_Male_60_79 = character(),
                 aer_Male_80_110 = character(),
                 prior_history_var = character(),
                 outcome_group = character(),
                 venn = character(),
                 data_only = character(),
                 stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# Add diabetes outcomes --------------------------------------------------------
# ------------------------------------------------------------------------------

outcomes <- c("type 1 diabetes",
              "type 2 diabetes",
              "type 2 diabetes - recovery",
              "type 2 diabetes - pre_recovery",
              "type 2 diabetes - post_recovery",
              "type 2 diabetes - persistant",
              "type 2 diabetes - pre diabetes",
              "type 2 diabetes - no pre diabetes",
              "type 2 diabetes - obesity",
              "type 2 diabetes - no obesity",
              "other or non-specific diabetes",
              "gestational diabetes",
              "type 1 diabetes - extended follow up",
              "type 2 diabetes - extended follow up",
              "other or non-specific diabetes - extended follow up",
              "gestational diabetes - extended follow up", 
              "type 2 diabetes - persistant - extended follow up",
              "type 2 diabetes - pre diabetes - extended follow up",
              "type 2 diabetes - no pre diabetes - extended follow up",
              "type 2 diabetes - obesity - extended follow up",
              "type 2 diabetes - no obesity - extended follow up",
              "type 2 diabetes - unvax sensitivity",
              "type 1 diabetes - unvax sensitivity",
              "other or non-specific diabetes - unvax sensitivity",
              "gestational diabetes - unvax sensitivity")

outcome_group <- "diabetes"

outcomes_short <- c("t1dm","t2dm", "t2dm_rec", "t2dm_pre_rec", "t2dm_post_rec", "t2dm_follow", "t2dm_pd","t2dm_pd_no", "t2dm_obes","t2dm_obes_no", "otherdm","gestationaldm",
                    "t1dm_extended_follow_up","t2dm_extended_follow_up", "otherdm_extended_follow_up","gestationaldm_extended_follow_up", "t2dm_follow_extended_follow_up",
                    "t2dm_pd_extended_follow_up","t2dm_pd_no_extended_follow_up", "t2dm_obes_extended_follow_up","t2dm_obes_no_extended_follow_up",
                    "t2dm_unvax_sens", "t1dm_unvax_sens", "otherdm_unvax_sens", "gestationaldm_unvax_sens")
outcome_venn <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(TRUE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                       rep("all",2),
                       rep(TRUE,4),
                       rep(FALSE,22),
                       "",
                       "diabetes",
                       outcome_venn[i],
                       "FALSE")
}

# change outcome group so that gestational diabetes has its own group

df <- df %>% mutate(outcome_group = case_when(outcome_variable == "out_date_gestationaldm" ~ "diabetes_gestational",
                                              TRUE ~ as.character(outcome_group)))

df <- df %>% mutate(outcome_group = case_when(outcome_variable == "out_date_gestationaldm_extended_follow_up" ~ "diabetes_gestational",
                                              TRUE ~ as.character(outcome_group)))

df <- df %>% mutate(outcome_group = case_when(outcome_variable == "out_date_gestationaldm_unvax_sens" ~ "diabetes_gestational",
                                              TRUE ~ as.character(outcome_group)))

# change outcome groups

df <- df %>% mutate(outcome_group = case_when(outcome == "type 2 diabetes - pre diabetes" | outcome == "type 2 diabetes - pre diabetes - extended follow up" ~ "diabetes_prediabetes",
                                              TRUE ~ as.character(outcome_group)),
                    outcome_group = case_when(outcome == "type 2 diabetes - recovery" ~ "diabetes_recovery",
                                              TRUE ~ as.character(outcome_group)),
                    outcome_group = case_when(outcome == "type 2 diabetes - pre_recovery" ~ "diabetes_pre_recovery",
                                              TRUE ~ as.character(outcome_group)),
                    outcome_group = case_when(outcome == "type 2 diabetes - post_recovery" ~ "diabetes_post_recovery",
                                              TRUE ~ as.character(outcome_group)),
                    outcome_group = case_when(outcome == "type 2 diabetes - no pre diabetes" | outcome == "type 2 diabetes - no pre diabetes - extended follow up" ~ "diabetes_no_prediabetes",
                                              TRUE ~ as.character(outcome_group)),
                    outcome_group = case_when(outcome == "type 2 diabetes - obesity" | outcome == "type 2 diabetes - obesity - extended follow up" ~ "diabetes_obesity",
                                              TRUE ~ as.character(outcome_group)), 
                    outcome_group = case_when(outcome == "type 2 diabetes - no obesity" | outcome == "type 2 diabetes - no obesity - extended follow up" ~ "diabetes_no_obesity",
                                              TRUE ~ as.character(outcome_group)))

# change diabetes recovery to prevax cohort only

df <- df %>% mutate(cohort = case_when(outcome == "type 2 diabetes - recovery" ~ "prevax",
                                              TRUE ~ as.character(cohort)),
                    cohort = case_when(outcome == "type 2 diabetes - post_recovery" ~ "prevax",
                                       TRUE ~ as.character(cohort)),
                    cohort = case_when(outcome == "type 2 diabetes - persistant" ~ "prevax",
                                      TRUE ~ as.character(cohort)),
                    cohort = case_when(grepl("extended_follow_up", outcome_variable) ~ "prevax",
                                       TRUE ~ as.character(cohort))) %>%
  # unvax sensitivity
  mutate(cohort = case_when(grepl("unvax_sens", outcome_variable) ~ "unvax",
                                                TRUE ~ as.character(cohort)))

# turn on subgroups for main t2dm analyses

#df[2,c(11:22, 25:32)] <- TRUE
df[2,c(11:22)] <- TRUE
#df[14,c(11:22, 25:32)] <- TRUE
df[14,c(11:22)] <- TRUE



# turn on t2dm

# df[2,1] <- TRUE

# Remove sex as a covariate for gestational diabetes analysis

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_gestationaldm" ~ "cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_gestationaldm_extended_follow_up" ~ "cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_gestationaldm_unvax_sens" ~ "cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

# remove BMI for obesity subgroup analysis

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_obes" | outcome_variable == "out_date_t2dm_obes_extended_follow_up"  ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_obes_no" | outcome_variable == "out_date_t2dm_obes_no_extended_follow_up" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

# remove pre-diabetes for pre-diabetes subgroup analysis

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_pd" | outcome_variable == "out_date_t2dm_pd_extended_follow_up" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_pd_no" | outcome_variable == "out_date_t2dm_pd_no_extended_follow_up" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

# turn all COVID history to false

df$covid_history <- FALSE
df$venn <- FALSE

# add pre diabetes subgroup analysis
# 
# df$prior_history_var <- ifelse(df$outcome=="type 2 diabetes" ,"cov_bin_prediabetes",df$prior_history_var)
# df$prior_history_TRUE <- ifelse(df$outcome=="type 2 diabetes" ,TRUE,df$prior_history_TRUE)
# df$prior_history_FALSE <- ifelse(df$outcome=="type 2 diabetes" ,TRUE,df$prior_history_FALSE)

# Save active analyses list ----------------------------------------------------

saveRDS(df, file = "lib/active_analyses.rds")