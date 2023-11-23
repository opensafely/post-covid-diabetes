# Create output directory ------------------------------------------------------

fs::dir_create(here::here("lib"))

# Run existing file ------------------------------------------------------------

source("analysis/active_analyses.R")

# Specify cohorts --------------------------------------------------------------

cohorts <- c("vax","unvax","prevax")

# Import existing active analyses table ----------------------------------------

df <- readr::read_rds("lib/active_analyses.rds")

# Remove unncessary covariates -------------------------------------------------

df[,c("active","outcome","model","venn","data_only","outcome_group","prior_history_var",
      colnames(df)[grepl("aer_",colnames(df))])] <- NULL

df$covid_history <- as.character(df$covid_history)

# Pivot to long format ---------------------------------------------------------

df <- tidyr::pivot_longer(data = df,
                          cols = setdiff(colnames(df),c("outcome_variable","covariates","cohort")),
                          names_to = "analysis",
                          values_to = "keep")

# Filter to relevant analyses --------------------------------------------------

df <- df[df$keep=="TRUE",
         c("outcome_variable","covariates","cohort","analysis")]

# Explicity specify cohorts ----------------------------------------------------

df$cohort <- ifelse(df$cohort=="all","prevax;vax;unvax",df$cohort)

df <- tidyr::separate_longer_delim(data = df,
                                   cols = "cohort",
                                   delim = ";")

# Sort covars and strata -------------------------------------------------------

df$covariates <- gsub("cov_num_tc_hdl_ratio;","",df$covariates)

df$covariate_age <- "cov_num_age"
df$covariates <- gsub("cov_num_age;","",df$covariates)

df$covariate_sex <- "cov_cat_sex"
df$covariates <- gsub("cov_cat_sex;","",df$covariates)

df$strata <- "cov_cat_region"
df$covariates <- gsub("cov_cat_region;","",df$covariates)

# Add missing variables --------------------------------------------------------

df$exposure <- "exp_date_covid19_confirmed"
df$cox_start <- "index_date"
df$cox_stop <- "end_date_outcome"
df$study_stop <- "2021-12-14"
df$total_event_threshold <- 50L
df$episode_event_threshold <- 5L
df$covariate_threshold <- 5L
df$study_start <- ifelse(df$cohort=="prevax", "2020-01-01", "2021-06-01")
df$cut_points <- ifelse(df$cohort=="prevax", "28;197;365;714", "28;197") # Only running extended follow up
df$controls_per_case <- 20L
df$ipw <- ifelse(df$cohort=="unvax", FALSE, TRUE)

# Rename variables -------------------------------------------------------------

df <- dplyr::rename(df,
                    "outcome" = "outcome_variable",
                    "covariate_other" = "covariates")

# Remove RECOVERY trial analyses as not used -----------------------------------

df <- df[!grepl("_rec",df$outcome),]

# Restrict to extended follow up analyses --------------------------------------
# prevax: outcomes containing "_extended_follow_up"
# vax: not applicable
# unvax: outcomes containing "_unvax_sens"

df <- df[!grepl("_rec",df$outcome),]

df <- df[(df$cohort=="prevax" & grepl("_extended_follow_up",df$outcome)) |
           (df$cohort=="vax") |
           (df$cohort=="unvax" & grepl("_unvax_sens",df$outcome)),]

# NOTE: Obesity and pre-diabetes subgroups have previously been incorporated as
# edited outcomes. This is not best practice for the current setup however has 
# not been changed as we are trying to replicate an existing analysis.

# Fix subgroup names -----------------------------------------------------------

df$analysis <- ifelse(df$analysis=="covid_pheno_hospitalised",
                      "sub_covid_hospitalised",df$analysis)

df$analysis <- ifelse(df$analysis=="covid_pheno_non_hospitalised",
                      "sub_covid_nonhospitalised",df$analysis)

df$analysis <- ifelse(df$analysis=="agegp_18_39",
                      "sub_age_18_39",df$analysis)

df$analysis <- ifelse(df$analysis=="agegp_40_59",
                      "sub_age_40_59",df$analysis)

df$analysis <- ifelse(df$analysis=="agegp_60_79",
                      "sub_age_60_79",df$analysis)

df$analysis <- ifelse(df$analysis=="agegp_80_110",
                      "sub_age_80_110",df$analysis)

df$analysis <- ifelse(df$analysis=="sex_Male",
                      "sub_sex_male",df$analysis)

df$analysis <- ifelse(df$analysis=="sex_Female",
                      "sub_sex_female",df$analysis)

df$analysis <- ifelse(df$analysis=="ethnicity_White",
                      "sub_ethnicity_white",df$analysis)

df$analysis <- ifelse(df$analysis=="ethnicity_Mixed",
                      "sub_ethnicity_mixed",df$analysis)

df$analysis <- ifelse(df$analysis=="ethnicity_South_Asian",
                      "sub_ethnicity_asian",df$analysis)

df$analysis <- ifelse(df$analysis=="ethnicity_Black",
                      "sub_ethnicity_black",df$analysis)

df$analysis <- ifelse(df$analysis=="ethnicity_Other",
                      "sub_ethnicity_other",df$analysis)

df$analysis <- ifelse(df$analysis=="ethnicity_Missing",
                      "sub_ethnicity_missing",df$analysis)

# Sort subgroup covariates -----------------------------------------------------

df$covariate_sex <- ifelse(substr(df$analysis,1,8)=="sub_sex_",
                           "NULL", df$covariate_sex)

df$covariate_other <- ifelse(substr(df$analysis,1,14)=="sub_ethnicity_",
                             gsub("cov_cat_ethnicity;","",df$covariate_other), df$covariate_other)

# Sort age spline --------------------------------------------------------------

df$age_spline <- ifelse(substr(df$analysis,1,8)=="sub_age_",
                        FALSE, TRUE)

# Order variables --------------------------------------------------------------

df <- df[,c("cohort",
            "exposure",
            "outcome",
            "ipw",
            "strata",
            "covariate_sex",
            "covariate_age",
            "covariate_other",
            "cox_start",
            "cox_stop",
            "study_start",
            "study_stop",
            "cut_points",
            "controls_per_case",
            "total_event_threshold",
            "episode_event_threshold",
            "covariate_threshold",
            "age_spline",
            "analysis")]

# Assign unique name -----------------------------------------------------------

df$name <- paste0("cohort_",df$cohort, "-", 
                  df$analysis, "-", 
                  gsub("out_date_","",df$outcome))

# Check names are unique and save active analyses list -------------------------

if (length(unique(df$name))==nrow(df)) {
  saveRDS(df, file = "lib/active_analyses.rds")
} else {
  stop(paste0("ERROR: names must be unique in active analyses table"))
}