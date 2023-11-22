## =============================================================================
## Pipeline (1): Control center, calls relevant analysis scripts, sets working 
## and saving directories, parallelises processes
##
## Based on scripts written by Samantha Ip, see the following repo's for 
## original scripts: https://github.com/BHFDSC/CCU002_01 & https://github.com/BHFDSC/CCU002_03
## =============================================================================


library(data.table)
library(dplyr)
library(survival)
library(broom)
library(DBI)
library(ggplot2)
library(nlme)
library(tidyverse)
#library(R.utils)
library(lubridate)
library(purrr)
library(parallel)
library(stats)
library(utils)
library(stringr)
library(rms)
#library(multcomp)
library(readr)
library(Hmisc)
library(matrixStats)


args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  event_name="t1dm"
  cohort="prevax"
  data_only_variable="FALSE"
}else{
  event_name  = args[[1]]
  cohort = args[[2]]
  data_only_variable = args[[3]]
}

# Specify directories ----------------------------------------------------------

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review", "model"))
fs::dir_create(here::here("output", "review", "model","fit-individual-covariates"))
fs::dir_create(here::here("output", "review", "model","prevax"))
fs::dir_create(here::here("output", "review", "model","vax"))
fs::dir_create(here::here("output", "review", "model","unvax"))
output_dir <- "output/review/model"
output_dir_prevax <- "output/review/model/prevax"
output_dir_vax <- "output/review/model/vax"
output_dir_unvax <- "output/review/model/unvax"
scripts_dir <- "analysis/model"

# Source relevant files --------------------------------------------------------

source(file.path(scripts_dir,"02_01_cox_analyses_to_run.R"))
source(file.path(scripts_dir,"02_02_cox_load_data.R")) # Prepare dataset for model
source(file.path(scripts_dir,"06_cox_extra_functions.R"))

# Add time point parameter to analyses to run  ----------------------------

source(file.path(scripts_dir,"02_03_cox_timepoint_param.R")) # Prepare dataset for model

# add reduced time point column 

analyses_to_run$reduced_timepoint <- lapply(split(analyses_to_run,seq(nrow(analyses_to_run))),
                                            function(analyses_to_run) 
                                              get_timepoint(
                                                event=analyses_to_run$event,
                                                subgroup=analyses_to_run$subgroup,
                                                stratify_by_subgroup=analyses_to_run$stratify_by_subgroup,
                                                stratify_by=analyses_to_run$strata,
                                                input)
)

analyses_to_run$reduced_timepoint <-  as.character(analyses_to_run$reduced_timepoint)
analyses_to_run <- analyses_to_run %>% filter(reduced_timepoint != "remove")
analyses_to_run_normal_timepoint <- analyses_to_run %>% filter(reduced_timepoint == "normal")
analyses_to_run$reduced_timepoint <- "reduced"
analyses_to_run <- rbind(analyses_to_run, analyses_to_run_normal_timepoint)

rm(analyses_to_run_normal_timepoint)

# Add day zero analyses

day_zero_analyses <- analyses_to_run %>% filter(subgroup %in% c("main","covid_pheno_hospitalised", "covid_pheno_non_hospitalised"))
day_zero_analyses$reduced_timepoint <- paste0("day_zero_",day_zero_analyses$reduced_timepoint)
analyses_to_run <- rbind(analyses_to_run, day_zero_analyses)
rm(day_zero_analyses)

# Add in data only setting
analyses_to_run$data_only <- data_only_variable
analyses_to_run$data_only <- ifelse(analyses_to_run$event %in% c("t2dm","t2dm_extended_follow_up") 
                                    & analyses_to_run$subgroup %in% c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"), TRUE, analyses_to_run$data_only)
analyses_to_run$data_only <- ifelse(analyses_to_run$event == "t2dm_pd_no"
                                    & analyses_to_run$subgroup %in% c("covid_pheno_hospitalised"), TRUE, analyses_to_run$data_only)

# Source remainder of relevant files --------------------------------------------------------

source(file.path(scripts_dir,paste0("03_01_cox_subgrouping.R"))) # Model specification

# ------------------------------------ LAUNCH JOBS -----------------------------
if(nrow(analyses_to_run>0)){
  lapply(split(analyses_to_run,seq(nrow(analyses_to_run))),
         function(analyses_to_run)
           get_vacc_res(           
             event=analyses_to_run$event,           
             subgroup=analyses_to_run$subgroup,           
             stratify_by_subgroup=analyses_to_run$stratify_by_subgroup,           
             stratify_by=analyses_to_run$strata,           
             time_point=analyses_to_run$reduced_timepoint,
             data_only=analyses_to_run$data_only,
             input,covar_names,
             cuts_days_since_expo,cuts_days_since_expo_reduced,
             cuts_days_since_expo_day_zero,cuts_days_since_expo_reduced_day_zero,
             mdl))
}

if(cohort == "prevax"){
  
  #Save csv of analyses not run
  write.csv(analyses_not_run, paste0(output_dir_prevax,"/analyses_not_run_" , event_name ,"_",cohort,".csv"), row.names = T)

} else if (cohort == "vax"){
  
  #Save csv of analyses not run
  write.csv(analyses_not_run, paste0(output_dir_vax,"/analyses_not_run_" , event_name ,"_",cohort,".csv"), row.names = T)
  
} else if (cohort == "unvax"){
  
  #Save csv of analyses not run
  write.csv(analyses_not_run, paste0(output_dir_unvax,"/analyses_not_run_" , event_name ,"_",cohort,".csv"), row.names = T)

}

if(nrow(analyses_to_run)==0){
  sink(paste0("output/not-for-review/describe_data_surv_",event_name,"_",cohort,"_time_periods.txt"))
  sink()
  
  df <- as.data.frame(matrix(ncol = 2))
#  write.csv(df, paste0("output/input_",event_name,"__",cohort,"__time_periods.csv"))
  write.csv(df, file=gzfile(paste0("output/input_sampled_data_",event_name,"_",cohort,"_time_periods.csv.gz")))
  
}
  

#Combine all results into one .csv
source(file.path(scripts_dir, "05_cox_format_tbls_HRs.R"))

