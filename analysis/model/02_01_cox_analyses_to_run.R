##==============================================================================
## Reads in the active analyses table which specifies which 
## analysis to run for the outcome
##
## Creates a table of all the analyses to run for the outcome
## =============================================================================

## Read in active analyses table and filter to relevant outcome

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>%dplyr::filter(outcome_variable==paste0("out_date_",event_name) & active == "TRUE")

## Select covariates of interest

if(event_name == "out_date_ami" | event_name == "out_date_stroke_isch" | event_name == "out_date_dvt" |
   event_name == "out_date_pe" | event_name == "out_date_tia" | event_name == "out_date_stroke_sah_hs" |
   event_name == "out_date_hf" | event_name == "out_date_angina" | event_name == "out_date_ate" |
   event_name == "out_date_vte" | event_name == "out_date_ami_primary_position" | event_name == "out_date_stroke_isch_primary_position" |
   event_name == "out_date_dvt_primary_position" | event_name == "out_date_pe_primary_position" | event_name == "out_date_tia_primary_position" |
   event_name == "out_date_stroke_sah_hs_primary_position" | event_name == "out_date_hf_primary_position" | event_name == "out_date_angina_primary_position" |
   event_name == "out_date_ate_primary_position" | event_name == "out_date_vte_primary_position"){
  
  for(i in c("normal","reduced")){
    assign(paste0("non_zero_covar_names_",i),read_csv(paste0("output/not-for-review/non_zero_selected_covariates_",cohort,"_CVD_",i,"_time_periods.csv")) )
  }
  
} else if(event_name == "t1dm" | event_name == "t2dm" | event_name == "otherdm"){
  for(i in c("normal","reduced")){
    assign(paste0("non_zero_covar_names_",i),read_csv(paste0("output/not-for-review/non_zero_selected_covariates_",cohort,"_diabetes_",i,"_time_periods.csv")) )
  }
  
} else if (event_name == "t2dm_pd"){
  for(i in c("normal","reduced")){
    assign(paste0("non_zero_covar_names_",i),read_csv(paste0("output/not-for-review/non_zero_selected_covariates_",cohort,"_diabetes_prediabetes_",i,"_time_periods.csv")) )
  }
  
} else if (event_name == "t2dm_pd_no"){
  for(i in c("normal","reduced")){
    assign(paste0("non_zero_covar_names_",i),read_csv(paste0("output/not-for-review/non_zero_selected_covariates_",cohort,"_diabetes_no_prediabetes_",i,"_time_periods.csv")) )
  }
  
} else if (event_name == "t2dm_obes"){
  for(i in c("normal","reduced")){
    assign(paste0("non_zero_covar_names_",i),read_csv(paste0("output/not-for-review/non_zero_selected_covariates_",cohort,"_diabetes_obesity_",i,"_time_periods.csv")) )
  }
  
} else if (event_name == "t2dm_obes_no"){
  for(i in c("normal","reduced")){
    assign(paste0("non_zero_covar_names_",i),read_csv(paste0("output/not-for-review/non_zero_selected_covariates_",cohort,"_diabetes_no_obesity_",i,"_time_periods.csv")) )
  }
  
} else if (event_name == "gestationaldm"){
  for(i in c("normal","reduced")){
    assign(paste0("non_zero_covar_names_",i),read_csv(paste0("output/not-for-review/non_zero_selected_covariates_",cohort,"_diabetes_gestational_",i,"_time_periods.csv")) )
  }
  
} 

non_zero_covar_names <- rbind(non_zero_covar_names_normal, non_zero_covar_names_reduced)
rm(non_zero_covar_names_normal, non_zero_covar_names_reduced)

non_zero_covar_names <- non_zero_covar_names %>% filter(outcome_event == paste0("out_date_",event_name))
non_zero_covar_names$outcome_event <- gsub("out_date_", "",non_zero_covar_names$outcome_event)

covar_names <-str_split(active_analyses$covariates, ";")[[1]]
covar_names <-append(covar_names,"patient_id")
covar_names <-covar_names[!covar_names %in% c("cov_num_age","cov_cat_ethnicity","cov_cat_region","cov_cat_sex")]

##Set which models and cohorts are required

if(active_analyses$model=="all"){
  mdl=c("mdl_age_sex","mdl_age_sex_region","mdl_max_adj","mdl_max_adj_reduced_covars")
}else{
  mdl=active_analyses$model
}


## Transpose active_analyses to single column so can filter to analysis models to run

analyses_to_run <- as.data.frame(t(active_analyses))
analyses_to_run$subgroup <- row.names(analyses_to_run)
colnames(analyses_to_run) <- c("run","subgroup")
analyses_to_run<- analyses_to_run %>% filter(run=="TRUE" & subgroup != "active" ) 
rownames(analyses_to_run) <- NULL
analyses_to_run <- analyses_to_run %>% select(!run)
analyses_to_run$event=event_name

## Add in  all possible combinations of the subgroups, models and cohorts
analyses_to_run <- crossing(analyses_to_run,cohort)

## Add in which covariates to stratify by
analyses_to_run$stratify_by_subgroup=NA
for(i in c("ethnicity","sex")){
  analyses_to_run$stratify_by_subgroup <- ifelse(startsWith(analyses_to_run$subgroup,i),i,analyses_to_run$stratify_by_subgroup)
}
analyses_to_run$stratify_by_subgroup <- ifelse(startsWith(analyses_to_run$subgroup,"prior_history"),active_analyses$prior_history_var,analyses_to_run$stratify_by_subgroup)
analyses_to_run$stratify_by_subgroup <- ifelse(is.na(analyses_to_run$stratify_by_subgroup),analyses_to_run$subgroup,analyses_to_run$stratify_by_subgroup)


## Add in relevant subgroup levels to specify which stratum to run for
analyses_to_run$strata <- NA
analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="main","main",analyses_to_run$strata)
analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="covid_history","TRUE",analyses_to_run$strata)

for(i in c("covid_pheno_","agegp_","sex_","ethnicity_","prior_history_")){
  analyses_to_run$strata <- ifelse(startsWith(analyses_to_run$subgroup,i),gsub(i,"",analyses_to_run$subgroup),analyses_to_run$strata)
  
}

analyses_to_run$strata[analyses_to_run$strata=="South_Asian"]<- "South Asian"

# add subgroup category

analyses_to_run <- analyses_to_run %>% 
  dplyr::mutate(subgroup_cat = case_when(
    startsWith(subgroup, "agegp") ~ "age",
    startsWith(subgroup, "covid_history") ~ "covid_history",
    startsWith(subgroup, "covid_pheno") ~ "covid_pheno",
    startsWith(subgroup, "ethnicity") ~ "ethnicity",
    startsWith(subgroup, "main") ~ "main",
    startsWith(subgroup, "prior_history") ~ "prior_history",
    startsWith(subgroup, "sex") ~ "sex",
    TRUE ~ as.character(subgroup)))


## Separate into to dataframes as this will allow all the vaccinated/electively unvaccinated
## analyses to be run in one go to save having to read in the data for each individual analysis
## i.e can read it in once and run all the vaccinated analyses in one go

#for(i in cohort_to_run){
#  assign(paste0("analyses_to_run_",i),analyses_to_run %>% filter(cohort_to_run == i) )
#}
