library(dplyr)
library(data.table)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
rm(list = ls())

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/model/"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/generated-tables/"

#dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)

#----------------------------Get CVD outcomes-----------------------------------

active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

active_analyses <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))


#---------------Focus on first 8 CVD outcomes (remove ate and vte)--------------
outcomes_to_plot <- c("t2dm","t2dm_unvax_sens")

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/master_hr_file.csv"))
unique(estimates$event)
# Remove any hospitalised analyses ran with R

# estimates <- estimates %>% filter(!(subgroup == "covid_pheno_hospitalised" & source == "R")) %>%
#   filter(!(subgroup == "covid_pheno_non_hospitalised" & source == "stata"))

# Remove any main analyses ran with Stata

#estimates <- estimates %>% filter(!(subgroup == "main" & source == "stata"))
estimates <- estimates %>% filter(((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                   | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) 
                                  & event %in% outcomes_to_plot
                                  & cohort == "unvax"
                                  & term %in% term[grepl("^days",term)]
                                  & time_points == "reduced")%>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)


#-------------------------------Specify estimate format ------------------------  

estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                                                            " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                                                            "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))

# Specify estimate order -----------------------------------------------------
estimates$subgroup <- ifelse(estimates$model == "mdl_max_adj" & estimates$subgroup == "main", "All COVID-19, maximally adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$model == "mdl_age_sex_region" & estimates$subgroup == "main", "All COVID-19, age/sex/region adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_hospitalised", "Hospitalised COVID-19, maximally adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_non_hospitalised", "Non-hospitalised COVID-19, maximally adjusted",estimates$subgroup)

estimates$subgroup <- factor(estimates$subgroup, levels = c("All COVID-19, age/sex/region adjusted",
                                                            "All COVID-19, maximally adjusted",
                                                            "Hospitalised COVID-19, maximally adjusted",
                                                            "Non-hospitalised COVID-19, maximally adjusted"))

estimates$cohort <- ifelse(estimates$event == "t2dm", "Unvaccinated cohort","Unvaccinated cohort without censoring at vaccination")


estimates$cohort <- factor(estimates$cohort, levels=c("Unvaccinated cohort","Unvaccinated cohort without censoring at vaccination")) 

estimates$term <- ifelse(estimates$term == "days0_28","1-4","5-28")
estimates$term <- factor(estimates$term, levels = c("1-4","5-28"))

estimates <- estimates[order(estimates$cohort,estimates$subgroup,estimates$term),]
# Remove unnecessary variables -----------------------------------------------

estimates[,c("event","estimate","conf_low","conf_high","model","time_points")] <- NULL

# Convert long to wide -------------------------------------------------------
estimates <- tidyr::pivot_wider(estimates, names_from = cohort, values_from = est)
estimates <-  estimates %>% select(subgroup, term, `Unvaccinated cohort`,`Unvaccinated cohort without censoring at vaccination`)


write.csv(estimates, file = paste0(output_dir,"supp_table5_unvax_sensitivity.csv"),row.names = F)


# END