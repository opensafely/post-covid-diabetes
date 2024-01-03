library(dplyr)
library(data.table)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
rm(list = ls())

results_dir <- "C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/model/"
output_dir <- "C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/generated-tables/"

#dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)

#----------------------------Get CVD outcomes-----------------------------------

#active_analyses <- read_rds("lib/active_analyses.rds")
#active_analyses <- active_analyses %>%
#  dplyr::rename('outcome_variable' = 'outcome')

#active_analyses <- active_analyses %>% 
#  select(outcome, outcome_variable) %>% 
#  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))


#---------------Focus on first 8 CVD outcomes (remove ate and vte)--------------
outcomes_to_plot <- c("t2dm_extended_follow_up", "t2dm")

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/master_hr_file.csv"))
unique(estimates$event)

# remove unnecessary columns and models
estimates <- estimates %>% filter((subgroup %in% c("day0_main", "day0_sub_covid_hospitalised","day0_sub_covid_nonhospitalised") & model=="mdl_max_adj") 
                                  & event %in% outcomes_to_plot
                                  & term %in% term[grepl("^days",term)])%>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,model,N_events_midpoint6)

estimates <- subset(estimates, term!="days_pre")

#-------------------------------Specify estimate format ------------------------  

estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                                                            " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                                                            "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))

# Specify estimate order -----------------------------------------------------
estimates$subgroup <- ifelse(estimates$subgroup == "day0_main", "All",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "day0_sub_covid_hospitalised", "Hospitalised COVID-19",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "day0_sub_covid_nonhospitalised", "Non-hospitalised COVID-19",estimates$subgroup)

estimates$subgroup <- factor(estimates$subgroup, levels = c("All",
                                                            "Hospitalised COVID-19",
                                                            "Non-hospitalised COVID-19"))


#estimates$term <- ifelse(estimates$term == "days0_28","1-4","5-28")
#estimates$term <- factor(estimates$term, levels = c("1-4","5-28"))

estimates <- estimates[order(estimates$cohort,estimates$subgroup,estimates$term),]
# Remove unnecessary variables -----------------------------------------------

estimates[,c("event","estimate","conf_low","conf_high","model")] <- NULL

# Convert long to wide -------------------------------------------------------
estimates <- tidyr::pivot_wider(estimates, names_from = cohort, values_from = c("est", "N_events_midpoint6"))
estimates <- estimates[, c("subgroup", "term", "N_events_midpoint6_prevax", "est_prevax", "N_events_midpoint6_vax", "est_vax", "N_events_midpoint6_unvax", "est_unvax")] 
write.csv(estimates, file = paste0(output_dir,"supp_table6_day0.csv"),row.names = F)


# END