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
outcomes_to_plot_prevax <- c("t2dm_extended_follow_up","t2dm_follow_extended_follow_up",
                             "t1dm_extended_follow_up","gestationaldm_extended_follow_up",
                             "otherdm_extended_follow_up")

outcomes_to_plot <- c("t2dm","t1dm","gestationaldm","otherdm")

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
                                  & ((event %in% outcomes_to_plot & cohort %in% c("vax","unvax")) | (event %in% outcomes_to_plot_prevax & cohort %in% c("prevax")))
                                  & term %in% term[grepl("^days",term)]
                                  & time_points == "reduced")%>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)


#----------------------Add empty rows for missing results-----------------------
df1 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("vax","unvax"),
                c("reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_28","days28_197"))

colnames(df1) <- c("event","subgroup","cohort","time_points","model","term")

df2 <- crossing(outcomes_to_plot_prevax,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("prevax"),
                c("reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_28","days28_197","days197_365","days365_714"))

colnames(df2) <- c("event","subgroup","cohort","time_points","model","term")

df<- rbind(df1,df2)

estimates <- df %>% left_join(estimates)

estimates <- estimates %>% filter((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                  | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) %>% 
  dplyr::mutate(across(c(estimate,conf_low,conf_high),as.numeric))

rm(df,df1,df2)
#------------------------------Tidy event names---------------------------------
estimates <- estimates %>% left_join(active_analyses %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

#-------------------------------Specify estimate format ------------------------  

estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                                                            " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                                                            "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))

# RENAME ALL OUTCOMES -----------------------------------------------------
unique(estimates$outcome)
estimates$outcome[estimates$outcome %in% c("type 1 diabetes","type 1 diabetes - extended follow up" )] <- "Type 1 diabetes"
estimates$outcome[estimates$outcome =="type 2 diabetes - persistant - extended follow up" ] <- "Type 2 diabetes - Persistent"
estimates$outcome[estimates$outcome %in% c("type 2 diabetes","type 2 diabetes - extended follow up" )] <- "Type 2 diabetes"
estimates$outcome[estimates$outcome %in% c("gestational diabetes","gestational diabetes - extended follow up")] <- "Gestational diabetes"
estimates$outcome[estimates$outcome %in% c("other or non-specific diabetes","other or non-specific diabetes - extended follow up")] <- "Other or non-specified diabetes"

# estimates$outcome[estimates$outcome == "type 2 diabetes - recovery"] <- "Type 2 diabetes - Recovery restricted"
# estimates$outcome[estimates$outcome == "type 2 diabetes - pre_recovery"] <- "Type 2 diabetes - Recovery pre"
# estimates$outcome[estimates$outcome == "type 2 diabetes - post_recovery"] <- "Type 2 diabetes - Recovery post"
# estimates$outcome[estimates$outcome == "type 2 diabetes - persistant"] <- "Type 2 diabetes - Persistant"
# estimates$outcome[estimates$outcome == "type 2 diabetes - pre diabetes"] <- "Type 2 diabetes - history of pre diabetes"
# estimates$outcome[estimates$outcome == "type 2 diabetes - no pre diabetes"] <- "Type 2 diabetes - no history of pre diabetes"
# estimates$outcome[estimates$outcome == "type 2 diabetes - obesity"] <- "Type 2 diabetes - history of obesity"
# estimates$outcome[estimates$outcome == "type 2 diabetes - no obesity"] <- "Type 2 diabetes - no history of obesity"



# Specify estimate order -----------------------------------------------------
estimates$outcome <- factor(estimates$outcome, levels=c("Type 2 diabetes",
                                                        "Type 2 diabetes - Persistent",
                                                        "Type 1 diabetes",
                                                        "Gestational diabetes",
                                                        "Other or non-specified diabetes")) 


estimates$subgroup <- ifelse(estimates$model == "mdl_max_adj" & estimates$subgroup == "main", "All COVID-19, maximally adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$model == "mdl_age_sex_region" & estimates$subgroup == "main", "All COVID-19, age/sex/region adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_hospitalised", "Hospitalised COVID-19, maximally adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_non_hospitalised", "Non-hospitalised COVID-19, maximally adjusted",estimates$subgroup)

estimates$subgroup <- factor(estimates$subgroup, levels = c("All COVID-19, age/sex/region adjusted",
                                                            "All COVID-19, maximally adjusted",
                                                            "Hospitalised COVID-19, maximally adjusted",
                                                            "Non-hospitalised COVID-19, maximally adjusted"))

estimates$cohort <- factor(estimates$cohort, levels=c("prevax","vax","unvax")) 
levels(estimates$cohort) <- list("Pre-vaccination cohort"="prevax", "Vaccinated cohort"="vax","Unvaccinated cohort"="unvax")


# Remove unnecessary variables -----------------------------------------------

estimates[,c("event","estimate","conf_low","conf_high","model","time_points")] <- NULL

# Convert long to wide -------------------------------------------------------

format_hr_table <- function(df){
  df <- tidyr::pivot_wider(df, names_from = cohort, values_from = est)
  df <- df %>% select("outcome","subgroup","term", "Pre-vaccination cohort","Vaccinated cohort","Unvaccinated cohort")
  
  df$term <- factor(df$term, levels = c("days0_28",
                                        "days28_197",
                                        "days197_365",
                                        "days365_714"))
  
  
  df <- df[order(df$outcome,df$subgroup,df$term),]
  df_persistent <- df %>% filter(outcome == "Type 2 diabetes - Persistent") %>% 
                          select(!c(`Vaccinated cohort`, `Unvaccinated cohort`)) %>%
                          rename("Pre-vaccination cohort - persistent" = "Pre-vaccination cohort")
  df_persistent$outcome <- "Type 2 diabetes"
  df <- df %>% left_join(df_persistent, by = c("outcome", "subgroup", "term")) %>%
                filter(outcome != "Type 2 diabetes - Persistent" ) %>%
                select("outcome","subgroup","term", "Pre-vaccination cohort","Pre-vaccination cohort - persistent","Vaccinated cohort","Unvaccinated cohort")
  
  write.csv(df, file = paste0(output_dir,"table3_formatted_hr.csv"),row.names = F)
}

format_hr_table(estimates)


# END