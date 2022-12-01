library(dplyr)
library(data.table)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
rm(list = ls())

results_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/model/"
output_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/generated-figures/"

dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)

#----------------------------Get CVD outcomes-----------------------------------

active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))


#---------------Focus on first 8 CVD outcomes (remove ate and vte)--------------
#outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name != c("ate","vte","ate_primary_position","vte_primary_position")]
outcomes_to_plot <- outcome_name_table$outcome_name

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

# Remove any hospitalised analyses ran with R

estimates <- estimates %>% filter(!(subgroup == "covid_pheno_hospitalised" & source == "R")) %>%
  filter(!(subgroup == "covid_pheno_non_hospitalised" & source == "stata"))

# Remove any main analyses ran with Stata

estimates <- estimates %>% filter(!(subgroup == "main" & source == "stata"))

estimates <- estimates %>% filter(((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                   | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) 
                                  & event %in% outcomes_to_plot 
                                  & term %in% term[grepl("^days",term)])%>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)

# remove duplicate rows
estimates <- estimates[!duplicated(estimates), ]

#----------------------Add empty rows for missing results-----------------------
df1 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("prevax","vax","unvax"),
                c("reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_28","days28_197","days197_535"))

colnames(df1) <- c("event","subgroup","cohort","time_points","model","term")

df2 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("prevax","vax","unvax"),
                c("normal"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_7","days7_14","days14_28","days28_56","days56_84","days84_197","days197_535"))

colnames(df2) <- c("event","subgroup","cohort","time_points","model","term")

df<- rbind(df1,df2)

estimates <- df %>% left_join(estimates)

estimates <- estimates %>% filter((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                  | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) %>% 
  dplyr::mutate(across(c(estimate,conf_low,conf_high),as.numeric))

rm(df1,df2,df)
#------------------------------Tidy event names---------------------------------
estimates <- estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

#-------------------------------Specify estimate format ------------------------  

estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                                                            " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                                                            "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))

# RENAME ALL OUTCOMES -----------------------------------------------------

estimates$outcome[estimates$outcome == "type 1 diabetes"] <- "Type 1 diabetes"
estimates$outcome[estimates$outcome == "type 2 diabetes"] <- "Type 2 diabetes"
estimates$outcome[estimates$outcome == "type 2 diabetes - recovery"] <- "Type 2 diabetes - Recovery restricted"
estimates$outcome[estimates$outcome == "type 2 diabetes - pre_recovery"] <- "Type 2 diabetes - Recovery pre"
estimates$outcome[estimates$outcome == "type 2 diabetes - post_recovery"] <- "Type 2 diabetes - Recovery post"
estimates$outcome[estimates$outcome == "type 2 diabetes - 4_mnth_follow"] <- "Type 2 diabetes - 4 month follow up"
estimates$outcome[estimates$outcome == "type 2 diabetes - pre diabetes"] <- "Type 2 diabetes - history of pre diabetes"
estimates$outcome[estimates$outcome == "type 2 diabetes - no pre diabetes"] <- "Type 2 diabetes - no history of pre diabetes"
estimates$outcome[estimates$outcome == "type 2 diabetes - obesity"] <- "Type 2 diabetes - history of obesity"
estimates$outcome[estimates$outcome == "type 2 diabetes - no obesity"] <- "Type 2 diabetes - no history of obesity"
estimates$outcome[estimates$outcome == "other or non-specific diabetes"] <- "Other or non-specified diabetes"
estimates$outcome[estimates$outcome == "gestational diabetes"] <- "Gestational diabetes"

# Specify estimate order -----------------------------------------------------
estimates$outcome <- factor(estimates$outcome, levels=c("Type 1 diabetes",
                                                        "Type 2 diabetes",
                                                        "Gestational diabetes",
                                                        "Other or non-specified diabetes",
                                                        "Type 2 diabetes - history of pre diabetes",
                                                        "Type 2 diabetes - no history of pre diabetes",
                                                        "Type 2 diabetes - history of obesity",
                                                        "Type 2 diabetes - no history of obesity",
                                                        "Type 2 diabetes - Recovery pre",
                                                        "Type 2 diabetes - Recovery post",
                                                        "Type 2 diabetes - Recovery restricted",
                                                        "Type 2 diabetes - 4 month follow up")) 


estimates$subgroup <- ifelse(estimates$model == "mdl_max_adj" & estimates$subgroup == "main", "All, maximally adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$model == "mdl_age_sex_region" & estimates$subgroup == "main", "All, age/sex/region adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_hospitalised", "Hospitalised COVID-19",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_non_hospitalised", "Non-hospitalised COVID-19",estimates$subgroup)

estimates$subgroup <- factor(estimates$subgroup, levels = c("All, age/sex/region adjusted",
                                                            "All, maximally adjusted",
                                                            "Hospitalised COVID-19",
                                                            "Non-hospitalised COVID-19"))

estimates$cohort <- factor(estimates$cohort, levels=c("prevax","vax","unvax")) 
levels(estimates$cohort) <- list("Pre-vaccination"="prevax", "Vaccinated"="vax","Unvaccinated"="unvax")


# Remove unnecessary variables -----------------------------------------------

estimates[,c("event","estimate","conf_low","conf_high","model")] <- NULL

# Convert long to wide -------------------------------------------------------

format_hr_table <- function(df, time_periods){
  df$time_points <- NULL
  df <- tidyr::pivot_wider(df, names_from = cohort, values_from = est)
  df <- df %>% select("outcome","subgroup","term", "Pre-vaccination","Vaccinated","Unvaccinated")
  
  if(grepl("reduced", time_periods)){
    df$term <- factor(df$term, levels = c("days0_28",
                                          "days28_197",
                                          "days197_535"))
    
  }else{
    df$term <- factor(df$term, levels = c("days0_7",
                                          "days7_14",
                                          "days14_28",
                                          "days28_56",
                                          "days56_84",
                                          "days84_197",
                                          "days197_535"))
  }
  
  df <- df[order(df$outcome,df$subgroup,df$term),]
  tmp <- as.data.frame(matrix(ncol=ncol(df),nrow=0))
  colnames(tmp) <- colnames(df)
  
  for (i in unique(df$outcome)) {
    df1 <- df %>% filter(outcome == i)
    tmp[nrow(tmp)+1,] <- i
    tmp <- rbind(tmp,df1)
  }
  
    # tmp[nrow(tmp)+1,] <- "Arterial thrombosis events"
    # tmp[nrow(tmp)+1,] <- "Venous thromboembolism events"
    # tmp[nrow(tmp)+1,] <- "Other vascular events"
  
    tmp$outcome <- factor(tmp$outcome, levels=c("Type 1 diabetes",
                                                            "Type 2 diabetes",
                                                            "Gestational diabetes",
                                                            "Other or non-specified diabetes",
                                                            "Type 2 diabetes - history of pre diabetes",
                                                            "Type 2 diabetes - no history of pre diabetes",
                                                            "Type 2 diabetes - history of obesity",
                                                            "Type 2 diabetes - no history of obesity",
                                                            "Type 2 diabetes - Recovery pre",
                                                            "Type 2 diabetes - Recovery post",
                                                            "Type 2 diabetes - Recovery restricted",
                                                            "Type 2 diabetes - 4 month follow up")) 
      
  tmp <- tmp[order(tmp$outcome),]
  tmp$outcome <- NULL
  tmp <- tmp %>% dplyr::rename("Event" = "subgroup")
  
  write.csv(tmp, file = paste0(output_dir,"table3_main_formatted_hr_",time_periods,"_time_periods.csv"),row.names = F)
}

estimates_reduced <- estimates %>% filter(time_points == "reduced" 
                                          & !outcome %in% outcome[grepl("Primary position",outcome)])

# estimates_reduced_primary_position <- estimates %>% filter(time_points == "reduced" 
#                                                            & outcome %in% outcome[grepl("Primary position",outcome)])

estimates_normal <- estimates %>% filter(time_points == "normal" 
                                         & !outcome %in% outcome[grepl("Primary position",outcome)])

# estimates_normal_primary_position <- estimates %>% filter(time_points == "normal" 
#                                                           & outcome %in% outcome[grepl("Primary position",outcome)])


format_hr_table(estimates_reduced,"reduced")
format_hr_table(estimates_normal,"normal")

# END