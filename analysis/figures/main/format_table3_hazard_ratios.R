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

#----------------------------Get Diabetes outcomes-----------------------------------

#active_analyses <- read_rds("lib/active_analyses.rds")# %>% filter(active == "TRUE")

#active_analyses <- active_analyses %>% 
#  select(outcome) %>% 
#  mutate(outcome_name=active_analyses$outcome %>% str_replace("out_date_", ""))


#---------------Focus on main and hospitalised phenotypes--------------
outcomes_to_plot_prevax <- c("gestationaldm_extended_follow_up","otherdm_extended_follow_up",
                             "t1dm_extended_follow_up","t2dm_extended_follow_up","t2dm_follow_extended_follow_up"                         )

outcomes_to_plot <- c("gestationaldm","otherdm","t1dm","t2dm")

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/master_hr_file.csv"))
unique(estimates$event)
unique(estimates$outcome_name)

# Remove days_pre
estimates <- estimates %>% filter(!(term == "days_pre"))

# Restrict to outcomes needed for Table 3
estimates <- estimates %>% filter(estimates$event %in% c(outcomes_to_plot, outcomes_to_plot_prevax))

# Models included in Table 3
table3 <- estimates %>% filter(subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex")
                            | (subgroup %in% c("sub_covid_hospitalised","sub_covid_nonhospitalised") & model=="mdl_max_adj")) %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,model,outcome_name)

#----------------------Add empty rows for missing results-----------------------

input <- estimates %>% filter((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex"))
                                  | (subgroup %in% c("sub_covid_hospitalised","sub_covid_nonhospitalised") & model=="mdl_max_adj")) %>% 
  dplyr::mutate(across(c(estimate,conf_low,conf_high),as.numeric))


#-------------------------------Specify estimate format ------------------------  

input$est <- ifelse(is.na(input$estimate),NA,paste0(ifelse(input$estimate>=10,sprintf("%.1f",input$estimate),sprintf("%.2f",input$estimate)),
                                                            " (",ifelse(input$conf_low>=10,sprintf("%.1f",input$conf_low),sprintf("%.2f",input$conf_low)),
                                                            "-",ifelse(input$conf_high>=10,sprintf("%.1f",input$conf_high),sprintf("%.2f",input$conf_high)),")"))

# Specify estimate order -----------------------------------------------------
input$outcome_name <- factor(input$outcome_name, levels=c("Type 2 diabetes",
                                                        "Type 2 diabetes - Persistent",
                                                        "Type 1 diabetes",
                                                        "Gestational diabetes",
                                                        "Other or non-specified diabetes")) 


input$subgroup <- ifelse(input$model == "mdl_max_adj" & input$subgroup == "main", "All COVID-19, maximally adjusted",input$subgroup)
input$subgroup <- ifelse(input$model == "mdl_age_sex" & input$subgroup == "main", "All COVID-19, age/sex/region adjusted",input$subgroup)
input$subgroup <- ifelse(input$subgroup == "sub_covid_hospitalised", "Hospitalised COVID-19, maximally adjusted",input$subgroup)
input$subgroup <- ifelse(input$subgroup == "sub_covid_nonhospitalised", "Non-hospitalised COVID-19, maximally adjusted",input$subgroup)

input$subgroup <- factor(input$subgroup, levels = c("All COVID-19, age/sex/region adjusted",
                                                            "All COVID-19, maximally adjusted",
                                                            "Hospitalised COVID-19, maximally adjusted",
                                                            "Non-hospitalised COVID-19, maximally adjusted"))

input$cohort <- factor(input$cohort, levels=c("prevax","vax","unvax")) 
levels(input$cohort) <- list("Pre-vaccination cohort"="prevax", "Vaccinated cohort"="vax","Unvaccinated cohort"="unvax")


# Remove unnecessary variables -----------------------------------------------

input[,c("estimate","conf_low","conf_high","model")] <- NULL

input <- input[, c("cohort","outcome_name","event","subgroup","term","est")] 

# Convert long to wide -------------------------------------------------------

format_hr_table <- function(df){
  df <- tidyr::pivot_wider(df, names_from = cohort, values_from = est)
  df <- df %>% select("outcome_name","subgroup","term", "Pre-vaccination cohort","Vaccinated cohort","Unvaccinated cohort")
  
  df$term <- factor(df$term, levels = c("days0_28",
                                        "days28_197",
                                        "days197_365",
                                        "days365_714"))
  
  df <- df[order(df$outcome_name,df$subgroup,df$term),]
  df_persistent <- df %>% filter(outcome_name == "Type 2 diabetes - Persistent") %>% 
                          select(!c(`Vaccinated cohort`, `Unvaccinated cohort`)) %>%
                          rename("Pre-vaccination cohort - persistent" = "Pre-vaccination cohort")
  df_persistent$outcome_name <- "Type 2 diabetes"
  df <- df %>% left_join(df_persistent, by = c("outcome_name", "subgroup", "term")) %>%
                filter(outcome_name != "Type 2 diabetes - Persistent" ) %>%
                select("outcome_name","subgroup","term", "Pre-vaccination cohort","Pre-vaccination cohort - persistent","Vaccinated cohort","Unvaccinated cohort")
  
  write.csv(df, file = paste0(output_dir,"table3_formatted_hr.csv"),row.names = F)
}

format_hr_table(input)


# END