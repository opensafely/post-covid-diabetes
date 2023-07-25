post_covid_diabetes <- "~/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/"
analysis <- "main"

####  IMPORTANT - PLEASE READ ####
# This script will save results directly to the EHR sharepoint so will save over any results that are already there
# The lifetables will be saved into the directory aer_raw_output_dir. 
# To create a new folder, please change the 'date' variable to avoid overwriting any previous results
# To save from your own OneDrive, update the staff ID (currenlty zy21123) to your own
library(tidyverse)

# Calculates AER within age/sex subgroups

# Set file locations
aer_raw_output_dir <- "output/"

aer_compiled_output_dir <- "output/"
scripts_dir <- "analysis/model/"

# dir.create(file.path(aer_raw_output_dir), recursive =TRUE, showWarnings = FALSE)
# dir.create(file.path(aer_compiled_output_dir), recursive =TRUE, showWarnings = FALSE)

#-------------------------Call AER function-------------------------------------
source(file.path(scripts_dir,"Absolute_excess_risk_function.R"))

library(purrr)
library(data.table)
library(tidyverse)


#--------------------which analyses to calculate AER for------------------------

# we only want t2dm

#Define the active analyses
active <- readr::read_rds("lib/active_analyses.rds")                             
active <- active[active$outcome_variable=="out_date_t2dm",]   

active$event <- gsub("out_date_","",active$outcome_variable)                                                                                        
active[,c("active","outcome","outcome_variable","prior_history_var","covariates","model","cohort")] <- NULL       

active <- active %>%
  mutate_all(as.character)

#Converts to long-format
active <- tidyr::pivot_longer(active, 
                              cols = setdiff(colnames(active),c("event")), 
                              names_to = "strata")                                                                          
active <- active[active$value==TRUE, c("event","strata")]                               

active$model <- "mdl_max_adj"                 

#Add cohorts
active <- crossing(active, c("prevax","vax","unvax"))
colnames(active) <- c("event", "subgroup", "model", "cohort")

#Add time points
active$time_points <- "reduced"

#Focus only on aer analyses
#active <- active %>% filter(startsWith(subgroup,"aer_") & model=="mdl_max_adj")
active <- active %>% filter(model=="mdl_max_adj")

active <- active[active$subgroup==analysis,]
active$analysis <- active$subgroup

#Add HR time point terms so that results can be left joined
term <- c("days0_28","days28_197","days197_365", "days365_714")
results <- crossing(active,term)


#------------------------------------ Load results------------------------------
# to run change zy21123 to your personal staff ID
# In this file the t2dm_extended_follow_up outcomes for prevax have been renamed t2dm
input <- read_csv(paste0(post_covid_diabetes,"results/model/master_hr_file.csv"))

#-------------------Select required columns and term----------------------------
input <- input %>% 
  filter(str_detect(term, "^days")
         & (subgroup == "main")
         & model=="mdl_max_adj"
         & estimate != "[Redacted]"
         & time_points == "reduced"
         & ((event == "t2dm" & cohort %in% c("vax","unvax")) | (event == "t2dm_extended_follow_up" & cohort %in% c("prevax")))) %>%
  select(event,cohort,model,time_points,term,estimate)

input$event = "t2dm"

#---------------------------------Input Table 2---------------------------------
table2_pre_vax <- read.csv(paste0(post_covid_diabetes,"results/descriptive/table2_prevax_diabetes.csv"))
table2_pre_vax <- table2_pre_vax %>% filter(event == "out_date_t2dm_extended_follow_up")

table2_vax <- read.csv(paste0(post_covid_diabetes,"results/descriptive/table2_vax_diabetes.csv"))
table2_vax <- table2_vax %>% filter(event == "out_date_t2dm")

table2_unvax <- read.csv(paste0(post_covid_diabetes,"results/descriptive/table2_unvax_diabetes.csv"))
table2_unvax <- table2_unvax %>% filter(event == "out_date_t2dm")


table_2 <- rbind(table2_pre_vax, table2_vax,table2_unvax)
table_2 <- table_2 %>% rename(cohort = cohort_to_run)
rm(table2_pre_vax,table2_vax,table2_unvax)

#-------------------Select required columns and term----------------------------

table_2 <- table_2 %>% select(subgroup, event, cohort,unexposed_person_days,unexposed_event_count,total_covid19_cases, N_population_size) %>%
  filter(startsWith(subgroup, "aer_"))

table_2$event <- gsub("_extended_follow_up","",table_2$event)
table_2$event <- gsub("out_date_","",table_2$event)

table_2$analysis <- analysis

# Join HRs onto active df

results$subgroup <- NULL
results <- results %>% left_join(input, by=c("event","cohort","model","time_points","term"))
results <- results %>% filter(!is.na(estimate))

#Join on table 2 event counts
results <- merge(results,table_2, by=c("event","cohort","analysis"))

results <- results %>% mutate(across(c(estimate, unexposed_person_days, unexposed_event_count, total_covid19_cases), as.numeric))

#-------------------------Run AER function--------------------------------------

active$subgroup <- paste0(unique(table_2$subgroup), collapse = ";")
active <- tidyr::separate_rows(active, subgroup, sep = ";")

lapply(split(active,seq(nrow(active))),
       function(active)
         excess_risk(   
           event_of_interest = active$event,
           cohort_of_interest = active$cohort,
           model_of_interest = active$model,
           subgroup_of_interest = active$subgroup,
           time_point_of_interest = active$time_points,
           input = results))

#------------------------------Compile the results------------------------------
AER_files=list.files(path = aer_raw_output_dir, pattern = "lifetable_")
AER_files=paste0(aer_raw_output_dir,"/",AER_files)
AER_compiled_results <- purrr::pmap(list(AER_files),
                                    function(fpath){
                                      df <- fread(fpath)
                                      return(df)})
AER_compiled_results=rbindlist(AER_compiled_results, fill=TRUE)

# Calculate overall AER
AER_combined <- AER_compiled_results %>% select(days, event, cohort, subgroup, time_points, cumulative_difference_absolute_excess_risk)
table_2 <- table_2 %>% select(event, cohort, subgroup, N_population_size)

# AER_combined <- AER_combined %>% left_join(table_2, by=c("event","cohort","subgroup"))

#Standardize AER and use pre-vax subgroup sizes for all cohorts
table_2 <- table_2 %>% filter(cohort == "prevax") %>% select(!cohort)
table_2$weight <- table_2$N_population_size/sum(table_2$N_population_size)
write.csv(table_2,file="table2.csv")
AER_combined <- AER_combined %>% left_join(table_2 %>% select(event,subgroup,weight), by=c("event","subgroup"))

AER_combined <- AER_combined %>% filter(!is.na(cumulative_difference_absolute_excess_risk))

AER_combined <- AER_combined %>% 
  dplyr::group_by(days, event, cohort,time_points) %>%
  dplyr::summarise(weighted_mean = weighted.mean(cumulative_difference_absolute_excess_risk,weight))


#Join all results together 
AER_combined$subgroup <- "aer_overall"

AER_combined <- AER_combined %>% dplyr::rename(cumulative_difference_absolute_excess_risk = weighted_mean )

AER_compiled_results <- rbind(AER_compiled_results,AER_combined, fill = TRUE)

write.csv(AER_compiled_results, paste0(aer_compiled_output_dir,"AER_compiled_results.csv"), row.names = F)
