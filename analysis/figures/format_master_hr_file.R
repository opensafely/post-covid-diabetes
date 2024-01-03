#Format stata output ready for plotting
library(stringi)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(tidyverse)

# You will need to ensure that you have created a shortcut to the group-EHR sharepoint in your OneDrive folder to 
# run this script
# Change user ID  to your own
staff_ID <- "rd16568"

# Read in results from stata output
df <- readr::read_csv(paste0("C:/Users/",staff_ID,"/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/model/stata_model_output_midpoint6.csv"))

#active_analyses <- read_rds("lib/active_analyses.rds")

## Select variables from active analysis  so can filter to analysis models to run
#analysis <- active_analyses %>%
#            select(cohort, outcome, analysis) %>%
#            mutate(outcome_name=active_analyses$outcome %>% str_replace("out_date_", ""))
  


# Get subgroup
#df$subgroup <- df$source
#df$subgroup <- str_replace(df$subgroup,paste0("input_sampled_data_", df$event,"_"),"")
#df$subgroup <- sub('\\_unvax.*', '', df$subgroup)
#df$subgroup <- sub('\\_vax.*', '', df$subgroup)
#df$subgroup <- sub('\\_prevax.*', '', df$subgroup)
#unique(df$subgroup)

# Rename model
#unique(df$model)
#df$model <- ifelse(df$model == "max", "mdl_max_adj","mdl_age_sex_region")

#Format columns
#df$time_points <- "reduced"
# Flag day_zero analysis
#df$time_points <- ifelse(grepl("day0TRUE",df$source),"day_zero_reduced",df$time_points)
df$results_fitted <- "fitted_successfully"
df$source <- NULL
#df$N_outcomes <- NULL
#df$timepoints <- df %>% 
#          select(term) %>%
#          mutate(timepoint=term %>% str_replace("days", ""))

# Filter to HR for time points   
df <- df %>% filter(str_detect(term,"days"))

#Exponentiate results
#df <- df %>% mutate(across(c(estimate,conf_low,conf_high,median_time_to_event),as.numeric))
#df$estimate <- exp(df$estimate)
#df$conf_low <- exp(df$conf_low)
#df$conf_high <- exp(df$conf_high)

#Some results have been run twice (once in stata and once in R so remove duplicates)
#Only use results that are in the analyses_to_run_in_stata files

# stata_analyses <- read_csv("lib/analyses_to_run_in_stata.csv")
# stata_analyses_day_zero <- read_csv("lib/analyses_to_run_in_stata_day_zero.csv")
# stata_analyses_pre_vax <- read_csv("lib/analyses_to_run_in_stata_pre_vax.csv")
# 
# stata_analyses_pre_vax$time_periods <- ifelse(stata_analyses_pre_vax$day0 == TRUE,"day_zero_reduced",stata_analyses_pre_vax$time_periods)
# stata_analyses_pre_vax[c("day0","extf")] <- NULL
# 
# stata_analyses <- rbind(stata_analyses,stata_analyses_day_zero,stata_analyses_pre_vax)
# 
# rm(stata_analyses_day_zero,stata_analyses_pre_vax)
# stata_analyses <- stata_analyses %>% dplyr::rename(time_points=time_periods,
#                                                    event = outcome)
# 
# stata_analyses$subgroup <- ifelse(stata_analyses$subgroup=="hospitalised","covid_pheno_hospitalised",stata_analyses$subgroup)
# stata_analyses$subgroup <- ifelse(stata_analyses$subgroup=="non_hospitalised","covid_pheno_non_hospitalised",stata_analyses$subgroup)
# 
# 
# df <- merge(df,stata_analyses, by=c("event","subgroup","cohort","time_points"))

#Previous time period days have been added to the median which hasn't been done in the R HRs and gets done
# in the figure scripts. Removing here so that everything is the same
#df$remove_from_median <- NA
#df$remove_from_median <- ifelse(grepl("days",df$term),df$term,df$remove_from_median)
#df$remove_from_median <- sub("days","",df$remove_from_median)
#df$remove_from_median <- as.numeric(sub("\\_.*","",df$remove_from_median))

#df$median_follow_up <- df$median_time_to_event - df$remove_from_median

df$source <- "stata"

#Read in R HRs
estimates <- readr::read_csv(paste0("C:/Users/",staff_ID,"/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/model/model_output_midpoint6.csv"))
#estimates <- estimates %>% filter(model != "mdl_age_sex")
estimates$results_fitted <- ""
estimates$source <- "R"

# Filter to HR for time points   
estimates <- estimates %>% filter(str_detect(term,"days"))

# identifying models fitted unsuccessfully
estimates <- estimates %>%
  mutate(results_fitted = case_when(conf_high=="Inf" ~ "fitted_unsuccessfully",
                                    TRUE ~ ""))

#If any of the models has fitted unsuccessfully, class all models as fitted unsuccessfully
estimates <- estimates %>%
  group_by(outcome,cohort,analysis, model) %>%
  dplyr::mutate(results_fitted = case_when(
    any(results_fitted == "fitted_unsuccessfully") ~ "fitted_unsuccessfully",
    TRUE ~ "fitted_successfully")) %>% ungroup()

# bringing R and stata together
df <- df %>% select(intersect(colnames(estimates),colnames(df)))
estimates <- estimates %>% select(intersect(colnames(estimates),colnames(df)))
estimates <- rbind(estimates, df, fill = TRUE)
rm(df)

#Check that all models that fit unsuccessfully have been run in stata
#estimates_unsuccessful <- estimates %>% filter(term %in% term[grepl("^days",term)]
#                                               & results_fitted == "fitted_unsuccessfully"
#                                               & !time_points %in% time_points[grepl("normal",time_points)])%>%
#  select(event,subgroup,cohort,time_points) %>% distinct()

#stata_analyses <- read_csv("lib/analyses_to_run_in_stata.csv")
#tmp <- estimates_unsuccessful %>% anti_join(stata_analyses)

#Filter to columns and terms of interest
estimates <- estimates %>% filter(term %in% term[grepl("^days",term)]
                                  & results_fitted == "fitted_successfully") 

# Duplicates - keeping if model in R
estimates <- estimates %>% 
  dplyr::group_by(cohort,outcome,analysis,model,term) %>% 
  dplyr::mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(is_duplicated = dup_id > 1)

estimates <- subset(estimates, dup_id==1 | (dup_id==2 & source=="R"))

estimates <- estimates %>% dplyr::mutate(across(c(hr,conf_low,conf_high,outcome_time_median),as.numeric))
estimates <-select(estimates, cohort,outcome,analysis,model,term,hr,conf_low,conf_high,N_events_midpoint6,N_exposed_midpoint6,N_total_midpoint6,outcome_time_median,person_time_total)
  
#Set any redacted values to NA
#estimates <- estimates %>%
#  mutate(across(c("hr","conf_low","conf_high","outcome_time_median"), ~ na_if(., "[Redacted]")))


#Calculate median follow-up in weeks for plotting
estimates$median_follow_up <- (estimates$outcome_time_median)/7
estimates$add_to_median <- NULL

estimates <- as.data.frame(estimates)
estimates <- estimates[!duplicated(estimates), ]

#df <- estimates %>% select(term,event,subgroup,cohort, time_points,model)
#df <- as.data.frame(df)
#df <- df[duplicated(df),]

#df <- merge(estimates,df)
#df <- df %>% filter(source == "stata")

#estimates <- estimates %>% anti_join(df)

#Left join event counts
#table2=list.files(path = paste0("C:/Users/",staff_ID,"/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/descriptive/"), pattern = "table2*")

#table2=paste0("C:/Users/",staff_ID,"/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/descriptive/",table2)
#table2 <- pmap(list(table2),
#               function(fpath){
#                 df <- fread(fpath)
#                 return(df)
##                 })
#table2 <- rbindlist(table2, fill=TRUE)

#table2 <- table2 %>% dplyr::rename(cohort = cohort_to_run)
#table2 <- table2 %>% select(event, subgroup, cohort, post_exposure_event_count)
#table2$event <- gsub("out_date_","",table2$event)

#table2_main <- table2 %>% filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))
#table2_main$post_exposure_event_count <- as.numeric(table2_main$post_exposure_event_count)
#table2_main <- table2_main %>% filter(!is.na(post_exposure_event_count))
#table2_main <- table2_main %>% group_by(event,cohort) %>% 
#  summarise(post_exposure_event_count = sum(post_exposure_event_count))

#table2_main$subgroup <- "main"
#table2 <- rbind(table2,table2_main)

#estimates$post_exposure_event_count <- NULL
#estimates <- estimates %>% left_join(table2) %>%
#  select(event, subgroup, cohort, model, time_points, source,term, estimate, conf_low, conf_high, post_exposure_event_count, median_follow_up)

#renaming to align with existing table and figure scripts
estimates <- dplyr::rename(estimates,
                    "estimate" = "hr",
                    "subgroup" = "analysis",
                    "event" = "outcome")

#new outcome name
estimates <- estimates %>%  
   mutate(outcome_name = case_when(grepl("t1dm", event) ~ "Type 1 diabetes",
                                   grepl("t2dm", event) & !grepl("t2dm_follow_extended_follow_up",event) ~ "Type 2 diabetes",
                                   grepl("t2dm_follow_extended_follow_up", event) ~ "Type 2 diabetes - Persistent",
                                   grepl("gest", event) ~ "Gestational diabetes",
                                   grepl("other", event, ignore.case = TRUE) ~ "Other or non-specified diabetes"))

write.csv(estimates, file = paste0("C:/Users/",staff_ID,"/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/model/master_hr_file.csv"),row.names = FALSE)
