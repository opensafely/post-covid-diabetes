#Format stata output ready for plotting
library(stringi)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(tidyverse)

# Read in results from stata output

stata_results_dir <- "output"
r_results_dir_prevax <- "output/review/model/prevax"
r_results_dir_vax <- "output/review/model/prevax"
r_results_dir_unvax <- "output/review/model/prevax"
output_dir <- "output/review/model"

print(getwd())

df <- read.csv(paste0(stata_results_dir,"/stata_output.csv"))
# df_prevax <- read.csv(paste0(results_dir,"/stata_output_prevax.csv"))
# df <- rbind(df, df_prevax)
# df$X <- NULL
# 
# rm(df_prevax)

active_analyses <- read_rds("lib/active_analyses.rds")

## Transpose active_analyses to single column so can filter to analysis models to run
subgroup <- as.data.frame(t(active_analyses[1,]))
subgroup$subgroup <- row.names(subgroup)
colnames(subgroup) <- c("run","subgroup")
subgroup<- subgroup %>% filter((run=="TRUE" | run == "FALSE") & subgroup != "active" ) 
rownames(subgroup) <- NULL
subgroup <- subgroup %>% select(!run)
subgroup$subgroup <- paste0("_",subgroup$subgroup)

# Get cohort
df$cohort <- ifelse(grepl("unvax",df$source),"unvax", ifelse(grepl("prevax",df$source),"prevax","vax"))
unique(df$cohort)

# Get outcome event name
df$event <- df$source
df$event <- gsub("input_sampled_data_","",df$event)
df$event <- sub('\\_unvax.*', '', df$event)
df$event <- sub('\\_vax.*', '', df$event)
df$event <- sub('\\_prevax.*', '', df$event)
df$event <- stri_replace_all_regex(df$event,
                                   pattern=subgroup$subgroup,
                                   replacement=c(""),
                                   vectorize=FALSE)
unique(df$event)

# Get subgroup
df$subgroup <- df$source
df$subgroup <- str_replace(df$subgroup,paste0("input_sampled_data_", df$event,"_"),"")
df$subgroup <- sub('\\_unvax.*', '', df$subgroup)
df$subgroup <- sub('\\_vax.*', '', df$subgroup)
df$subgroup <- sub('\\_prevax.*', '', df$subgroup)
unique(df$subgroup)

# Rename model
df$model <- ifelse(df$model == "max", "mdl_max_adj","mdl_age_sex_region")

#Fomat columns
df$time_points <- "reduced"
df$results_fitted <- "fitted_successfully"
df$source <- NULL
df$N_outcomes <- NULL

#Exponentiate results
df$estimate <- exp(df$estimate)
df$conf_low <- exp(df$conf_low)
df$conf_high <- exp(df$conf_high)

#Some results have been run twice (once in stata and once in R so remove duplicates)
#Only use results that are in the analyses_to_run_in_stata files

stata_analyses <- read_csv("lib/analyses_to_run_in_stata.csv")
stata_analyses <- stata_analyses %>% dplyr::rename(time_points=time_periods,
                                                   event = outcome)

stata_analyses$subgroup <- ifelse(stata_analyses$subgroup=="hospitalised","covid_pheno_hospitalised",stata_analyses$subgroup)
stata_analyses$subgroup <- ifelse(stata_analyses$subgroup=="non_hospitalised","covid_pheno_non_hospitalised",stata_analyses$subgroup)

df <- merge(df,stata_analyses, by=c("event","subgroup","cohort","time_points"))

#Previous time period days have been added to the median which hasn't been done in the R HRs and gets done
# in the figure scripts. Removing here so that everything is the same
# df$remove_from_median <- NA
# df$remove_from_median <- ifelse(grepl("days",df$term),df$term,df$remove_from_median)
# df$remove_from_median <- sub("days","",df$remove_from_median)
# df$remove_from_median <- as.numeric(sub("\\_.*","",df$remove_from_median))
# 
# df$median_follow_up <- df$median_follow_up - df$remove_from_median

df$source <- "stata"

print("Stata part of script ran successfully")
#Read in R HRs

# PREVAX 

print(getwd())

hr_files=list.files(path = "output/review/model/prevax/", pattern = "to_release")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0("output/review/model/prevax/", hr_files)
hr_file_paths <- pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
estimates <- rbindlist(hr_file_paths, fill=TRUE)

# VAX

hr_files_vax=list.files(path = "output/review/model/vax/", pattern = "to_release")
hr_files_vax=hr_files_vax[endsWith(hr_files_vax,".csv")]
hr_files_vax=paste0("output/review/model/vax/", hr_files_vax)
hr_file_paths_vax <- pmap(list(hr_files_vax),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
estimates_vax <- rbindlist(hr_file_paths_vax, fill=TRUE)

# UNVAX

hr_files_unvax=list.files(path = "output/review/model/unvax/", pattern = "to_release")
hr_files_unvax=hr_files_unvax[endsWith(hr_files_unvax,".csv")]
hr_files_unvax=paste0("output/review/model/unvax/", hr_files_unvax)
hr_file_paths_unvax <- pmap(list(hr_files_unvax),
                          function(fpath){
                            df <- fread(fpath)
                            return(df)
                          })
estimates_unvax <- rbindlist(hr_file_paths_unvax, fill=TRUE)

estimates <- do.call("rbind", list(estimates, estimates_vax, estimates_unvax))

estimates$source <- "R"

df <- df %>% select(intersect(colnames(estimates),colnames(df)))
estimates <- rbind(estimates, df, fill = TRUE)
rm(df)

#If any of the models has fitted unsuccessfully, class all models as fitted unsuccessfully
estimates <- estimates %>%
  group_by(event,cohort,subgroup,time_points, source) %>%
  dplyr::mutate(results_fitted = case_when(
    any(results_fitted == "fitted_unsuccessfully") ~ "fitted_unsuccessfully",
    TRUE ~ "fitted_successfully")) %>% ungroup()

#Filter to columns and terms of interest
estimates <- estimates %>% filter(term %in% term[grepl("^days",term)]
                                  & results_fitted == "fitted_successfully") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up, model, source)

#Set any redacted values to NA
estimates <- estimates %>%
  mutate(across(c("estimate","conf_low","conf_high","median_follow_up"), ~ na_if(., "[Redacted]")))

estimates <- estimates %>% dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))

#Calculate median follow-up for plotting
estimates$median_follow_up <- as.numeric(estimates$median_follow_up)
estimates$add_to_median <- sub("days","",estimates$term)
estimates$add_to_median <- as.numeric(sub("\\_.*","",estimates$add_to_median))

estimates$median_follow_up <- ((estimates$median_follow_up + estimates$add_to_median)-1)/7
estimates$add_to_median <- NULL

estimates <- as.data.frame(estimates)
df <- estimates %>% select(term,event,subgroup,cohort, time_points,model)
df <- as.data.frame(df)
df <- df[duplicated(df),]

df <- merge(estimates,df)
df <- df %>% filter(source == "stata")

df <- df[!(df$cohort == "vax" & df$subgroup == "covid_pheno_hospitalised"),] 


# estimates <- estimates %>% anti_join(df)

#Left join event counts
table2_pre_vax <- read.csv("output/review/descriptives/table2_prevax_diabetes.csv")
table2_vax <- read.csv("output/review/descriptives/table2_vax_diabetes.csv")
table2_unvax <- read.csv("output/review/descriptives/table2_unvax_diabetes.csv")

# table2_pre_vax <- table2_pre_vax %>% rename(cohort_to_run = cohort_name)
table2 <- rbind(table2_unvax,table2_vax,table2_pre_vax)
table2 <- table2 %>% dplyr::rename(cohort = cohort_to_run)
table2 <- table2 %>% dplyr::select(event, subgroup, cohort, post_exposure_event_count)
table2$event <- gsub("out_date_","",table2$event)

estimates$post_exposure_event_count <- NULL
estimates <- estimates %>% left_join(table2) %>%
  select(event, subgroup, cohort, model, time_points, source,term, estimate, conf_low, conf_high, post_exposure_event_count, median_follow_up)

write.csv(estimates, file = paste0(output_dir,"/hr_output_formatted.csv"),row.names = FALSE)