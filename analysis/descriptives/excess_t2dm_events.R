library(dplyr)
library(readr)
library(stringr)
library(tidyr)

aer_output_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/model/AER/"
aer_output_fig <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/generated-figures/"
results_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/descriptive/"

#Read in AER
df <- readr::read_csv(paste0(aer_output_dir,"/AER_compiled_results.csv"))

#Select max day of follow up (196 for vax/unvax, 534 for pre-vax)
# df <- df %>%
#   group_by(event, subgroup, time_points, cohort) %>%
#   filter(days == max(days) & subgroup != "aer_overall" & !is.na(AER_main) ) %>%
#   select(event, subgroup, cohort, subgroup, AER_main, time_points, days)%>% ungroup()

df <- df %>% filter(days == 196 & subgroup != "aer_overall") %>%
  filter(time_points == "reduced")
  distinct()

#Sum over all subgroups to get total
df <- df %>% 
  group_by(event, time_points, cohort) %>%
  summarise(total = sum(AER_main))

#Add person years of follow up
table2_pre_vax <- read.csv(paste0(results_dir,"table2_prevax_diabetes_extended.csv"))
table2_vax <- read.csv(paste0(results_dir,"table2_vax_diabetes.csv"))
table2_unvax <- read.csv(paste0(results_dir,"table2_unvax_diabetes.csv"))

table2_pre_vax <- dplyr::rename(table2_pre_vax, cohort = cohort_to_run)
table2_vax <- dplyr::rename(table2_vax, cohort = cohort_to_run)
table2_unvax <- dplyr::rename(table2_unvax, cohort = cohort_to_run)

table_2 <- plyr::rbind.fill(table2_pre_vax, table2_vax,table2_unvax)
rm(table2_pre_vax,table2_vax,table2_unvax)

table_2$event <- gsub("_extended_follow_up","",table_2$event)

#To get total follow up for main analysis need to sum unexposed person days of follow up & exposed person days
table_2 <- table_2 %>% select(event, subgroup, cohort, total_person_days, unexposed_person_days,total_person_days_to_day_197) %>% 
  filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))

table_2$exposed_person_days <- ifelse(table_2$cohort != "prevax", table_2$total_person_days - table_2$unexposed_person_days, table_2$total_person_days_to_day_197)
table_2$total_person_days <- NULL
table_2$unexposed_person_days <- NULL
table_2$total_person_days_to_day_197 <- NULL

#Sum follow up
table_2 <- table_2 %>% group_by(event, cohort) %>%
  summarise(total_exposed_follow_up = sum(exposed_person_days))

#Convert from days to years
table_2$total_exposed_follow_up <- table_2$total_exposed_follow_up/365.2
table_2$event <- gsub("out_date_","",table_2$event)

#Left join follow up onto AER table
df <- df %>% left_join(table_2, by = c("event"="event", "cohort"="cohort"))
df$excess_events_per_1000_person_years <- df$total * (1000/df$total_exposed_follow_up)

df <- pivot_longer(df, cols = c(total, total_exposed_follow_up, excess_events_per_1000_person_years), names_to = "summary", values_to = "total")
df <- pivot_wider(df, names_from = cohort, values_from = total)

df <- df %>% rename("Pre-vaccination cohort"="prevax",
                    "Vaccinated cohort"="vax",
                    "Unvaccinated cohort"="unvax")

df$summary <- ifelse(df$summary == "total", "Total excess events",df$summary)
df$summary <- ifelse(df$summary == "total_exposed_follow_up", "Total post exposure follow up (years)",df$summary)
df$summary <- ifelse(df$summary == "excess_events_per_1000_person_years", "Excess events per 1000 person years",df$summary)

#Get tidy names
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))%>%
  select(outcome, outcome_name)

df <- df %>% left_join(outcome_name_table, by = c("event"="outcome_name"))
df$event <- NULL
df <- df %>% select(outcome,summary,time_points, `Pre-vaccination cohort`, `Vaccinated cohort`, `Unvaccinated cohort`)

# reduced only 

df <- df %>% dplyr::filter(time_points == "reduced")

write.csv(df, paste0(aer_output_fig,"excess_t2dm_events.csv"),row.names = F)
    

