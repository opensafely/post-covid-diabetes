library(dplyr)
library(readr)
library(stringr)
library(tidyr)

#aer_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/AER/compiled_results/"
#results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"

#Read in AER
#df <- readr::read_csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/model/AER/AER_compiled_results_extended.csv")
df <- readr::read_csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/model/AER//compiled_results_for_plotting/18_05_2023/AER_compiled_results.csv")

#Select max day of follow up (196 for vax/unvax, 534 for pre-vax)
# df <- df %>%
#   group_by(event, subgroup, time_points, cohort) %>%
#   filter(days == max(days) & subgroup != "aer_overall" & !is.na(AER_main) ) %>%
#   select(event, subgroup, cohort, subgroup, AER_main, time_points, days)%>% ungroup()

df <- df %>% filter(days == 196 & subgroup != "aer_overall" & time_points == "reduced") %>%
  distinct()

df$time_points <- NULL

#Sum over all subgroups to get total
df <- df %>% 
  group_by(event, cohort) %>%
  summarise(total = sum(AER))

#Add person years of follow up
table2_pre_vax <- read.csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/descriptive/table2_prevax_diabetes.csv")
table2_pre_vax <- table2_pre_vax %>% filter(event == "out_date_t2dm_extended_follow_up")

table2_vax <- read.csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/descriptive/table2_vax_diabetes.csv")
table2_vax <- table2_vax %>% filter(event == "out_date_t2dm")

table2_unvax <- read.csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/descriptive/table2_unvax_diabetes.csv")
table2_unvax <- table2_unvax %>% filter(event == "out_date_t2dm")

table_2 <- rbind(table2_pre_vax, table2_vax,table2_unvax)
rm(table2_pre_vax,table2_vax,table2_unvax)

table_2$event <- gsub("_extended_follow_up","",table_2$event)

#To get total follow up for main analysis need to sum unexposed person days of follow up & exposed person days
table_2 <- table_2 %>% select(event, subgroup, cohort_to_run, total_person_days, unexposed_person_days, person_days_total_exposed_to_day_197) %>% 
  filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))

table_2$exposed_person_days <- ifelse(table_2$cohort_to_run != "prevax", table_2$total_person_days - table_2$unexposed_person_days, table_2$person_days_total_exposed_to_day_197)
table_2$total_person_days <- NULL
table_2$unexposed_person_days <- NULL
table_2$person_days_total_exposed_to_day_197 <- NULL

#Sum follow up
table_2 <- table_2 %>% group_by(event, cohort_to_run) %>%
  summarise(total_exposed_follow_up = sum(exposed_person_days))

#Convert from days to years
table_2$total_exposed_follow_up <- table_2$total_exposed_follow_up/365.2
table_2$event <- gsub("out_date_","",table_2$event)

#Left join follow up onto AER table
df <- df %>% left_join(table_2, by = c("event"="event", "cohort"="cohort_to_run"))
df$excess_events_per_1000_person_years <- df$total * (1000/df$total_exposed_follow_up)

df <- pivot_longer(df, cols = c(total, total_exposed_follow_up, excess_events_per_1000_person_years), names_to = "summary", values_to = "total")
df <- pivot_wider(df, names_from = cohort, values_from = total)

df <- df %>% rename("Pre-vaccination cohort"="prevax",
                    "Vaccinated cohort"="vax",
                    "Unvaccinated cohort"="unvax")

df$summary <- ifelse(df$summary == "total", "Total excess events",df$summary)
df$summary <- ifelse(df$summary == "total_exposed_follow_up", "Total post exposure follow up (years)",df$summary)
df$summary <- ifelse(df$summary == "excess_events_per_1000_person_years", "Excess events per 1000 person years",df$summary)
df$event <- ifelse(df$event == "t2dm", "Type-2 diabetes", df$event)

df <- df[c("event","summary","Pre-vaccination cohort","Vaccinated cohort","Unvaccinated cohort")]
write.csv(df,"C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/generated-tables/Estimated_excess_events_at_28_weeks.csv", row.names = F )


