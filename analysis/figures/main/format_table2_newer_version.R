library(dplyr)
library(broman)
library(stringr)
# Load data --------------------------------------------------------------------

#results_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/descriptive/"
#output_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/generated-figures/"

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"
dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)


#############################################################
# Alternative formatting of table 2 with cohorts as columns #
#############################################################

# Get active analysis table for labels -----------------------------------------
active_analyses <- readr::read_rds("lib/active_analyses.RDS")

# Get data from each cohort ----------------------------------------------------
table2_pre_vax <- read.csv(paste0(results_dir,"table2_prevax_diabetes.csv"))
table2_vax <- read.csv(paste0(results_dir,"table2_vax_diabetes.csv"))
table2_unvax <- read.csv(paste0(results_dir,"table2_unvax_diabetes.csv"))

table2_pre_vax_gestational <- read.csv(paste0(results_dir,"table2_prevax_diabetes_gestational.csv"))
table2_vax_gestational <- read.csv(paste0(results_dir,"table2_vax_diabetes_gestational.csv"))
table2_unvax_gestational <- read.csv(paste0(results_dir,"table2_unvax_diabetes_gestational.csv"))


# Format data from each cohort -------------------------------------------------

format_table_2_short<- function(df, cohort){
  table2_raw <- df
  
  table2_raw <- table2_raw %>% select(subgroup,event,unexposed_event_count, post_exposure_event_count, unexposed_person_days, total_person_days)%>%
    filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))
  
  table2_raw$post_exposure_person_days <- table2_raw$total_person_days - table2_raw$unexposed_person_days
  
  table2_pre_expo <- table2_raw %>% select(subgroup, event, unexposed_event_count, unexposed_person_days)
  table2_pre_expo$period <- "unexposed"
  table2_pre_expo <- table2_pre_expo %>% dplyr::rename(event_count = unexposed_event_count,
                                                person_days = unexposed_person_days)
  
  table2_post_expo <- table2_raw %>% select(subgroup, event, post_exposure_event_count, post_exposure_person_days)
  table2_post_expo$period <- "post_expo"
  table2_post_expo <- table2_post_expo %>% dplyr::rename(event_count = post_exposure_event_count,
                                                  person_days = post_exposure_person_days )
  
  table2 <- rbind(table2_pre_expo,table2_post_expo)
  rm(table2_pre_expo,table2_post_expo,table2_raw)
  
  table2 <- table2 %>% mutate(across(c("event_count","person_days"), as.numeric))
  
  # Add in incidence rate
  table2[,"Incidence rate (per 100,000 person-years)"] <- add_commas(round((table2$event_count/(table2$person_days/365.25))*100000))
  table2[,"Event counts/100,000 person-years"] <- paste0(add_commas(table2$event_count), "/", add_commas(round((table2$person_days/365.25))))
  
  
  table2$period <- ifelse(table2$period=="unexposed","No COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_hospitalised","After hospitalised COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_non_hospitalised","After non-hospitalised COVID-19",table2$period)
  
  table2[,c("subgroup","person_days")] <- NULL
  table2 <- table2[!duplicated(table2), ]
  
  
  # Tidy event labels ----------------------------------------------------------
  table2 <- table2 %>% left_join(active_analyses %>% select(outcome_variable,outcome), by=c("event"="outcome_variable"))
  table2$event <- NULL
  
  #Split primary position event
  table2 <- table2 %>% dplyr::rename(Outcome = outcome)
  
  # table2 <- table2 %>% filter(!grepl('Primary position',Outcome))
  
  # -> Not using primary position results for table 2 in CVD paper
  #table2_primary_position <- table2 %>% filter(grepl('Primary position',Outcome))
  
  # Re-order columns -----------------------------------------------------------
  table2 <- table2 %>% select("Outcome","period","Event counts/100,000 person-years","Incidence rate (per 100,000 person-years)")
  
  table2$cohort <- cohort
  
  return(table2)
}

table2_pre_vax_formatted <- format_table_2_short(table2_pre_vax,"Pre-vaccination")
table2_vax_formatted <- format_table_2_short(table2_vax,"Vaccinated")
table2_unvax_formatted <- format_table_2_short(table2_unvax,"Unvaccinated")

table2_pre_vax_formatted_gestational <- format_table_2_short(table2_pre_vax_gestational,"Pre-vaccination")
table2_vax_formatted_gestational <- format_table_2_short(table2_vax_gestational,"Vaccinated")
table2_unvax_formatted_gestational <- format_table_2_short(table2_unvax_gestational,"Unvaccinated")

rm(table2_pre_vax,table2_pre_vax_gestational,table2_vax,table2_vax_gestational,table2_unvax,table2_unvax_gestational)

# Combine results for all cohorts ----------------------------------------------
table2_merged <- rbind(table2_pre_vax_formatted,table2_vax_formatted,table2_unvax_formatted,
                       table2_pre_vax_formatted_gestational,table2_vax_formatted_gestational,table2_unvax_formatted_gestational)

rm(table2_pre_vax_formatted,table2_vax_formatted,table2_unvax_formatted,
   table2_pre_vax_formatted_gestational,table2_vax_formatted_gestational,table2_unvax_formatted_gestational)

# remove prevax outcomes without extended follow up 

table2_merged <- table2_merged %>%
  dplyr::filter(!(!str_detect(Outcome, 'extended') & str_detect(cohort, 'Pre')))

# remove persistent diabetes for now - will update once updated table 2 release
table2_merged <- table2_merged %>% dplyr::filter(!str_detect(Outcome, '4_mnth'))

# remove unvax outcomes with extended follow up (sensitivity analyses - in separate table)

table2_merged <- table2_merged %>%
  dplyr::filter(!(str_detect(Outcome, 'extended') & cohort == "Unvaccinated"))

# remove extended follow up text
table2_merged$Outcome <- gsub("\\ - extended follow up*","",table2_merged$Outcome)

# Make columns for cohort ------------------------------------------------------

#table2_transposed <- tidyr::pivot_wider(table2_merged, names_from = "cohort", values_from = "Event counts/100,000 person-years")
table2_transposed <- tidyr::pivot_wider(table2_merged, names_from = "cohort", values_from = c("Event counts/100,000 person-years","Incidence rate (per 100,000 person-years)"))

# Add labels and re-order rows -------------------------------------------------
table2_transposed <- table2_transposed[,c("Outcome","period","Event counts/100,000 person-years_Pre-vaccination","Incidence rate (per 100,000 person-years)_Pre-vaccination",
                                          "Event counts/100,000 person-years_Vaccinated","Incidence rate (per 100,000 person-years)_Vaccinated",
                                          "Event counts/100,000 person-years_Unvaccinated","Incidence rate (per 100,000 person-years)_Unvaccinated")]

# table2_transposed <- rbind(table2_transposed,c("Other vascular events",rep(NA,7)))
# table2_transposed <- rbind(table2_transposed,c("Arterial thrombosis events",rep(NA,7)))
# table2_transposed <- rbind(table2_transposed,c("Venous thromboembolism events",rep(NA,7)))
# table2_transposed <- rbind(table2_transposed,c(rep("table_sub_heading",2),sub('\\_.*', '', colnames(table2_transposed)[3:8])))

colnames(table2_transposed) <- c("Outcome","period","Pre-vaccination cohort (N=15,211,471)","Pre-vaccination cohort (N=15,211,471)",
                                 "Vaccinated cohort (N=11,822,640)","Vaccinated cohort (N=11,822,640)",
                                 "Unvaccinated cohort (N=2,851,183)","Unvaccinated cohort (N=2,851,183)") 

table2_transposed$Outcome <- as.factor(table2_transposed$Outcome)

levels(table2_transposed$Outcome) <- list("Type 1 diabetes"="type 1 diabetes",
                                          "Type 2 diabetes"="type 2 diabetes",
                                          "Other or non-specified diabetes"="other or non-specific diabetes",
                                          "Gestational diabetes" = "gestational diabetes")


table2_transposed$Outcome <- factor(table2_transposed$Outcome, levels=c("Type 2 diabetes",
                                                                        "Type 1 diabetes",
                                                                        "Gestational diabetes",
                                                                        "Other or non-specified diabetes"))

table2_transposed$period <- factor(table2_transposed$period, levels = c("No COVID-19",
                                                                        "After hospitalised COVID-19",
                                                                        "After non-hospitalised COVID-19"))

table2_transposed <- table2_transposed[order(table2_transposed$Outcome, table2_transposed$period),]

write.csv(table2_transposed, paste0(output_dir,"formatted_table_2_cohorts_in_columns_updated-persistent.csv"),row.names = F)
