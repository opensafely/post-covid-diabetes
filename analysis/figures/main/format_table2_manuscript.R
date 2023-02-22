library(dplyr)
# Load data --------------------------------------------------------------------

results_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/descriptive/"
output_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/generated-figures/"
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


# Format data from each cohort -------------------------------------------------
format_table_2_short<- function(df, cohort){
  table2_raw <- df
  
  table2_raw <- table2_raw %>% select(subgroup,event,unexposed_event_count, post_exposure_event_count)%>%
    filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))
  
  table2_pre_expo <- table2_raw %>% select(subgroup, event, unexposed_event_count)
  table2_pre_expo$period <- "unexposed"
  table2_pre_expo <- table2_pre_expo %>% rename(event_count = unexposed_event_count)
  
  table2_post_expo <- table2_raw %>% select(subgroup, event, post_exposure_event_count)
  table2_post_expo$period <- "post_expo"
  table2_post_expo <- table2_post_expo %>% rename(event_count = post_exposure_event_count)
  
  table2 <- rbind(table2_pre_expo,table2_post_expo)
  rm(table2_pre_expo,table2_post_expo,table2_raw)
  
  table2$period <- ifelse(table2$period=="unexposed","No COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_hospitalised","After hospitalised COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_non_hospitalised","After non-hospitalised COVID-19",table2$period)
  
  table2[,"subgroup"] <- NULL
  table2 <- table2[!duplicated(table2), ]
  
  
  # Tidy event labels ----------------------------------------------------------
  table2 <- table2 %>% left_join(active_analyses %>% select(outcome_variable,outcome), by=c("event"="outcome_variable"))
  table2$event <- NULL
  
  #Split primary position event
  table2 <- table2 %>% rename(Outcome = outcome)
  
  table2 <- table2 %>% filter(!grepl('Primary position',Outcome))
  
  # -> Not using primary position results for table 2 in CVD paper
  #table2_primary_position <- table2 %>% filter(grepl('Primary position',Outcome))
  
  # Re-order columns -----------------------------------------------------------
  table2 <- table2 %>% select(Outcome,period,event_count)
  
  table2$cohort <- cohort
  
  return(table2)
}

table2_pre_vax_formatted <- format_table_2_short(table2_pre_vax,"prevax")
table2_vax_formatted <- format_table_2_short(table2_vax,"vax")
table2_unvax_formatted <- format_table_2_short(table2_unvax,"unvax")

# Combine results for all cohorts ----------------------------------------------
table2_merged <- rbind(table2_pre_vax_formatted,table2_vax_formatted,table2_unvax_formatted)

# rename columns ---------------------------------------------------------------
table2_merged$cohort[table2_merged$cohort == "prevax"] <- "Pre-vaccination"
table2_merged$cohort[table2_merged$cohort == "vax"] <- "Vaccinated"
table2_merged$cohort[table2_merged$cohort == "unvax"] <- "Unvaccinated"

# Make columns for cohort ------------------------------------------------------
table2_transposed <- tidyr::pivot_wider(table2_merged, names_from = "cohort", values_from = "event_count")

# Add labels and re-order rows -------------------------------------------------
# table2_transposed <- rbind(table2_transposed,c("Type 1 diabetes",rep(NA,4)))
# table2_transposed <- rbind(table2_transposed,c("Type 2 diabetes",rep(NA,4)))
# table2_transposed <- rbind(table2_transposed,c("Other or non-specified diabetes",rep(NA,4)))

table2_transposed$Outcome[table2_transposed$Outcome == "type 1 diabetes"] <- "Type 1 diabetes"
table2_transposed$Outcome[table2_transposed$Outcome == "type 2 diabetes"] <- "Type 2 diabetes"
table2_transposed$Outcome[table2_transposed$Outcome == "other or non-specific diabetes"] <- "Other or non-specified diabetes"


table2_transposed$Outcome <- factor(table2_transposed$Outcome, levels=c("Type 1 diabetes",
                                                                        "Type 2 diabetes",
                                                                        "Other or non-specified diabetes"))

table2_transposed$period <- factor(table2_transposed$period, levels = c("No COVID-19",
                                                                        "After hospitalised COVID-19",
                                                                        "After non-hospitalised COVID-19"))

table2_transposed <- table2_transposed[order(table2_transposed$Outcome, table2_transposed$period),]



write.csv(table2_transposed, paste0(output_dir,"formatted_table_2_cohorts_in_columns.csv"),row.names = F)
