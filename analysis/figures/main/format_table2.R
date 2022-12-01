library(dplyr)
# Load data --------------------------------------------------------------------

dir <- ("~/Library/CloudStorage/OneDrive-UniversityofBristol/ehr_postdoc/projects/post-covid-diabetes")
setwd(dir)

results_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/")
output_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/generated-figures/")


format_table_2 <- function(df, cohort){
  table2_raw <- df
  
  table2_raw <- table2_raw %>% select(subgroup,event,unexposed_event_count, post_exposure_event_count)%>%
    filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))
  
  table2_pre_expo <- table2_raw %>% select(subgroup, event, unexposed_event_count)
  table2_pre_expo$period <- "unexposed"
  table2_pre_expo <- table2_pre_expo %>% dplyr::rename(event_count = unexposed_event_count)
  
  table2_post_expo <- table2_raw %>% dplyr::select(subgroup, event, post_exposure_event_count)
  table2_post_expo$period <- "post_expo"
  table2_post_expo <- table2_post_expo %>% dplyr::rename(event_count = post_exposure_event_count)
  
  table2 <- rbind(table2_pre_expo,table2_post_expo)
  
  rm(table2_pre_expo,table2_post_expo,table2_raw)
  
  
  table2$period <- ifelse(table2$period=="unexposed","No COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_hospitalised","After hospitalised COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_non_hospitalised","After non-hospitalised COVID-19",table2$period)
  
  table2[,"subgroup"] <- NULL
  table2 <- table2[!duplicated(table2), ]
  
  # Make columns for exposure time -----------------------------------------------
  
  table2 <- tidyr::pivot_wider(table2, names_from = "period", values_from = "event_count")
  
  #Add totals columm
  table2 <- table2 %>% mutate(across(c("No COVID-19","After hospitalised COVID-19","After non-hospitalised COVID-19"),as.numeric))
  table2$Total <- rowSums(table2[,c(2,3,4)])
  
  #Save table 2 for venn diagram numbers check
  # write.csv(table2,paste0(results_dir,"table_2_venn_diagram_number_check_",cohort,".csv"),row.names = F)
  
  # Tidy event labels ------------------------------------------------------------
  active_analyses <- readr::read_rds("lib/active_analyses.RDS")
  
  table2 <- table2 %>% left_join(active_analyses %>% select(outcome_variable,outcome), by=c("event"="outcome_variable"))
  table2$event <- NULL
  
  #Split primary position event
  table2 <- table2  %>% dplyr::rename(Outcome = outcome)
  table2 <- table2[,c(5,1,2,3,4)] 
  
  # Re-order rows and add empty rows ---------------------------------------------------------------
  
  table2$Cohort <- cohort
  
  return(list(table2))
}


# Combine across cohorts
table2_prevax <- read.csv(paste0(results_dir,"table2_prevax_diabetes.csv"))
table2_vax <- read.csv(paste0(results_dir,"table2_vax_diabetes.csv"))
table2_unvax <- read.csv(paste0(results_dir,"table2_unvax_diabetes.csv"))

table2_prevax <- format_table_2(table2_prevax,"Pre-Vaccination")
table2_vax <- format_table_2(table2_vax,"Vaccinated")
table2_unvax <- format_table_2(table2_unvax,"Unvaccinated")

table2_prevax <- table2_prevax[[1]]
table2_vax <- table2_vax[[1]]
table2_unvax <- table2_unvax[[1]]

table2_merged <- rbind(table2_prevax,table2_vax,table2_unvax)
table2_merged <- table2_merged[!duplicated(table2_merged),]
table2_merged$Outcome <- str_to_title(table2_merged$Outcome)



table2_merged$Cohort <- factor(table2_merged$Cohort, levels = c("Pre-Vaccination",
                                                                "Vaccinated",
                                                                "Unvaccinated"))
table2_merged$Outcome <- gsub("Other Or Non-Specific Diabetes", "Other Or Non-Specified Diabetes", table2_merged$Outcome)

# levels(table2_merged$Cohort) <- list("Pre-vaccination"="Pre-Vaccination", "Vaccinated"="Vaccinated","Unvaccinated"="Unvaccinated")
levels(table2_merged$Cohort) <- list("Pre-vaccination (1 Jan 2020 to 18 Jun 2021)"="Pre-Vaccination", "Vaccinated (1 Jun 2021 to 14 Dec 2021)"="Vaccinated","Unvaccinated (1 Jun 2021 to 14 Dec 2021)"="Unvaccinated")

table2_merged <- table2_merged[order(table2_merged$Outcome),]
table2_merged <- table2_merged %>% select("Outcome","Cohort","No COVID-19","After hospitalised COVID-19",
                                          "After non-hospitalised COVID-19","Total")


write.csv(table2_merged, paste0(output_dir,"formatted_table_2.csv"),row.names = F)

