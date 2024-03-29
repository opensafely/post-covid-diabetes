#===============================================================================
#
# Create table of follow up end dates that can be used for venn diagrams, 
# Table 2 and Cox scripts to save repeatedly calculating them 
#
#===============================================================================
library(dplyr)
library(readr)
library(data.table)
library(jsonlite)

#clear memory
rm(list=ls())

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort_name <- "prevax"
  group <- "diabetes"
} else {
  cohort_name <- args[[1]]
}

#json file containing vax study dates
study_dates <- fromJSON("output/study_dates.json")

if (cohort_name %in% c("vax","unvax")){
  #These are the study start and end dates for the Delta era
  cohort_start_date <- as.Date(study_dates$delta_date)
  cohort_end_date <- as.Date(study_dates$omicron_date)
}else if (cohort_name == "prevax") {
  cohort_start_date <- as.Date(study_dates$pandemic_start)
  cohort_end_date <- as.Date(study_dates$all_eligible)
  cohort_end_date_extended <- as.Date("2021-12-14")
  
}


follow_up_end_dates <- function(cohort_name, group){
  # Load relevant data
  input <- read_rds(paste0("output/input_",cohort_name,"_stage1_",group,".rds"))
  
  ## Read in active analyses table and filter to relevant outcomes
  
  active_analyses <- read_rds("lib/active_analyses.rds")
  active_analyses <- active_analyses %>% 
    filter(outcome_group == group) %>% 
    select(outcome, outcome_group) %>%
    filter(outcome != "out_date_t2dm_follow" & outcome != "out_date_t2dm_follow_extended_follow_up") %>%
    unique()
  
  input <- input[,c("patient_id","death_date","index_date","sub_cat_covid19_hospital",active_analyses$outcome,
                    colnames(input)[grepl("exp_",colnames(input))], 
                    colnames(input)[grepl("vax_date_",colnames(input))])] 
  
  input_dereg <- readr::read_csv("output/input_dereg.csv.gz")
  input <- dplyr::left_join(input, input_dereg, by = "patient_id")
  
  input$cohort_end_date <- cohort_end_date
  if (cohort_name == "prevax") {
  input$cohort_end_date_extended<- cohort_end_date_extended
  }
  for(event in active_analyses$outcome){
    print(paste0("Working on ",event))
    
    input <- input %>%rename(event_date = event)
    input$expo_date <- input$exp_date_covid19_confirmed
    input$expo_pheno <- input$sub_cat_covid19_hospital
    input <- input %>% mutate(expo_date = replace(expo_date, which(expo_date<index_date), NA))
    
    # Calculate follow up end dates based on cohort
    # follow_up_end_unexposed is required in Table 2 script and follow_up_end is 
    # the general follow up end date for each patient
    if(cohort_name=="prevax" & group != "diabetes_recovery"){
      
      input$follow_up_end_unexposed <- apply(input[,c("vax_date_covid_1", "vax_date_eligible", "event_date", "expo_date", "death_date", "cohort_end_date","deregistered_date")],1, min,na.rm=TRUE)
      input$follow_up_end <- apply(input[,c("vax_date_covid_1", "vax_date_eligible", "event_date", "death_date", "cohort_end_date","deregistered_date")],1, min,na.rm=TRUE)
      
      input$follow_up_end_unexposed <- as.Date(input$follow_up_end_unexposed)
      input$follow_up_end <- as.Date(input$follow_up_end)
      
    }else if(cohort_name=="prevax" & group == "diabetes_recovery"){
      
      cohort_end_date <- as.Date(as.Date("2020-06-15"))
      
      input$follow_up_end_unexposed <- apply(input[,c("vax_date_covid_1", "vax_date_eligible", "event_date", "expo_date", "death_date", "cohort_end_date","deregistered_date")],1, min,na.rm=TRUE)
      input$follow_up_end <- apply(input[,c("vax_date_covid_1", "vax_date_eligible", "event_date", "death_date", "cohort_end_date","deregistered_date")],1, min,na.rm=TRUE)
      
      input$follow_up_end_unexposed <- as.Date(input$follow_up_end_unexposed)
      input$follow_up_end <- as.Date(input$follow_up_end)
      
    }else if(cohort_name=="vax"){
      input$follow_up_end_unexposed <- apply(input[,c("event_date", "expo_date", "death_date", "cohort_end_date","deregistered_date")],1, min,na.rm=TRUE)
      input$follow_up_end <- apply(input[,c("event_date", "death_date", "cohort_end_date","deregistered_date")],1, min, na.rm=TRUE)
      
      input$follow_up_end_unexposed <- as.Date(input$follow_up_end_unexposed)
      input$follow_up_end <- as.Date(input$follow_up_end)
      
    }else if(cohort_name=="unvax" & !grepl("unvax_sens",event)){
      input$follow_up_end_unexposed <- apply(input[,c("vax_date_covid_1","event_date", "expo_date", "death_date","cohort_end_date","deregistered_date")],1, min,na.rm=TRUE)
      input$follow_up_end <- apply(input[,c("vax_date_covid_1","event_date", "death_date","cohort_end_date","deregistered_date")],1, min, na.rm=TRUE)
      
      input$follow_up_end_unexposed <- as.Date(input$follow_up_end_unexposed)
      input$follow_up_end <- as.Date(input$follow_up_end)
      
    } else if(cohort_name=="unvax" & grepl("unvax_sens",event)){
      input$follow_up_end_unexposed <- apply(input[,c("event_date", "expo_date", "death_date","cohort_end_date","deregistered_date")],1, min,na.rm=TRUE)
      input$follow_up_end <- apply(input[,c("event_date", "death_date","cohort_end_date","deregistered_date")],1, min, na.rm=TRUE)
      
      input$follow_up_end_unexposed <- as.Date(input$follow_up_end_unexposed)
      input$follow_up_end <- as.Date(input$follow_up_end)
      
    }
    
    if(cohort_name=="prevax" & grepl("extended_follow_up",event)){
      input$follow_up_end_unexposed <- apply(input[,c("event_date", "expo_date", "death_date","cohort_end_date_extended","deregistered_date")],1, min,na.rm=TRUE)
      input$follow_up_end <- apply(input[,c("event_date", "death_date","cohort_end_date_extended","deregistered_date")],1, min, na.rm=TRUE)
      
      input$follow_up_end_unexposed <- as.Date(input$follow_up_end_unexposed)
      input$follow_up_end <- as.Date(input$follow_up_end)
    }
    # Calculate date_expo_censor which is the COVID exposure date for the phenotype  not of interest
    # in the phenotype analyses. This is needed to re-calculate follow-up end for pheno anlayses
    
    input <- input %>% mutate(event_date = replace(event_date, which(event_date>follow_up_end | event_date<index_date), NA))
    input <- input %>% mutate(expo_date =  replace(expo_date, which(expo_date>follow_up_end | expo_date<index_date), NA))
    
    # Update COVID phenotypes after setting COVID exposure dates to NA that lie
    # outside follow up
    input$expo_pheno=as.character(input$expo_pheno)
    input=input%>%rowwise()%>%mutate(expo_pheno =ifelse(is.na(expo_date), "no_infection",expo_pheno))
    
    
    # Get COVID pheno specific dataset if necessary
    # Adds in variable date_expo_censor which is the COVID exposure date for the phenotype  not of interest
    # We want to be able to include follow up time prior to exposure for the pheno no of interest which uses date_expo_censor
    # to find this time period
    event_short <- gsub("out_date_","", event)
    
    for(pheno in c("hospitalised","non_hospitalised")){
      input$date_expo_censor <- as.Date(ifelse(!(input$expo_pheno %in% pheno),
                                               input$expo_date, 
                                               NA), origin='1970-01-01')
      setnames(input,
               old="date_expo_censor",
               new=paste0(pheno,"_date_expo_censor"))
    }
    
    input$hospitalised_follow_up_end <- apply(input[,c("follow_up_end","hospitalised_date_expo_censor")],1, min,na.rm=TRUE)
    input$non_hospitalised_follow_up_end <- apply(input[,c("follow_up_end","non_hospitalised_date_expo_censor")],1, min,na.rm=TRUE)
    
    input$hospitalised_follow_up_end <- as.Date(input$hospitalised_follow_up_end)
    input$non_hospitalised_follow_up_end <- as.Date(input$non_hospitalised_follow_up_end)
    
    # CENSOR PRE RECOVERY ANALYSIS #
    
    if(cohort_name=="prevax" & group == "diabetes_recovery_pre"){
      
      # N.B. using dplyr if_else to preserve date formatting.
      input$follow_up_end <- dplyr::if_else(input$expo_date >= "2020-06-16", input$expo_date, input$follow_up_end)
      input$follow_up_end_unexposed <- dplyr::if_else(input$expo_date >= "2020-06-16", input$expo_date, input$follow_up_end_unexposed)
      
    } 
    
    setnames(input,
             old = c("event_date",
                     "follow_up_end_unexposed",
                     "follow_up_end",
                     "hospitalised_follow_up_end",
                     "non_hospitalised_follow_up_end",
                     "hospitalised_date_expo_censor",
                     "non_hospitalised_date_expo_censor"),
             new = c(paste0("out_date_",event_short),
                     paste0(event_short,"_follow_up_end_unexposed"),
                     paste0(event_short,"_follow_up_end"),
                     paste0(event_short,"_hospitalised_follow_up_end"),
                     paste0(event_short,"_non_hospitalised_follow_up_end"),
                     paste0(event_short,"_hospitalised_date_expo_censor"),
                     paste0(event_short,"_non_hospitalised_date_expo_censor")))
    
    print(paste0("Finished working on ",event))
  }
  
  input <- input[,c("patient_id","index_date",
                    colnames(input)[grepl("follow_up_end",colnames(input))],
                    colnames(input)[grepl("censor",colnames(input))])] 
  
  saveRDS(input, paste0("output/follow_up_end_dates_",cohort_name, "_",group, ".rds"))
  
  # Save as CSV as well only for diabetes group to save time
  
  if (group == "diabetes"){
    readr::write_csv(input, paste0("output/follow_up_end_dates_",cohort_name, "_",group,".csv.gz"))
  }
  
}

# Run function using specified commandArgs and active analyses for group

active_analyses <- read_rds("lib/active_analyses.rds")
group <- unique(active_analyses$outcome_group)

for(i in group){
  if (cohort_name == "all") {
    follow_up_end_dates("prevax", i)
    follow_up_end_dates("vax", i)
    follow_up_end_dates("unvax", i)
  } else{
    follow_up_end_dates(cohort_name, i)
  }
}