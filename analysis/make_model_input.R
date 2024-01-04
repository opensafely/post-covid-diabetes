# Load packages ----------------------------------------------------------------
print('Load packages')

library(magrittr)
library(data.table)

# Source functions -------------------------------------------------------------
print('Source functions')

source("analysis/fn-check_vitals.R")

# Specify arguments ------------------------------------------------------------
print('Specify arguments')

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  name <- "all" # prepare datasets for all active analyses 
  # name <- "cohort_vax-sub_history_none-depression" # prepare datasets for all active analyses whose name contains X
  # name <- "vax-depression-main;vax-depression-sub_covid_hospitalised;vax-depression-sub_covid_nonhospitalised" # prepare datasets for specific active analyses
} else {
  name <- args[[1]]
}

# Load active analyses ---------------------------------------------------------
print('Load active analyses')

active_analyses <- readr::read_rds("lib/active_analyses.rds")

# Identify model inputs to be prepared -----------------------------------------
print('Identify model inputs to be prepared')

if (name=="all") {
  prepare <- active_analyses$name
} else if(grepl(";",name)) {
  prepare <- stringr::str_split(as.vector(name), ";")[[1]]
} else {
  prepare <- active_analyses[grepl(name,active_analyses$name),]$name
}

# Filter active_analyses to model inputs to be prepared ------------------------
print('Filter active_analyses to model inputs to be prepared')

active_analyses <- active_analyses[active_analyses$name %in% prepare,]

for (i in 1:nrow(active_analyses)) {
  
  # Define data suffix ---------------------------------------------------------
  print('Define data suffix')
  
  suffix <- ""
  suffix <- ifelse(grepl("gestationaldm",active_analyses$outcome[i]),"_gestational",suffix)
  suffix <- ifelse(grepl("_obes",active_analyses$outcome[i]),"_obesity",suffix)
  suffix <- ifelse(grepl("_obes_no",active_analyses$outcome[i]),"_no_obesity",suffix)
  suffix <- ifelse(grepl("_pd",active_analyses$outcome[i]),"_prediabetes",suffix)
  suffix <- ifelse(grepl("_pd_no",active_analyses$outcome[i]),"_no_prediabetes",suffix)
  
  # Load data ------------------------------------------------------------------
  print(paste0("Load data for ",active_analyses$name[i]))
  
  input <- dplyr::as_tibble(readr::read_rds(paste0("output/input_",active_analyses$cohort[i],"_stage1_diabetes",suffix,".rds")))
  
  end_dates <- readr::read_rds(paste0("output/follow_up_end_dates_",active_analyses$cohort[i],"_diabetes",suffix,".rds"))
  
  
  # Add indicator for 4 months (= 4 * 28 = 112 days) follow-up ------------------
  print('Add indicator for 4 months (= 4 * 28 = 112 days) follow-up')
  
  input$fup4m <- (input$end_date - input$index_date) > 112
  
  # Add end dates --------------------------------------------------------------
  print("Add end dates")
  
  event_name <- gsub("out_date_","",active_analyses$outcome[i])
  event_name <- gsub("_follow_extended_follow_up","_extended_follow_up",event_name) # 'follow' is persistant diabetes, should use same as main analysis
  
  end_dates <- end_dates[,c("patient_id",
                            paste0(event_name,"_follow_up_end"),
                            paste0(event_name,"_hospitalised_follow_up_end"),
                            paste0(event_name,"_non_hospitalised_follow_up_end"),
                            paste0(event_name,"_hospitalised_date_expo_censor"),
                            paste0(event_name,"_non_hospitalised_date_expo_censor"))]
  
  colnames(end_dates) <- gsub(paste0(event_name,"_"),"",colnames(end_dates))
  
  input <- input %>% dplyr::left_join(end_dates, by = "patient_id")
  
  # Define end dates -----------------------------------------------------------
  print('Define data suffix')
  
  input$end_date_exposure <- as.Date(ifelse(active_analyses$cohort[i]=="prevax",
                                    "2021-06-18", active_analyses$study_stop[i]))
  
  input$end_date_outcome <- input$follow_up_end
  
  input$end_date_outcome <- ifelse(active_analyses$analysis[i]=="sub_covid_hospitalised",
                                   input$hospitalised_follow_up_end,
                                   input$end_date_outcome)
  
  input$end_date_outcome <- ifelse(active_analyses$analysis[i]=="sub_covid_nonhospitalised",
                                   input$non_hospitalised_follow_up_end,
                                   input$end_date_outcome)
  
  # Restrict to required variables ---------------------------------------------
  print('Restrict to required variables')
  
  input <- input[,unique(c("patient_id",
                           "fup4m",
                           "index_date",
                           "end_date_exposure",
                           "end_date_outcome",
                           active_analyses$exposure[i], 
                           active_analyses$outcome[i],
                           unlist(strsplit(active_analyses$strata[i], split = ";")),
                           unlist(strsplit(active_analyses$covariate_other[i], split = ";")),
                           "sub_cat_covid19_hospital",
                           "sub_bin_covid19_confirmed_history",
                           "cov_cat_sex",
                           "cov_num_age",
                           "cov_cat_ethnicity"))]
  
  # Remove outcomes outside of follow-up time ----------------------------------
  print('Remove outcomes outside of follow-up time')
  
  input <- dplyr::rename(input, 
                         "out_date" = active_analyses$outcome[i],
                         "exp_date" = active_analyses$exposure[i])
  
  input <- input %>% 
    dplyr::mutate(out_date = replace(out_date, which(out_date>end_date_outcome | out_date<index_date), NA),
                  exp_date =  replace(exp_date, which(exp_date>end_date_exposure | exp_date<index_date), NA),
                  sub_cat_covid19_hospital = replace(sub_cat_covid19_hospital, which(is.na(exp_date)),"no_infection"))
  
  # Update end date to be outcome date where applicable ------------------------
  print('Update end date to be outcome date where applicable')
  
  input <- input %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(end_date_outcome = min(end_date_outcome, out_date, na.rm = TRUE))
  
  # Make model input: main and day0 --------------------------------------------
  
  if (grepl("main",active_analyses$analysis[i])) {
    
    print(paste0('Make model input: ',active_analyses$analysis[i]))
    
    df <- input[input$sub_bin_covid19_confirmed_history==FALSE,]
    
    df[,c(colnames(df)[grepl("sub_",colnames(df))],"fup4m")] <- NULL
    
    check_vitals(df)
    readr::write_rds(df, file.path("output", paste0("model_input-",active_analyses$name[i],".rds")), compress = "gz")
    print(paste0("Saved: output/model_input-",active_analyses$name[i],".rds"))
    rm(df)
    
  }
  
  # Make model input: fup4m ----------------------------------------------------
  
  if (grepl("fup4m",active_analyses$analysis[i])) {
    
    print(paste0('Make model input: ',active_analyses$analysis[i]))
    
    df <- input[input$sub_bin_covid19_confirmed_history==FALSE & input$fup4m==TRUE,]
    
    df[,c(colnames(df)[grepl("sub_",colnames(df))],"fup4m")] <- NULL
    
    check_vitals(df)
    readr::write_rds(df, file.path("output", paste0("model_input-",active_analyses$name[i],".rds")), compress = "gz")
    print(paste0("Saved: output/model_input-",active_analyses$name[i],".rds"))
    rm(df)
    
  }
  # Make model input: sub_covid_hospitalised -----------------------------------
  
  if (grepl("sub_covid_hospitalised",active_analyses$analysis[i])) {
    
    print(paste0('Make model input: ',active_analyses$analysis[i]))
    
    df <- input[input$sub_bin_covid19_confirmed_history==FALSE,]
    
    df <- df %>% 
      dplyr::mutate(end_date_outcome = replace(end_date_outcome, which(sub_cat_covid19_hospital=="non_hospitalised"), exp_date-1),
                    exp_date = replace(exp_date, which(sub_cat_covid19_hospital=="non_hospitalised"), NA),
                    out_date = replace(out_date, which(out_date>end_date_outcome), NA))
    
    df <- df[df$end_date_outcome>=df$index_date,]
    
    df[,c(colnames(df)[grepl("sub_",colnames(df))],"fup4m")] <- NULL
    
    check_vitals(df)
    readr::write_rds(df, file.path("output", paste0("model_input-",active_analyses$name[i],".rds")), compress = "gz")
    print(paste0("Saved: output/model_input-",active_analyses$name[i],".rds"))
    rm(df)
    
  }
  
  # Make model input: sub_covid_nonhospitalised --------------------------------
  
  if (grepl("sub_covid_nonhospitalised",active_analyses$analysis[i])) {
    
    print(paste0('Make model input: ',active_analyses$analysis[i]))
    
    df <- input[input$sub_bin_covid19_confirmed_history==FALSE,]
    
    df <- df %>% 
      dplyr::mutate(end_date_outcome = replace(end_date_outcome, which(sub_cat_covid19_hospital=="hospitalised"), exp_date-1),
                    exp_date = replace(exp_date, which(sub_cat_covid19_hospital=="hospitalised"), NA),
                    out_date = replace(out_date, which(out_date>end_date_outcome), NA))
    
    df <- df[df$end_date_outcome>=df$index_date,]
    df$index_date <- as.Date(df$index_date)
    
    df[,c(colnames(df)[grepl("sub_",colnames(df))],"fup4m")] <- NULL
    
    check_vitals(df)
    readr::write_rds(df, file.path("output", paste0("model_input-",active_analyses$name[i],".rds")), compress = "gz")
    print(paste0("Saved: output/model_input-",active_analyses$name[i],".rds"))
    rm(df)
    
  }
  
  # Make model input: sub_sex_* ------------------------------------------------
  
  if (grepl("sub_sex_",active_analyses$analysis[i])) {
    
    print(paste0('Make model input: ',active_analyses$analysis[i]))
    
    sex <- stringr::str_to_title(gsub(".*sub_sex_","",active_analyses$analysis[i]))
    
    df <- input[input$sub_bin_covid19_confirmed_history==FALSE & 
                  input$cov_cat_sex==sex,]
    
    df[,c(colnames(df)[grepl("sub_",colnames(df))],"cov_cat_sex")] <- NULL
    
    check_vitals(df)
    readr::write_rds(df, file.path("output", paste0("model_input-",active_analyses$name[i],".rds")), compress = "gz")
    print(paste0("Saved: output/model_input-",active_analyses$name[i],".rds"))
    rm(df)
    
  }
  
  # Make model input: sub_age_* ------------------------------------------------
  
  if (grepl("sub_age_",active_analyses$analysis[i])==TRUE) {
    
    print(paste0('Make model input: ',active_analyses$analysis[i]))
    
    min_age <- as.numeric(strsplit(gsub(".*sub_age_","",active_analyses$analysis[i]), split = "_")[[1]][1])
    max_age <- as.numeric(strsplit(gsub(".*sub_age_","",active_analyses$analysis[i]), split = "_")[[1]][2])
    
    df <- input[input$sub_bin_covid19_confirmed_history==FALSE & 
                  input$cov_num_age>=min_age &
                  input$cov_num_age<ifelse(max_age==110,max_age+1,max_age),]
    
    df[,c(colnames(df)[grepl("sub_",colnames(df))],"fup4m")] <- NULL
    
    check_vitals(df)
    readr::write_rds(df, file.path("output", paste0("model_input-",active_analyses$name[i],".rds")), compress = "gz")
    print(paste0("Saved: output/model_input-",active_analyses$name[i],".rds"))
    rm(df)
    
  }
  
  # Make model input: sub_ethnicity_* ------------------------------------------
  
  if (grepl("sub_ethnicity_",active_analyses$analysis[i])==TRUE) {
    
    print(paste0('Make model input: ',active_analyses$analysis[i]))
    
    ethnicity <- stringr::str_to_title(gsub(".*sub_ethnicity_","",active_analyses$analysis[i]))
    ethnicity <- ifelse(ethnicity=="Asian","South Asian",ethnicity)
    
    df <- input[input$sub_bin_covid19_confirmed_history==FALSE & 
                  input$cov_cat_ethnicity==ethnicity,]
    
    df[,c(colnames(df)[grepl("sub_",colnames(df))],"fup4m")] <- NULL
    
    check_vitals(df)
    readr::write_rds(df, file.path("output", paste0("model_input-",active_analyses$name[i],".rds")), compress = "gz")
    print(paste0("Saved: output/model_input-",active_analyses$name[i],".rds"))
    rm(df)
    
  }
  
}