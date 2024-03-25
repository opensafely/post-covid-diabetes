# Specify directories -----------------------------------------------------------
results_dir <- "C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/model/"

 # Specify parameters -----------------------------------------------------------
print('Specify parameters')

analysis <- "day0_main"

# Load model output ------------------------------------------------------------
print('Load model output')
model_output <- read.csv(paste0(results_dir,"/master_hr_file.csv"))

model_output <- dplyr::rename(model_output,
                "outcome" = "event", 
                "analysis" = "subgroup",
                "hr" = "estimate")

# Format and restrict to relevant models ---------------------------------------
print('Restrict to relevant models')

model_output <- model_output[,c("outcome","cohort","analysis","model","term","hr")]
model_output$hr <- as.numeric(model_output$hr)

model_output <- model_output[stringr::str_detect(model_output$term, "^days") &
                               model_output$analysis==analysis &
                               model_output$model=="mdl_max_adj" & 
                               model_output$hr!="[Redacted]" & 
                               ((model_output$outcome=="t2dm_extended_follow_up" & model_output$cohort=="prevax") |
                                (model_output$outcome=="t2dm" & model_output$cohort!="prevax")) &
                                  !is.na(model_output$hr),]

# Add start and end for time periods to model output ---------------------------
print('Add start and end for time periods to model output')

model_output$time_period_start <- as.numeric(gsub("_.*", "",gsub("days", "",model_output$term)))
model_output$time_period_end <- as.numeric(gsub(".*_", "",model_output$term))

# Load AER input ---------------------------------------------------------------
print('Load AER input')
aer_input <- read.csv(paste0(results_dir,"/AER/aer_input-main-rounded.csv"))
aer_input$analysis <- paste0("day0_",aer_input$analysis) # To save rerunning aer_input as day0 models use same sample but different analysis names 

# Run AER function -------------------------------------------------------------
print('Run AER function')

lifetables_compiled <- NULL

for (i in 1:nrow(aer_input)) {
  
  if (nrow(model_output[model_output$outcome==aer_input$outcome[i] &
                        model_output$cohort==aer_input$cohort[i],])>0) {
    
    tmp <- lifetable(model_output = model_output,
                     aer_input = aer_input[i,],
                     day0 = TRUE) 
    
    tmp$day0 <- TRUE
    lifetables_compiled <- rbind(lifetables_compiled, tmp)
    
    tmp <- lifetable(model_output = model_output,
                     aer_input = aer_input[i,],
                     day0 = FALSE) 
    
    tmp$day0 <- FALSE
    lifetables_compiled <- rbind(lifetables_compiled, tmp)
    
  }
  
}

# Calculate prevax weightings --------------------------------------------------
print('Calculate prevax weightings')

prevax_weightings <- aer_input[aer_input$cohort=="prevax",
                               c("analysis",
                                 "outcome",
                                 "aer_sex", 
                                 "aer_age", 
                                 "sample_size")]

prevax_weightings$weight <- prevax_weightings$sample_size/sum(prevax_weightings$sample_size)
prevax_weightings$sample_size <- NULL

# Calculate overall AER --------------------------------------------------------
print('Calculate overall AER')

lifetable_overall <- lifetables_compiled[,c("analysis","outcome","cohort","days",
                                            "aer_age","aer_sex",
                                            "cumulative_difference_absolute_excess_risk","day0")]

lifetable_overall <- merge(lifetable_overall, prevax_weightings,
                           by=c("analysis","aer_sex","aer_age"))

lifetable_overall <- lifetable_overall %>% 
  dplyr::group_by(analysis, cohort, days, day0) %>%
  dplyr::mutate(cumulative_difference_absolute_excess_risk = weighted.mean(cumulative_difference_absolute_excess_risk,weight)) %>%
  dplyr::ungroup() %>%
  dplyr::select(analysis, cohort, days, day0, cumulative_difference_absolute_excess_risk) %>%
  unique

lifetable_overall$aer_sex <- "overall"
lifetable_overall$aer_age <- "overall"

# Compile aer_group and overall life tables -------------------------------------
print('Compile aer_group and overall life tables')

lifetables_compiled <- lifetables_compiled[,c("analysis","cohort","days","day0",
                                              "aer_age","aer_sex",
                                              "cumulative_difference_absolute_excess_risk")]

lifetables_compiled <- rbind(lifetables_compiled, lifetable_overall)

# Save compiled life tables ----------------------------------------------------
print('Save compiled life tables')

write.csv(lifetables_compiled, paste0("C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/AER/lifetables_compiled.csv"),row.names = F)
