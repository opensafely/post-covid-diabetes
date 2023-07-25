# Function for making life tables
# Inputs are df, which is a data frame containing: hazard ratio, unexposed_person_days, unexposed_events, total_exposed, time_period_start, time_period_end
# Plus the relevant subgroup, outcome, cohort, analysis and model to use

lifetable <- function(model_output, aer_input) {

  # Filter model output --------------------------------------------------------
  print('Filter model output')
  
  model_output <- model_output[model_output$analysis==aer_input$analysis &
                                 model_output$outcome==aer_input$outcome &
                                 model_output$cohort==aer_input$cohort,]
  
  # Step 0. Create life table --------------------------------------------------
  print('Step 0. Create life table')
  
  lifetable <- data.frame(analysis = aer_input$analysis, 
                          outcome = aer_input$outcome, 
                          cohort = aer_input$cohort,
                          aer_age = aer_input$aer_age,
                          aer_sex = aer_input$aer_sex,
                          days = c(0:(max(model_output$time_period_end)-1)),
                          stringsAsFactors = FALSE)
  
  # Step 1. Average daily incidence in unexposed -------------------------------
  print('Step 1. Average daily incidence in unexposed')
  
  # Divide the number of events in the unexposed by time in days
  
  lifetable$incidence_unexp <- aer_input$unexposed_events / aer_input$unexposed_person_days
 
  # Step 2. Cumulative risk over time in unexposed -----------------------------
  print('Step 2. Cumulative risk over time in unexposed')
  
  lifetable$cumulative_survival_unexp <- cumprod(1 - lifetable$incidence_unexp) 
  
  # Step 3. Add HRs  -----------------------------------------------------------
  print('Step 3. Add HRs')

  lifetable$hr <- NA
  
  for(i in 1:nrow(model_output)){
    tmp <- model_output[i,]
    lifetable$hr <- ifelse(lifetable$days >= tmp$time_period_start & lifetable$days < tmp$time_period_end, tmp$hr, lifetable$hr)
  }
  
  # Step 4. Predict incidence of outcome after exposure ------------------------
  print('Step 4. Predict incidence of outcome after exposure')
  
  # Multiply daily incidence in unexposed by relevant hazard ratio
  
  lifetable$cumulative_survival_exp <- cumprod(1 - (lifetable$hr * lifetable$incidence_unexp))

  # Step 5. Calculate daily excess risk ----------------------------------------
  print('Step 5. Calculate daily excess risk')

  # This is the difference in cumulative survival unexposed and expected cumulative survival in unexposed
  
  lifetable$cumulative_difference_absolute_excess_risk <- lifetable$cumulative_survival_unexp - lifetable$cumulative_survival_exp

  # Save output for AER figure -------------------------------------------------
  print('Save output for AER figure')

  write.csv(lifetable, 
            file=paste0("output/lifetable_",
                        aer_input$outcome,"_",
                        aer_input$cohort,"_",
                        aer_input$aer_sex,"_",
                        aer_input$aer_age,".csv"), 
            row.names = FALSE)

  # Return output for compiling ------------------------------------------------
  print('Return output for compiling')
  
  return(lifetable)

}