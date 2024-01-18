# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Create empty results table ---------------------------------------------------
print('Create empty results table')

df <- data.frame(cohort = character(),
                 death28days = numeric(),
                 sample_size = numeric())

# Repeat for each cohort -------------------------------------------------------
print('Repeat for each cohort')

for (cohort in c("prevax","vax","unvax")) {
  
  ## Load data -----------------------------------------------------------------
  print('Load data')
  
  input <- dplyr::as_tibble(readr::read_rds(paste0("output/input_",cohort,"_stage1_diabetes.rds")))
  
  input <- input[input$sub_bin_covid19_confirmed_history==FALSE,
                 c("patient_id","index_date","death_date")]
  
  ## Create variable for died within 28 days -----------------------------------
  print('Create variable for died within 28 days')
  
  input$death28days <- !is.na(input$death_date) & (input$death_date-input$index_date)<28
  
  ## Record number died within 28 days -----------------------------------------
  print('Record number died within 28 days')
  
  df[nrow(df)+1,] <- c(cohort, sum(input$death28days), nrow(input))
  
}

# Save results -----------------------------------------------------------------
print('Save results')

write.csv(df, "output/death28days.csv", row.names = FALSE)

# Perform rounding -------------------------------------------------------------
print('Perform rounding')

df$death28days_midpoint6 <- roundmid_any(as.numeric(df$death28days))
df$sample_size_midpoint6  <- roundmid_any(as.numeric(df$sample_size))
df[,c("death28days","sample_size")] <- NULL

# Save rounded results ---------------------------------------------------------
print('Save rounded results')

write.csv(df, "output/death28days_rounded.csv", row.names = FALSE)