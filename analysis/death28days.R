# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Create empty results table ---------------------------------------------------
print('Create empty results table')

df <- data.frame(cohort = character(),
                 exposed_death28days = numeric(),
                 exposed_total = numeric())

# Repeat for each cohort -------------------------------------------------------
print('Repeat for each cohort')

for (cohort in c("prevax","vax","unvax")) {
  
  # Define data suffix ---------------------------------------------------------
  print('Define data suffix')

  suffix <- ifelse(cohort=="prevax","_extended_follow_up","")  

  # Load data ------------------------------------------------------------------
  print("Load data")
  
  input <- dplyr::as_tibble(readr::read_rds(paste0("output/model_input-cohort_",cohort,"-main-t2dm",suffix,".rds")))
  input <- input[,c("patient_id","exp_date")]
  input$patient_id <- as.character(input$patient_id)
  input$exp_date <- as.Date(input$exp_date)
  print(Hmisc::describe(input))
  
  studydef <- arrow::read_feather(file = "output/input_prelim.feather")
  studydef <- studydef[, c("patient_id","death_date")]
  studydef$patient_id <- as.character(studydef$patient_id)
  studydef$death_date <- as.Date(studydef$death_date)
  print(Hmisc::describe(studydef))
  
  input <- merge(input, studydef, by = "patient_id")
  print(Hmisc::describe(studydef))
  
  print(paste0("Among ",nrow(input)," individuals in the cohort, ",sum(!is.na(input$death_date)), " individuals die during follow-up."))

  ## Restrict to exposed individuals -------------------------------------------
  print('Restrict to exposed individuals')
  
  input <- input[!is.na(input$exp_date),]
  
  ## Create variable for died within 28 days -----------------------------------
  print('Create variable for died within 28 days')
  
  input$death28days <- !is.na(input$death_date) & (input$death_date-input$exp_date)<28
  
  ## Record number died within 28 days -----------------------------------------
  print('Record number died within 28 days')
  
  print(paste0("Among ",nrow(input)," exposed individuals, ",sum(input$death28days), " die within 28 days of COVID-19."))
  df[nrow(df)+1,] <- c(cohort, sum(input$death28days), nrow(input))
  
}

# Save results -----------------------------------------------------------------
print('Save results')

write.csv(df, "output/death28days.csv", row.names = FALSE)

# Perform rounding -------------------------------------------------------------
print('Perform rounding')

df$exposed_death28days_midpoint6 <- roundmid_any(as.numeric(df$exposed_death28days), threshold)
df$exposed_total_midpoint6  <- roundmid_any(as.numeric(df$exposed_total), threshold)
df[,c("exposed_death28days","exposed_total")] <- NULL

# Save rounded results ---------------------------------------------------------
print('Save rounded results')

write.csv(df, "output/death28days_rounded.csv", row.names = FALSE)