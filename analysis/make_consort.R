# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Create blank table -----------------------------------------------------------
print('Create blank table')

df <- NULL

# Add output from each cohort --------------------------------------------------
print('Add output from each cohort')

for (i in c("prevax","vax","unvax")) {

  # Load data ------------------------------------------------------------------
  print('Load data')
  
  tmp <- readr::read_csv(paste0("output/review/descriptives/Cohort_flow_",i,"_diabetes.csv"))
  tmp$cohort <- i
  
  # Perform redaction ----------------------------------------------------------
  print('Perform redaction')
  
  tmp$N_midpoint6 <- roundmid_any(as.numeric(tmp$N), threshold)
  tmp$N_removed_midpoint6_derived <- dplyr::lag(tmp$N_midpoint6,1) - tmp$N_midpoint6
  tmp$N_removed_midpoint6_derived <- ifelse(tmp$N_removed=="Calculate manually", "Calculate manually", tmp$N_removed_midpoint6_derived)
  
  # Bind -----------------------------------------------------------------------
  print('Bind')
  
  df <- rbind(df, tmp)
  
}

# Save output ------------------------------------------------------------------
print('Save output')

readr::write_csv(df[,c("cohort","N","N_removed","Description")], 
                "output/consort.csv")

# Save redacted output ---------------------------------------------------------
print('Save redacted output')

readr::write_csv(df[,c("cohort","N_midpoint6","N_removed_midpoint6_derived","Description")],
                 "output/consort_midpoint6.csv")
