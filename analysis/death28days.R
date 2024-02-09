# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")
source("analysis/fn-explore_characteristics.R")

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
  
  model_input <- dplyr::as_tibble(readr::read_rds(paste0("output/model_input-cohort_",cohort,"-main-t2dm",suffix,".rds")))
  model_input <- model_input[,c("patient_id","exp_date")]
  model_input$patient_id <- as.character(model_input$patient_id)
  model_input$exp_date <- as.Date(model_input$exp_date)
  print(paste0("output/model_input-cohort_",cohort,"-main-t2dm",suffix,".rds"))
  print(Hmisc::describe(model_input))
  
  input_prelim <- arrow::read_feather(file = "output/input_prelim.feather")
  input_prelim <- input_prelim[, c("patient_id","death_date")]
  input_prelim$patient_id <- as.character(input_prelim$patient_id)
  input_prelim$death_date <- as.Date(input_prelim$death_date)
  print("output/input_prelim.feather:")
  print(Hmisc::describe(input_prelim))
  
  tmp <- input_prelim[input_prelim$death_date>="2020-01-01" & input_prelim$death_date>="2021-12-18",]$patient_id
  characteristics <- explore_characteristics(death_dates = input_prelim, cohort = "prevax",  patients = tmp)
  data.table::fwrite(characteristics, paste0("output/characteristics_",cohort,".csv"), row.names = FALSE)
  
  ggplot2::ggplot(input_prelim, ggplot2::aes(x=death_date)) + 
    ggplot2::geom_histogram(binwidth = 7) +
    ggplot2::xlim(as.Date("2020-01-01"),as.Date("2023-01-01")) +
    ggplot2::theme_minimal()
  ggplot2::ggsave(paste0("output/hist_input_prelim_",cohort,".png"), unit="mm", width = 297, height = 210, bg = "white")
  
  ggplot2::ggplot(input_prelim[input_prelim$patient_id %in% model_input$patient_id,], ggplot2::aes(x=death_date)) + 
    ggplot2::geom_histogram(binwidth = 7) +
    ggplot2::xlim(as.Date("2020-01-01"),as.Date("2023-01-01")) +
    ggplot2::theme_minimal() 
  ggplot2::ggsave(paste0("output/hist_input_prelim_restricted_",cohort,".png"), unit="mm", width = 297, height = 210, bg = "white")
  
  print(paste0("Unique patient IDs in model_input: ", length(unique(model_input$patient_id))))
  print(paste0("Unique rows in model_input: ", nrow(model_input)))
  print(paste0("Unique patient IDs in input_prelim: ", length(unique(input_prelim$patient_id))))
  print(paste0("Unique rows in input_prelim: ", nrow(input_prelim)))
  print(paste0("Intersect of patient IDs in model_input and input_prelim: ", length(intersect(input_prelim$patient_id,model_input$patient_id))))

  model_input <- merge(model_input, input_prelim, by = "patient_id")
  print("Merged data:")
  print(Hmisc::describe(model_input))
  
  ggplot2::ggplot(model_input, ggplot2::aes(x=death_date)) + 
    ggplot2::geom_histogram(binwidth = 7) +
    ggplot2::xlim(as.Date("2020-01-01"),as.Date("2023-01-01")) +
    ggplot2::theme_minimal() 
  ggplot2::ggsave(paste0("output/hist_model_input_",cohort,".png"), unit="mm", width = 297, height = 210, bg = "white")
  
  print(paste0("Among ",nrow(model_input)," individuals in the cohort, ",sum(!is.na(model_input$death_date)), " individuals die during follow-up."))

  ## Restrict to exposed individuals -------------------------------------------
  print('Restrict to exposed individuals')
  
  model_input <- model_input[!is.na(model_input$exp_date),]
  
  ## Create variable for died within 28 days -----------------------------------
  print('Create variable for died within 28 days')
  
  model_input$death28days <- !is.na(model_input$death_date) & (model_input$death_date-model_input$exp_date)<28
  
  ## Record number died within 28 days -----------------------------------------
  print('Record number died within 28 days')
  
  print(paste0("Among ",nrow(model_input)," exposed individuals, ",sum(model_input$death28days), " die within 28 days of COVID-19 and ",sum(!is.na(model_input$death_date)), " die at any time after exposure."))
  df[nrow(df)+1,] <- c(cohort, sum(model_input$death28days), nrow(model_input))
  
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