# Define function to calculate follow-up ---------------------------------------
print('Define function to calculate follow-up')

get_fup <- function(input) {
  
  df <- readr::read_csv(paste0("output/",input,".csv"),
                        col_select = c("patient_id","tstart","tstop"),
                        col_types = c("c","D","D"))
  
  df$fup <- as.numeric(df$tstop - df$tstart)
  
  fup <- data.frame(input = input,
                    q25 = quantile(df$fup,0.25),
                    q50 = quantile(df$fup,0.50),
                    q75 = quantile(df$fup,0.75))
  
  row.names(fup) <- NULL
  
  return(fup)
  
}

# List input files -------------------------------------------------------------
print('List input files')

input_files <- c("input_t2dm_main_prevax_normal_time_periods",
                 "input_t2dm_main_vax_normal_time_periods",
                 "input_t2dm_main_unvax_normal_time_periods")

# Calculate follow-up for each input file --------------------------------------
print('Calculate follow-up for each input file')

df <- data.frame(input = character(),
                 q25 = numeric(),
                 q50 = numeric(),
                 q75 = numeric())

for (input in input_files) {
  
  fup <- get_fup(input = input)
  print(fup)
  df <- rbind(df,fup)
}

# Save follow-up table ---------------------------------------------------------
print('Save follow-up table')

write.csv(df, "output/summary_follow_up.csv", row.names = FALSE)