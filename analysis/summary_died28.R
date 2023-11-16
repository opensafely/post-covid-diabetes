# Define function to calculate deaths within 28 days of COVID-19 ---------------
print('Define function to calculate deaths within 28 days of COVID-19')

get_died28 <- function(input) {
  
  df <- readr::read_csv(paste0("output/",input,".csv"),
                        col_select = c("patient_id","expo_date","DATE_OF_DEATH"),
                        col_types = c("c","D","D"))
  
  exposed <- nrow(df[!is.na(df$expo_date),])
  
  df <- df[!is.na(df$expo_date) & !is.na(df$DATE_OF_DEATH),]
  
  df$diedwithin <- as.numeric(df$DATE_OF_DEATH - df$expo_date)
  
  died28 <- data.frame(input = input,
                    exposed = exposed,
                    died28 = nrow(df[df$diedwithin<=28,]))
  
  row.names(died28) <- NULL
  
  return(fup)
  
}

# List input files -------------------------------------------------------------
print('List input files')

input_files <- c("input_t2dm_main_prevax_normal_time_periods",
                 "input_t2dm_main_vax_normal_time_periods",
                 "input_t2dm_main_unvax_normal_time_periods")

# Calculate deaths within 28 days of COVID-19 for each input file --------------
print('Calculate deaths within 28 days of COVID-19 for each input file')

df <- data.frame(input = character(),
                 exposed = numeric(),
                 died28 = numeric())

for (input in input_files) {
  
  died28 <- get_died28(input = input)
  print(died28)
  df <- rbind(df,died28)
}

# Save deaths within 28 days of COVID-19 table ---------------------------------
print('Save deaths within 28 days of COVID-19 table')

write.csv(df, "output/summary_died28.csv", row.names = FALSE)