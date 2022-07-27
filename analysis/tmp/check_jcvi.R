args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort_name <- "vaccinated"
} else {
  cohort_name <- args[[1]]
}

# Load packages

library(magrittr)

# Create output directory

fs::dir_create(here::here("output", "not-for-review", "tmp"))

# Define check_jcvi function

check_jcvi <- function(cohort_name) {
  
  # Load data  
  
  input <- readr::read_rds(file.path("output", paste0("input_",cohort_name,".rds")))
  input <- input[,c("patient_id","cov_num_age","vax_cat_jcvi_group")]

  # Define age groups
  
  input <- input %>%
    dplyr::mutate(age_group = dplyr::case_when(
      cov_num_age < 18 ~ "<18",
      cov_num_age >= 18 & cov_num_age < 30 ~ "18-29",
      cov_num_age >= 30 & cov_num_age < 40 ~ "30-39",
      cov_num_age >= 40 & cov_num_age < 50 ~ "40-49",
      cov_num_age >= 50 & cov_num_age < 55 ~ "50-54",
      cov_num_age >= 55 & cov_num_age < 60 ~ "55-59",
      cov_num_age >= 60 & cov_num_age < 65 ~ "60-64",
      cov_num_age >= 65 & cov_num_age < 70 ~ "65-69",
      cov_num_age >= 70 & cov_num_age < 75 ~ "70-74",
      cov_num_age >= 75 & cov_num_age < 80 ~ "75-79",
      cov_num_age >= 80 & cov_num_age <= 110 ~ "80-110",
      cov_num_age > 110 ~ "<110"))
  
  # Count individuals by age and jcvi group
  
  df <- data.frame(table(input$age_group, input$vax_cat_jcvi_group))
  
  df <- dplyr::rename(df, 
                      "age_group" = "Var1", 
                      "jcvi_group" = "Var2")
  
  # Tidy data
  
  df <- tidyr::pivot_wider(df, 
                           names_from = jcvi_group, 
                           names_prefix = "jcvi_",
                           values_from = "Freq")
  
  df <- df[,c("age_group",paste0("jcvi_",stringr::str_pad(1:12, 2, pad = "0")),"jcvi_99")]
 
  # Save data
  
  write.csv(df, file = file.path(paste0("output/not-for-review/tmp/check_jcvi_",cohort_name,".csv")), row.names=F)
   
}

if (cohort_name == "both") {
  check_jcvi("electively_unvaccinated")
  check_jcvi("vaccinated")
} else{
  check_jcvi(cohort_name)
}