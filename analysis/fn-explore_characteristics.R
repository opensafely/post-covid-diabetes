explore_characteristics <- function(death_dates, cohort, patients) {
  
  # Load data ----
  
  input_cohort <- arrow::read_feather(file = paste0("output/input_",cohort,".feather"))
  
  # Add death date ----
  
  input_cohort <- merge(input_cohort, death_dates, by = "patient_id")
  
  # Restrict to relevant people ----
  
  input_cohort <- input_cohort[input_cohort$patient_id %in% patients,]
  
  # Restrict to relevant variables ----
  
  input_cohort <- input_cohort[,c("patient_id",
                                  "qa_num_birth_year",
                                  "dereg_date",
                                  "death_date",
                                  "qa_bin_prostate_cancer",
                                  "qa_bin_pregnancy",
                                  "has_follow_up_previous_6months",
                                  "has_died",
                                  "registered_at_start",
                                  "registered_as_of_6months_before_delta",
                                  "registered_as_of_pandemic_start",
                                  "registered_as_of_6months_before_pandemic_start",
                                  colnames(input_cohort)[grepl("cov_",colnames(input_cohort))])]
  
  input_cohort[,c(colnames(input_cohort)[grepl("tmp_",colnames(input_cohort))],
                  "cov_num_bmi_date_measured","cov_num_bmi")] <- NULL
  
  # Sort dates
  
  input_cohort$dereg_date <- as.Date(input_cohort$dereg_date)
  input_cohort$qa_num_birth_year <- as.numeric(format(as.Date(input_cohort$qa_num_birth_year, format="%d/%m/%Y"),"%Y"))
  input_cohort$death_year <- as.numeric(format(as.Date(input_cohort$death_date, format="%d/%m/%Y"),"%Y"))
  
  # Determine variable type ----
  
  date_vars <- c("dereg_date")
    
  cat_vars <- c(colnames(input_cohort)[grepl("cov_cat_",colnames(input_cohort))])
  
  bin_vars <- c(colnames(input_cohort)[grepl("cov_bin_",colnames(input_cohort))],
                colnames(input_cohort)[grepl("qa_bin_",colnames(input_cohort))],
                colnames(input_cohort)[grepl("registered_",colnames(input_cohort))],
                colnames(input_cohort)[grepl("has_",colnames(input_cohort))])
  
  num_vars <- c(colnames(input_cohort)[grepl("cov_num_",colnames(input_cohort))],
                colnames(input_cohort)[grepl("qa_num_",colnames(input_cohort))])
  
  # Create empty summary table ----
  
  df <- data.frame(variable = character(),
                   category = character(),
                   summary = numeric(),
                   stringsAsFactors = FALSE)
  
  # Summarize numeric variables ---
  
  input_cohort$birth_after_2020 <- input_cohort$qa_num_birth_year>2019
  input_cohort$birth_after_death <- input_cohort$qa_num_birth_year>=input_cohort$death_year
  
  input_cohort$cov_cat_age_group <- ""
  input_cohort$cov_cat_age_group <- ifelse(input_cohort$cov_num_age>=18 & input_cohort$cov_num_age<=29, "18-29", input_cohort$cov_cat_age_group)
  input_cohort$cov_cat_age_group <- ifelse(input_cohort$cov_num_age>=30 & input_cohort$cov_num_age<=39, "30-39", input_cohort$cov_cat_age_group)
  input_cohort$cov_cat_age_group <- ifelse(input_cohort$cov_num_age>=40 & input_cohort$cov_num_age<=49, "40-49", input_cohort$cov_cat_age_group)
  input_cohort$cov_cat_age_group <- ifelse(input_cohort$cov_num_age>=50 & input_cohort$cov_num_age<=59, "50-59", input_cohort$cov_cat_age_group)
  input_cohort$cov_cat_age_group <- ifelse(input_cohort$cov_num_age>=60 & input_cohort$cov_num_age<=69, "60-69", input_cohort$cov_cat_age_group)
  input_cohort$cov_cat_age_group <- ifelse(input_cohort$cov_num_age>=70 & input_cohort$cov_num_age<=79, "70-79", input_cohort$cov_cat_age_group)
  input_cohort$cov_cat_age_group <- ifelse(input_cohort$cov_num_age>=80 & input_cohort$cov_num_age<=89, "80-89", input_cohort$cov_cat_age_group)
  input_cohort$cov_cat_age_group <- ifelse(input_cohort$cov_num_age>=90, "90+", input_cohort$cov_cat_age_group)
  
  input_cohort$cov_cat_consulation_rate_group <- ""
  input_cohort$cov_cat_consulation_rate_group <- ifelse(input_cohort$cov_num_consulation_rate==0, "0", input_cohort$cov_cat_consulation_rate_group)
  input_cohort$cov_cat_consulation_rate_group <- ifelse(input_cohort$cov_num_consulation_rate>=1 & input_cohort$cov_num_consulation_rate<=5, "1 to 6", input_cohort$cov_cat_consulation_rate_group)
  input_cohort$cov_cat_consulation_rate_group <- ifelse(input_cohort$cov_num_consulation_rate>=6, "6+", input_cohort$cov_cat_consulation_rate_group)
  
  # Sumamrize variables ----
  
  for (v in c(bin_vars,cat_vars,"birth_after_2020","birth_after_death","cov_cat_age_group","cov_cat_consulation_rate_group")) {
    
    tmp1 <- input_cohort[,c("patient_id",v)]
    colnames(tmp1) <- c("patient_id","variable")
    
    tmp2 <- as.data.frame(table(tmp1$variable))
    tmp2$variable <- v
    tmp2 <- dplyr::rename(tmp2, "category" = "Var1", "summary" = "Freq")
    tmp2 <- tmp2[,colnames(df)]
    df <- rbind(df, tmp2)

    df[nrow(df)+1,] <- c(v, "NA", nrow(tmp1[is.na(tmp1$variable),]))
    
  }
  
  # Return ----
  
  return(df)
  
}