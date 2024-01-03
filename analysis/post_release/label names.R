# Make plot lables table

term <- c("prevax", "vax","unvax","main","sub_age_18_39","sub_age_40_59",
          "sub_age_60_79","sub_age_80_110","sub_covid_hospitalised",
          "sub_covid_nonhospitalised","sub_ethnicity_asian", 
          "sub_ethnicity_black","sub_ethnicity_mixed","sub_ethnicity_other",
          "sub_ethnicity_white","sub_smoking_ever", "sub_smoking_never", 
          "sub_smoking_current","sub_sex_female,Sex","sub_sex_male,Sex",
          "sub_covid_history", "t1dm","t2dm","gestationaldm","otherdm",
          "t2dm_follow")
label <- c("Pre-vaccination (Jan 1 2020 - Dec 14 2021)", "Vaccinated (Jun 1 2021 - Dec 14 2021)",
          "Unvaccinated (Jun 1 2021 - Dec 14 2021)","All COVID-19","Age group: 18-39",
          "Age group: 40-59","Age group: 60-79","Age group: 80-110","Hospitalised COVID-19",
          "Non-hospitalised COVID-19","Ethnicity: South Asian","Ethnicity: Black",
          "Ethnicity: Mixed","Ethnicity: Other","Ethnicity: White","Smoking status: Former",
          "Smoking status: Never","Smoking status: Current","Sex: Female","Sex: Male",
          "History of COVID-19","Type 1 Diabetes","Type 2 Diabetes","Gestational Diabetes",
          "Other or non-specified Diabetes","Persistent type 2 diabetes")

df <- data.frame(term, label)
write.csv(df, "C:\\Users\\rd16568\\Documents\\Github\\post-covid-diabetes\\lib\\plot_labels.csv")