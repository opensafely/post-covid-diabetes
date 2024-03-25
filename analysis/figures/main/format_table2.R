### IMPORTANT ###
# This will save directly to the EHR sharepoint and will replace the file saved there

library(dplyr)
library(broman)
library(stringr)
# Load data --------------------------------------------------------------------

#results_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/descriptive/"
#output_dir <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/generated-figures/"

# results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"
# output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"
# dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)


#############################################################
# Alternative formatting of table 2 with cohorts as columns #
#############################################################

# Get active analysis table for labels -----------------------------------------
active_analyses <- readr::read_rds("lib/active_analyses.RDS")

# Get data from each cohort ----------------------------------------------------
table2_pre_vax <- read.csv("C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/descriptive/table2_prevax_rounded.csv")
table2_vax <- read.csv("C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/descriptive/table2_vax_rounded.csv")
table2_unvax <- read.csv("C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/descriptive/table2_unvax_rounded.csv")

df <-rbind(table2_pre_vax, table2_unvax, table2_vax)
# tidying prevax outcome names
df$outcome <- stringr::str_remove(df$outcome, "_extended_follow_up")
df$outcome[df$outcome == "out_date_t2dm" & (df$analysis == "main_fup4m"|
                                            df$analysis == "sub_covid_hospitalised_fup4m"|
                                            df$analysis == "sub_covid_nonhospitalised_fup4m")] <- "out_date_t2dm_fup4m"
df <- subset(df, name!= "cohort_prevax-main-t2dm_follow_extended_follow_up")
df <- subset(df, name!= "cohort_prevax-sub_covid_hospitalised-t2dm_follow_extended_follow_up")
df <- subset(df, name!= "cohort_prevax-sub_covid_nonhospitalised-t2dm_follow_extended_follow_up")    
df$analysis <- stringr::str_remove(df$analysis, "_fup4m")
     

# Keep totals ------------------------------------------------------------------
print("Keep totals")

totals <- unique(df[df$analysis=="main",c("cohort","sample_size_midpoint6")])

totals <- tidyr::pivot_wider(totals,
                             names_from = "cohort",
                             values_from = c("sample_size_midpoint6"))

totals <-dplyr::rename(totals,
                       "event_personyears_prevax" = "prevax",
                       "event_personyears_vax" = "vax",
                       "event_personyears_unvax" = "unvax")

totals$outcome_label <- "N"

# Filter data ------------------------------------------------------------------
print("Filter data")

df <- df[df$analysis %in% c("main","sub_covid_hospitalised","sub_covid_nonhospitalised"),]

df$events <- ifelse(df$analysis=="main", df$unexposed_events, df$exposed_events)
df$person_days <- ifelse(df$analysis=="main", df$unexposed_person_days, df$exposed_person_days)

df <- df[,c("cohort","analysis","outcome","events","person_days")]

# Add plot labels --------------------------------------------------------------
print("Add plot labels")

plot_labels <- readr::read_csv("lib/plot_labels.csv",
                               show_col_types = FALSE)

df$outcome <- gsub("out_date_","",df$outcome)
df <- merge(df, plot_labels, by.x = "outcome", by.y = "term", all.x = TRUE)
df <- dplyr::rename(df, "outcome_label" = "label")

df <- merge(df, plot_labels, by.x = "analysis", by.y = "term", all.x = TRUE)
df <- dplyr::rename(df, "covid19_severity" = "label")
df$covid19_severity <- ifelse(df$covid19_severity=="All COVID-19","No COVID-19",df$covid19_severity)
df$covid19_severity <- factor(df$covid19_severity, levels = c("No COVID-19","Hospitalised COVID-19","Non-hospitalised COVID-19"))

# Add other columns ------------------------------------------------------------
print("Add other columns")

df$event_personyears <- paste0(df$events,"/", round((df$person_days/365.25)))
df$incidencerate <- round(df$events/((df$person_days/365.25)/100000))

# Pivot table ------------------------------------------------------------------
print("Pivot table")

df <- df[,c("cohort","outcome_label","covid19_severity","event_personyears","incidencerate")]

df <- tidyr::pivot_wider(df, 
                         names_from = "cohort",
                         values_from = c("event_personyears","incidencerate"))

# Add totals to table ----------------------------------------------------------
print("Add totals to table")

df <- plyr::rbind.fill(totals, df)

# Order outcomes ---------------------------------------------------------------
print("Order outcomes")

df$outcome_label <- factor(df$outcome_label,
                           levels = c("N",
                                      "Type 2 Diabetes",
                                      "Type 1 Diabetes",
                                      "Gestational Diabetes",
                                      "Other or non-specified Diabetes",
                                      "Type 2 diabetes 4 month follow up",
                                      "Persistent type 2 diabetes"))

# Tidy table -------------------------------------------------------------------
print("Tidy table")

df <- df[order(df$outcome_label,df$covid19_severity),
         c("outcome_label","covid19_severity",
           paste0(c("event_personyears","incidencerate"),"_prevax"),
           paste0(c("event_personyears","incidencerate"),"_vax"),
           paste0(c("event_personyears","incidencerate"),"_unvax"))]

df <- dplyr::rename(df,
                    "Outcome" = "outcome_label",
                    "COVID-19 severity" = "covid19_severity")

# Save table -------------------------------------------------------------------
print("Save table")

df2 = data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
write.csv(df2, paste0("C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/generated-tables/formatted_table_2.csv"),row.names = F)


