library(dplyr)
library(data.table)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
rm(list = ls())

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/model/"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/generated-tables/"

#dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)

#----------------------------Get outcomes-----------------------------------
# Load all estimates
estimates <- read.csv(paste0(results_dir,"/master_hr_file.csv"))

# Select subgroup analyses run as main analysis (obesity, pre-diabetes)
subgroup_table1 <- subset(estimates, subgroup == "main" & event %in% event[grep("t2dm_",event)] & event != "t2dm_extended_follow_up")
subgroup_table1$subgroup <- gsub("t2dm_","",subgroup_table1$event)
subgroup_table1$event <- "t2dm"

subgroup_table2 <- subset(estimates, event %in% c("t2dm","t2dm_extended_follow_up"))

estimates <- rbind(subgroup_table1,subgroup_table2)
estimates <- estimates[!duplicated(estimates),]

estimates <- estimates %>% filter(((event == "t2dm" & cohort %in% c("vax","unvax")) | (cohort == "prevax" & (event == "t2dm_extended_follow_up" | grepl("extended_follow_up",subgroup) )))
                                  & model == "mdl_max_adj"
                                  & term %in% term[grepl("^days",term)]
                                  & time_points == "reduced") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)

estimates$subgroup <- gsub("_extended_follow_up","",estimates$subgroup)

### REMOVE SUBGROUPS THAT ARE NOT NEEDED IN FIGURES

estimates <- subset(estimates, subgroup == "agegp_18_39" | subgroup == "agegp_40_59" | subgroup == "agegp_60_79" | subgroup == "agegp_80_110" |
                        subgroup == "ethnicity_Black" | subgroup == "ethnicity_Missing" | subgroup == "ethnicity_Other" | subgroup == "ethnicity_South_Asian" |
                        subgroup == "ethnicity_White" | subgroup == "obes" | subgroup == "obes_no" | subgroup == "pd" |
                        subgroup == "pd_no" | subgroup == "sex_Female" | subgroup == "sex_Male")

# Rename subgroup to 'nice' format------------------------------------------------

estimates$subgroup <- ifelse(estimates$subgroup=="agegp_18_39","Age group: 18-39",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_40_59","Age group: 40-59",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_60_79","Age group: 60-79",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_80_110","Age group: 80-110",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="sex_Male","Sex: Male",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="sex_Female","Sex: Female",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_White","Ethnicity: White",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Mixed","Ethnicity: Mixed",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_South_Asian","Ethnicity: South Asian",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Black","Ethnicity: Black",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Other","Ethnicity: Other Ethnic Groups",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Missing","Ethnicity: Missing",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="obes","History of obesity",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="obes_no","No history of obesity",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="pd","History of pre-diabetes",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="pd_no","No history of pre-diabetes",estimates$subgroup)

estimates$event <- "Type 2 diabetes"

#-------------------------------Specify estimate format ------------------------  

estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                                                            " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                                                            "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))


estimates$subgroup <- factor(estimates$subgroup, levels = c("Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110",
                                                            "Sex: Male", "Sex: Female",
                                                            "Ethnicity: White","Ethnicity: Black","Ethnicity: South Asian","Ethnicity: Other Ethnic Groups", "Ethnicity: Mixed","Ethnicity: Missing",
                                                            "History of obesity", "No history of obesity", "History of pre-diabetes", "No history of pre-diabetes"))

estimates$cohort <- factor(estimates$cohort, levels=c("prevax","vax","unvax")) 
levels(estimates$cohort) <- list("Pre-vaccination cohort"="prevax", "Vaccinated cohort"="vax","Unvaccinated cohort"="unvax")


# Remove unnecessary variables -----------------------------------------------

estimates[,c("estimate","conf_low","conf_high","model","time_points")] <- NULL

# Convert long to wide -------------------------------------------------------
estimates <- tidyr::pivot_wider(estimates, names_from = cohort, values_from = est)
estimates <- estimates %>% select("event","subgroup","term", "Pre-vaccination cohort","Vaccinated cohort","Unvaccinated cohort")

estimates$term <- factor(estimates$term, levels = c("days0_28",
                                      "days28_197",
                                      "days197_365",
                                      "days365_714"))


estimates <- estimates[order(estimates$subgroup,estimates$term),]

write.csv(estimates, file = paste0(output_dir,"supp_table7_formatted_subgroup_hr.csv"),row.names = F)


# END