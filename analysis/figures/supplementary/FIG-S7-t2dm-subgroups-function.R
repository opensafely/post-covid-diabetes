subgroup_fig <- function(cohort){
#-----------------------Determine active outcome events-------------------------
# We are not using active analysis here. "t2dm" is directly selected in the next section of the code

#active_analyses <- read_rds("lib/active_analyses.rds")
#active_analyses$outcome_variable <- gsub("out_date_","",active_analyses$outcome_variable)

#active_analyses_table <- subset(active_analyses, active_analyses$active =="TRUE" & grepl("t2dm", active_analyses$outcome_variable)) 
#outcome_name_table <- active_analyses_table %>% select(outcome, outcome_variable,outcome_group)
#outcome_to_plot <- outcome_name_table$outcome_variable

#--------Load fully adjusted main and COVID phenotype results-------------------

combined_hr <- read.csv(paste0(results_dir,"/master_hr_file.csv"))
combined_hr <- combined_hr %>%
    filter(combined_hr$term != "days_pre" | grepl("hospital", combined_hr$subgroup))

# Get estimates for subgroup analyses for t2dm
# Select subgroup analyses run as main analysis (obesity, pre-diabetes)
subgroup_table1 <- combined_hr %>%
  filter(grepl("obes|pd", combined_hr$event) & combined_hr$subgroup=="main")
subgroup_table1$subgroup <- gsub("t2dm_","",subgroup_table1$event)
subgroup_table1$event[subgroup_table1$cohort == "vax"|subgroup_table1$cohort == "unvax"] <- "t2dm" 

subgroup_table2 <- subset(combined_hr, event %in% c("t2dm","t2dm_extended_follow_up"))
subgroup_table2 <- subgroup_table2 %>% 
  filter(grepl("^sub_", subgroup_table2$subgroup))

combined_hr <- rbind(subgroup_table1,subgroup_table2)
#combined_hr$event <- "t2dm"

combined_hr <- combined_hr[!duplicated(combined_hr),]
# exclude minimally adjusted models
combined_hr <- combined_hr[combined_hr$model!="mdl_age_sex",]


if (cohort == "prevax"){
  combined_hr <- combined_hr %>% filter(cohort == "prevax"
                                        & (event == "t2dm_extended_follow_up" | grepl("extended_follow_up",subgroup)))
  combined_hr$event <- ifelse(combined_hr$event == "t2dm_extended_follow_up", "t2dm", combined_hr$event)
  combined_hr$subgroup <- gsub("_extended_follow_up","",combined_hr$subgroup)
} else if (cohort == "vax"){
  combined_hr <- combined_hr %>% filter(cohort == "vax"
                                        & event == "t2dm"
                                        & !grepl("extended_follow_up",subgroup))
} else if (cohort == "unvax"){
  combined_hr <- combined_hr %>% filter(cohort == "unvax"
                                        & event == "t2dm"
                                        & !grepl("extended_follow_up",subgroup))
}

#-------------------------Filter to active outcomes-----------------------------

combined_hr <- combined_hr %>% mutate_at(c("estimate","conf_low","conf_high", "median_follow_up"), as.numeric)

### REMOVE SUBGROUPS THAT ARE NOT NEEDED IN FIGURES
combined_hr$subgroup <- gsub("sub_","", combined_hr$subgroup)

combined_hr <- subset(combined_hr, subgroup == "age_18_39" | subgroup == "age_40_59" | subgroup == "age_60_79" | subgroup == "age_80_110" |
                        subgroup == "ethnicity_black" | subgroup == "ethnicity_missing" | subgroup == "ethnicity_other" | subgroup == "ethnicity_asian" |
                        subgroup == "ethnicity_white" | subgroup == "ethnicity_mixed" | subgroup == "obes" | subgroup == "obes_no" | subgroup == "pd" |
                        subgroup == "pd_no" | subgroup == "sex_female" | subgroup == "sex_male")


# Rename subgroup to 'nice' format------------------------------------------------
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="age_18_39","Age group: 18-39",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="age_40_59","Age group: 40-59",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="age_60_79","Age group: 60-79",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="age_80_110","Age group: 80-110",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="sex_male","Sex: Male",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="sex_female","Sex: Female",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_white","Ethnicity: White",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_mixed","Ethnicity: Mixed",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_asian","Ethnicity: South Asian",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_black","Ethnicity: Black",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_other","Ethnicity: Other Ethnic Groups",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_missing","Ethnicity: Missing",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="obes","History of obesity",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="obes_no","No history of obesity",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="pd","History of pre-diabetes",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="pd_no","No history of pre-diabetes",combined_hr$subgroup)

# Give ethnicity estimates extra space -----------------------------------------

#combined_hr$time <- ifelse(combined_hr$subgroup=="Ethnicity: South Asian", combined_hr$time-0.25, combined_hr$time)
#combined_hr$time <- ifelse(combined_hr$subgroup=="Ethnicity: Other Ethnic Groups", combined_hr$time-0.5, combined_hr$time)
#combined_hr$time <- ifelse(combined_hr$subgroup=="Ethnicity: Mixed", combined_hr$time+0.25, combined_hr$time)
#combined_hr$time <- ifelse(combined_hr$subgroup=="Ethnicity: Black", combined_hr$time+0.5, combined_hr$time)

# Give age estimates extra space -----------------------------------------

#combined_hr$time <- ifelse(combined_hr$subgroup=="Age group: 40-59", combined_hr$time-0.25, combined_hr$time)
#combined_hr$time <- ifelse(combined_hr$subgroup=="Age group: 60-79", combined_hr$time-0.5, combined_hr$time)
#combined_hr$time <- ifelse(combined_hr$subgroup=="Age group: 80-110", combined_hr$time+0.25, combined_hr$time)

# Specify line colours ---------------------------------------------------------

combined_hr$colour <- ""
# combined_hr$colour <- ifelse(combined_hr$subgroup=="Extensive adjustment","#000000",combined_hr$colour)
# combined_hr$colour <- ifelse(combined_hr$subgroup=="Age/sex adjustment","#bababa",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Age group: 18-39","#006d2c",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Age group: 40-59","#31a354",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Age group: 60-79","#74c476",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Age group: 80-110","#bae4b3",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Sex: Male","#bababa",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Sex: Female","#000000",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: White","#08519c",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: Black","#2171b5",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: South Asian","#4292c6",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: Other Ethnic Groups","#6baed6",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: Mixed","#9ecae1",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: Missing","#c5dfed",combined_hr$colour)
#combined_hr$colour <- ifelse(combined_hr$subgroup=="Prior history of event","#ff7f00",combined_hr$colour)
#combined_hr$colour <- ifelse(combined_hr$subgroup=="No prior history of event","#fdbf6f",combined_hr$colour)
# combined_hr$colour <- ifelse(combined_hr$subgroup=="Non-hospitalised COVID-19","#000000",combined_hr$colour)
# combined_hr$colour <- ifelse(combined_hr$subgroup=="Hospitalised COVID-19","#bababa",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="History of obesity","#fb9a99",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="No history of obesity","#e31a1c",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="History of pre-diabetes","#ff7f00",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="No history of pre-diabetes","#fdbf6f",combined_hr$colour)

# Make event names 'nice' ------------------------------------------------------

#combined_hr <- combined_hr %>% left_join(active_analyses %>% select(outcome, outcome), by = c("event"="outcome"))

#Add in which subgroup stratified-----------------------------------------------------------

combined_hr$grouping=""
# combined_hr$grouping=ifelse(combined_hr$subgroup=="Extensive adjustment","Overall",combined_hr$grouping)
# combined_hr$grouping=ifelse(combined_hr$subgroup=="Age/sex adjustment","Overall",combined_hr$grouping)
# combined_hr$grouping=ifelse(combined_hr$subgroup=="Hospitalised COVID-19","Hospitalised/Non-hospitalised COVID-19",combined_hr$grouping)
# combined_hr$grouping=ifelse(combined_hr$subgroup=="Non-hospitalised COVID-19","Hospitalised/Non-hospitalised COVID-19",combined_hr$grouping)
#combined_hr$grouping=ifelse(endsWith(combined_hr$subgroup,"event")==T,"Prior history of event",combined_hr$grouping)
combined_hr$grouping=ifelse(startsWith(combined_hr$subgroup,"Age group")==T,"Age group",combined_hr$grouping)
combined_hr$grouping=ifelse(startsWith(combined_hr$subgroup,"Sex")==T,"Sex",combined_hr$grouping)
combined_hr$grouping=ifelse(startsWith(combined_hr$subgroup,"Ethnicity")==T,"Ethnicity",combined_hr$grouping)
combined_hr$grouping=ifelse(combined_hr$subgroup=="History of obesity","Obesity",combined_hr$grouping)
combined_hr$grouping=ifelse(combined_hr$subgroup=="No history of obesity","Obesity",combined_hr$grouping)
combined_hr$grouping=ifelse(combined_hr$subgroup=="History of pre-diabetes","Pre-diabetes",combined_hr$grouping)
combined_hr$grouping=ifelse(combined_hr$subgroup=="No history of pre-diabetes","Pre-diabetes",combined_hr$grouping)


# Plot figure

df=combined_hr 

if(nrow(df)>0){
  group_levels <-c()
  #for(i in c("Overall","Hospitalised/Non-hospitalised COVID-19", "Prior history of event","Age group","Sex","Ethnicity" )){
  # for(i in c("Age group","Sex","Ethnicity", "Obesity", "Pre-diabetes")){
  # without SEX
  for(i in c("Age group", "Sex","Ethnicity", "Obesity", "Pre-diabetes")){
    levels_available <- unique(df$grouping)
    if(i %in% levels_available){
      group_levels <- append(group_levels,i)
    }
  }
  
  df$grouping <- factor(df$grouping, levels=group_levels)
  
  sub_group_levels <-c()
  for(i in c(
    # "Hospitalised COVID-19", "Non-hospitalised COVID-19",
    "Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110",
    "Sex: Male", "Sex: Female",
    "Ethnicity: White","Ethnicity: Black","Ethnicity: South Asian","Ethnicity: Other Ethnic Groups", "Ethnicity: Mixed","Ethnicity: Missing",
    "History of obesity", "No history of obesity", "History of pre-diabetes", "No history of pre-diabetes")){
    levels_available <- unique(df$subgroup)
    if(i %in% levels_available){
      sub_group_levels <- append(sub_group_levels,i)
    }
  }
  
  df$subgroup <- factor(df$subgroup, levels=sub_group_levels)
  
  colour_levels <-c()
  
  for(i in c("#006d2c",
             "#31a354",
             "#74c476",
             "#bae4b3",
             "#bababa",
             "#000000",
             "#08519c",
             "#2171b5",
             "#4292c6",
             "#6baed6",
             "#9ecae1",
             "#c5dfed",
             "#fb9a99",
             "#e31a1c",
             "#ff7f00",
             "#fdbf6f")){
    levels_available <- unique(df$colour)
    if(i %in% levels_available){
      colour_levels <- append(colour_levels,i)
    }
  } 
  
  # Plot figures------------------------------------------------------------------
  min_plot <- 0.25
  max_plot <- 32
  p <- ggplot2::ggplot(data = df, 
                       mapping = ggplot2::aes(x = median_follow_up, y = estimate, color = subgroup, shape = subgroup, fill = subgroup)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0), size = 2)+
    ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<min_plot,min_plot,conf_low), 
                                                              ymax = ifelse(conf_high>max_plot,max_plot,conf_high),  
                                                              width = 0), 
                           position = ggplot2::position_dodge(width = 0))+
   # ggplot2::geom_line(position = ggplot2::position_dodge(width = 0)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
    # ggplot2::scale_x_continuous(lim = c(0,round_any(max(df$median_follow_up, na.rm = T),4, f= ceiling)), breaks = seq(0,round_any(max(df$median_follow_up, na.rm = T),4, f= ceiling),4)) +
    ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
    ggplot2::scale_fill_manual(values = colour_levels, labels = sub_group_levels)+ 
    ggplot2::scale_color_manual(values = colour_levels, labels = sub_group_levels) +
    ggplot2::scale_shape_manual(values = c(rep(21,17)), labels = sub_group_levels) +
    ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis",
                  y = ifelse(df$cohort=="prevax","Hazard ratio and 95% confidence interval",
                             " ")) +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol = 7, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white")) +
    ggtitle(ifelse(cohort == "prevax",
                   paste0("Pre-Vaccinated \n1 Jan 2020 to 14 Dec 2021"),
                   ifelse(cohort == "vax",
                          paste0("Vaccinated \n1 Jun 2021 to 14 Dec 2021"),
                          paste0("Unvaccinated \n1 Jun 2021 to 14 Dec 2021")))) +
    theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
    theme(legend.text = element_text(face="bold", size = 12),
          legend.position = "bottom",
          legend.title = element_blank()) +
    ggplot2::facet_wrap(grouping~.,ncol=1) +   
    theme(text = element_text(size = 15)) 
  return(p)
}
}
