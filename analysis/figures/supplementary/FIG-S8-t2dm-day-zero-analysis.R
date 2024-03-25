#libraries
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)

#dir <- ("~/Library/CloudStorage/OneDrive-UniversityofBristol/ehr_postdoc/projects/post-covid-diabetes")
#setwd(dir)

results_dir <- "C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/model/"
output_dir <- "C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/generated-figures/supplementary/"

#-------------------------#
# 2. Get outcomes to plot #
#-------------------------#
#active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

#outcome_name_table <- active_analyses %>% 
#  select(outcome, outcome_variable) %>% 
#  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))

#outcomes_to_plot <- outcome_name_table$outcome_name
outcomes_to_plot <- c("t2dm", "t2dm_extended_follow_up")

#---------------------------------------------#
# 3. Load all estimates #
#---------------------------------------------#

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/master_hr_file.csv"))
unique(estimates$event)
unique(estimates$subgroup)

# Get estimates for main analyses and list of outcomes from active analyses
main_estimates <- estimates %>% filter(event %in% outcomes_to_plot 
                                       & term %in% term[grepl("^days",term)]
                                       & model == "mdl_max_adj"
                                       & subgroup %in% subgroup[grepl("day0", subgroup)]) %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,median_follow_up)


main_estimates <- main_estimates %>% dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))

# We want to plot the figures using the same time-points across all cohorts so that they can be compared
# If any cohort uses reduced time points then all cohorts will be plotted with reduced time points
# main_estimates <- main_estimates %>%
#   group_by(event,subgroup,cohort) %>%
#   dplyr::mutate(time_period_to_plot = case_when(
#     any(time_points == "normal") ~ "normal",
#     TRUE ~ "reduced"))
# 
# main_estimates <- main_estimates %>%
#   group_by(event,subgroup) %>%
#   dplyr::mutate(time_period_to_plot = case_when(
#     any(time_period_to_plot == "reduced") ~ "reduced",
#     TRUE ~ "normal"))

#---------------------------Specify time to plot--------------------------------
# main_estimates$add_to_median <- sub("days","",main_estimates$term)
# main_estimates$add_to_median <- as.numeric(sub("\\_.*","",main_estimates$add_to_median))
# 
# main_estimates$median_follow_up <- ((main_estimates$median_follow_up + main_estimates$add_to_median)-1)/7
# main_estimates$median_follow_up <- ifelse(main_estimates$median_follow_up == 0, 0.001,main_estimates$median_follow_up )
# 

#term_to_time <- data.frame(term = c("days0_7","days7_14", "days14_28", "days28_56", "days56_84", "days84_197","days197_535", 
#                                    "days0_28","days28_197","days28_535"),
#                           time = c(0.5,1.5,3,6,10,20,52,
#                                    2,16,40))
#main_estimates <- merge(main_estimates, term_to_time, by = c("term"), all.x = TRUE)


#------------------------------------------#
# 4. Specify groups and their line colours #
#------------------------------------------#
# Specify colours
main_estimates$colour <- ""
main_estimates$colour <- ifelse(main_estimates$cohort=="prevax","#d2ac47",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$cohort=="vax","#58764c",main_estimates$colour) # Grey
main_estimates$colour <- ifelse(main_estimates$cohort=="unvax","#0018a8",main_estimates$colour) # Black

#Specify lines
main_estimates$linetype <- ""
main_estimates$linetype <- ifelse(main_estimates$subgroup=="day0_sub_covid_hospitalised","solid",main_estimates$linetype)
main_estimates$linetype <- ifelse(main_estimates$subgroup=="day0_sub_covid_nonhospitalised","dashed",main_estimates$linetype)

# Factor variables for ordering
main_estimates$cohort <- factor(main_estimates$cohort, levels=c("prevax","vax","unvax")) 
main_estimates$colour <- factor(main_estimates$colour, levels=c("#d2ac47","#58764c","#0018a8"))
main_estimates$linetype <- factor(main_estimates$linetype,levels = c("solid","dashed"))
main_estimates$subgroup <- factor(main_estimates$subgroup,levels = c("day0_main", "day0_sub_covid_hospitalised","day0_sub_covid_nonhospitalised"))

# Rename adjustment groups
levels(main_estimates$cohort) <- list("Pre-Vaccination (2020-01-01 - 2021-06-18)"="prevax", "Vaccinated (2021-06-01 - 2021-12-14)"="vax","Unvaccinated (2021-06-01 - 2021-12-14)"="unvax")

# Order outcomes for plotting
# Use the nice names from active_analyses table i.e. outcome_name_table
main_estimates$outcome==""
main_estimates <- main_estimates %>% mutate(outcome = case_when(subgroup=="day0_main" ~ "All COVID-19",
                                                      subgroup=="day0_sub_covid_hospitalised" ~ "Hospitalised COVID-19",
                                                      subgroup=="day0_sub_covid_nonhospitalised" ~ "Non-hospitalised COVID-19"))
main_estimates$outcome <- str_to_title(main_estimates$outcome)
main_estimates$outcome <- factor(main_estimates$outcome, levels=c("Type 2 Diabetes"))

# df <- main_estimates %>% dplyr::filter(time_points == "reduced")

df <- main_estimates
df <- df[df$term!="days_pre",]

# MAIN --------------------------------------------------------------------

# set desired dodge width
pd <- position_dodge2(width = 0.5)

df_main <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="day0_main")

main <- ggplot2::ggplot(data=df_main,
                        mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(),size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                            ymax = ifelse(conf_high>1024,1024,conf_high),  
                                                            width = 0))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
   ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  #ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.25,1024), breaks = c(0.25,0.5,1,2,4,8,16,32,64,128,256,512,1024), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
  #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
  ggplot2::labs(x = "\n ", y = "Hazard ratio and 95% confidence interval") +
  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, nrow = 3, byrow = TRUE)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.spacing.x = ggplot2::unit(0.5, "lines"),
                 panel.spacing.y = ggplot2::unit(0, "lines"),
                 legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                 legend.title = ggplot2::element_blank(),
                 legend.position="bottom",
                 plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                 plot.margin = margin(1,1,1,1, "cm")) +   
  theme(text = element_text(size = 12)) # +
#  theme(legend.text = element_blank())



# HOSPITALISED ------------------------------------------------------------

df_hosp <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="day0_sub_covid_hospitalised")

hosp <- ggplot2::ggplot(data=df_hosp,
                        mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(),size = 2, position = pd) + 
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                            ymax = ifelse(conf_high>1024,1024,conf_high),  
                                                            width = 0))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  #ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.25,1024), breaks = c(0.25,0.5,1,2,4,8,16,32,64,128,256,512,1024), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
  #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
  ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = NULL) +
  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, nrow = 3, byrow = TRUE)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.spacing.x = ggplot2::unit(0.5, "lines"),
                 panel.spacing.y = ggplot2::unit(0, "lines"),
                 legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                 legend.title = ggplot2::element_blank(),
                 legend.position="bottom",
                 plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                 plot.margin = margin(1,1,1,1, "cm")) +   
  theme(text = element_text(size = 12)) # +
#  theme(legend.text = element_blank())

# ggplot2::ggsave(paste0(output_dir,"Figure2_covid_pheno_HOSP_all_cohorts_TEST.png"), height = 297, width = 230, unit = "mm", dpi = 600, scale = 1)


# NON HOSPITALISED --------------------------------------------------------


df_nonhosp <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="day0_sub_covid_nonhospitalised")

non_hosp <- ggplot2::ggplot(data=df_nonhosp,
                            mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(), size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                            ymax = ifelse(conf_high>1024,1024,conf_high),  
                                                            width = 0))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  #ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.25,1024), breaks = c(0.25,0.5,1,2,4,8,16,32,64,128,256,512,1024), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
  #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
  ggplot2::labs(x = "\n ", y = NULL) +
  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, nrow = 3, byrow = TRUE)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.spacing.x = ggplot2::unit(0.5, "lines"),
                 panel.spacing.y = ggplot2::unit(0, "lines"),
                 legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                 legend.title = ggplot2::element_blank(),
                 legend.position="bottom",
                 legend.spacing.y = unit(0.01, 'cm'),
                 plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                 plot.margin = margin(1,1,1,1, "cm")) +   
  theme(text = element_text(size = 12)) # +
#  theme(legend.text = element_blank())

# ggplot2::ggsave(paste0(output_dir,"Figure2_covid_pheno_NON_HOSP_all_cohorts_TEST.png"), height = 297, width = 230, unit = "mm", dpi = 600, scale = 1)

# PLOT WITHOUT TABLE ------------------------------------------------------

png(paste0(output_dir,"t2dm_day0_extended_3plots.png"),
    units = "mm", width=330, height=195, res = 1000)
ggpubr::ggarrange(main, hosp, non_hosp, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  labels = c("A: All COVID-19", "B: Hospitalised-COVID-19", "C: Non-Hospitalised-COVID-19"),
                  hjust = -0.1,
                  font.label = list(size = 12)) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 
dev.off() 






######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
