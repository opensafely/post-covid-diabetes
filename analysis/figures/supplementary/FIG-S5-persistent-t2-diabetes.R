#libraries
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)

#---------------------------------------------#
# Load all estimates #
#---------------------------------------------#


results_dir <- "/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/model/"
output_dir <- "/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/generated-figures/supplementary/"

#-------------------------#
# 1. Restrict outcomes and models to plot #
#-------------------------#
# Load all estimates
estimates <- read.csv(paste0(results_dir,"/master_hr_file.csv"))
unique(estimates$event)
unique(estimates$outcome_name)
unique(estimates$subgroup)


outcomes_to_plot <- c("t2dm_extended_follow_up","t2dm_follow_extended_follow_up")

# Remove days_pre
estimates <- estimates %>% filter(!(term == "days_pre"))

# Restrict to outcomes needed for Table 3
estimates <- estimates %>% filter(estimates$event %in% c(outcomes_to_plot))

# Remove days_pre
estimates <- estimates %>% filter(!(term == "days_pre"))

# restrict models
estimates <- estimates %>% filter(subgroup %in% c("main", "sub_covid_hospitalised","sub_covid_nonhospitalised") & model=="mdl_max_adj") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,model,median_follow_up,outcome_name)


#Renaming/creating label variables
estimates$analysis[estimates$event == "t2dm_extended_follow_up"] <- "Type 2 diabetes - main analysis (Pre-vaccination cohort)"
estimates$analysis[estimates$event == "t2dm_follow_extended_follow_up"] <- "Type 2 diabetes - still treated after 4 months (Pre-vaccination cohort)"

estimates$subgroup[estimates$subgroup == "main"] <- "All COVID-19"
estimates$subgroup[estimates$subgroup == "sub_covid_hospitalised"] <- "Hospitalised COVID-19"
estimates$subgroup[estimates$subgroup == "sub_covid_nonhospitalised"] <- "Non-hospitalised COVID-19"


#------------------------------------------#
# 4. Specify groups and their line colours #
#------------------------------------------#
# Specify colours
estimates$colour <- ""
estimates$colour <- ifelse(estimates$analysis=="Type 2 diabetes - main analysis (Pre-vaccination cohort)","#d2ac47",estimates$colour)
estimates$colour <- ifelse(estimates$analysis=="Type 2 diabetes - still treated after 4 months (Pre-vaccination cohort)","#009999",estimates$colour) # Black

# Factor variables for ordering
estimates$analysis <- factor(estimates$analysis, levels=c("Type 2 diabetes - main analysis (Pre-vaccination cohort)","Type 2 diabetes - still treated after 4 months (Pre-vaccination cohort)")) 
estimates$colour <- factor(estimates$colour, levels=c("#d2ac47","#009999"))
#estimates$linetype <- factor(estimates$linetype,levels = c("solid","dashed"))
estimates$subgroup <- factor(estimates$subgroup,levels = c("All COVID-19", "Hospitalised COVID-19","Non-hospitalised COVID-19"))

df <- estimates 


# MAIN --------------------------------------------------------------------

# set desired dodge width
pd <- position_dodge2(width = 0.5)

df_main <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="All COVID-19")

main <- ggplot2::ggplot(data=df_main,
                        mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = analysis, shape= analysis, fill= analysis))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(),size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.5,0.5,conf_low), 
                                                            ymax = ifelse(conf_high>32,32,conf_high),  
                                                            width = 0), 
                         position = pd)+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.5,32), breaks = c(0.5,1,2,4,8,16,32), trans = "log") +
  #ggplot2::scale_y_continuous(lim = c(0.25,64), breaks = c(0.25,0.5,1,2,4,8,16,32,64), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$analysis))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$analysis)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$analysis)) +
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
 # theme(legend.text = element_blank())



# HOSPITALISED ------------------------------------------------------------

df_hosp <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="Hospitalised COVID-19")

hosp <- ggplot2::ggplot(data=df_hosp,
                        mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = analysis, shape= analysis, fill= analysis))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(),size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.5,0.5,conf_low), 
                                                            ymax = ifelse(conf_high>32,32,conf_high),  
                                                            width = 0), 
                         position = pd)+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.5,32), breaks = c(0.5,1,2,4,8,16,32), trans = "log") +
  #ggplot2::scale_y_continuous(lim = c(0.25,64), breaks = c(0.25,0.5,1,2,4,8,16,32,64), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$analysis))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$analysis)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$analysis)) +
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

#ggplot2::ggsave(paste0(output_dir,"Supp_Fig6_covid_pheno_HOSP_all_cohorts_TEST.png"), height = 297, width = 230, unit = "mm", dpi = 600, scale = 1)


# NON HOSPITALISED --------------------------------------------------------


df_nonhosp <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="Non-hospitalised COVID-19")

non_hosp <- ggplot2::ggplot(data=df_nonhosp,
                            mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = analysis, shape= analysis, fill= analysis))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(), size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.5,0.5,conf_low), 
                                                            ymax = ifelse(conf_high>32,32,conf_high),  
                                                            width = 0), 
                         position = pd)+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.5,32), breaks = c(0.5,1,2,4,8,16,32), trans = "log") +
  #ggplot2::scale_y_continuous(lim = c(0.25,64), breaks = c(0.25,0.5,1,2,4,8,16,32,64), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$analysis))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$analysis)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$analysis)) +
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
  theme(text = element_text(size = 12))# +
#  theme(legend.text = element_blank())

# ggplot2::ggsave(paste0(output_dir,"Figure2_covid_pheno_NON_HOSP_all_cohorts_TEST.png"), height = 297, width = 230, unit = "mm", dpi = 600, scale = 1)

# PLOT WITHOUT TABLE ------------------------------------------------------

png(paste0(output_dir,"supp-FIG-5-persistent-t2dm.png"),
    units = "mm", width=330, height=195, res = 1000)
ggpubr::ggarrange(main, hosp, non_hosp, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  labels = c("A: All COVID-19", "B: Hospitalised-COVID-19", "C: Non-Hospitalised-COVID-19"),
                  hjust = -0.1,
                  font.label = list(size = 12)) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 
dev.off() 






#ggplot2::ggplot(data=estimates,
#                mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = analysis, shape= analysis, fill= analysis))+
#  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
#  ggplot2::geom_point() +
#  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
#  ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
#                                                ymax = ifelse(conf_high>32,32,conf_high),  
#                                                width = 0)
#                         #,position = ggplot2::position_dodge(width = 1)
#  )+   
#  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
#  ggplot2::geom_line() +
#  ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.5,1,2,4,8,16,32), trans = "log") +
#  #ggplot2::scale_y_continuous(lim = c(0.5,ylim), breaks = ybreaks, trans = "log") +
#  ggplot2::scale_x_continuous(lim = c(0,ceiling(max(df$median_follow_up, na.rm = T) / 8) * 8), breaks = seq(0,ceiling(max(df$median_follow_up, na.rm = T) / 8) * 8,8)) +
#  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$analysis))+ 
#  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$analysis)) +
#  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$analysis)) +
#  ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
#  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, byrow = TRUE)) +
#  ggplot2::theme_minimal() +
#  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
#                 panel.grid.minor = ggplot2::element_blank(),
#                 panel.spacing.x = ggplot2::unit(0.5, "lines"),
#                 panel.spacing.y = ggplot2::unit(0, "lines"),
#                 legend.key = ggplot2::element_rect(colour = NA, fill = NA),
#                 legend.title = ggplot2::element_blank(),
#                 legend.position="bottom",
#                 #legend.justification = "left",
#                 plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
#                 text=element_text(size=13),
#                 strip.text = element_text(face = "bold")) +
#  ggplot2::facet_wrap(subgroup~., ncol = 3)

#ggsave("C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/generated-figures/supplementary/supp-table-6-persistent-t2dm.png",height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)



