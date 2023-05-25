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

# Load all estimates
estimates <- read.csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/model/hr_output_formatted.csv")
stata_estimates <- read.csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/model/stata_output_formatted.csv")

# keep only Stata results for all hosp analyses

estimates <- estimates %>% filter(cohort == "prevax" 
                                  & event == "t2dm_extended_follow_up"
                                  & time_points == "reduced"
                                  & subgroup %in% c("main","covid_pheno_hospitalised","covid_pheno_non_hospitalised")
                                  & source == "R"
                                  & model == "mdl_max_adj")

stata_estimates <- stata_estimates %>% filter(cohort == "prevax" 
                                              & event == "t2dm_follow_extended_follow_up"
                                              & time_points == "reduced"
                                              & subgroup %in% c("main","covid_pheno_hospitalised","covid_pheno_non_hospitalised")
                                              & model == "mdl_max_adj"
                                              & term %in% term[grepl("^days",term)])

stata_estimates <- stata_estimates %>% select(intersect(colnames(estimates),colnames(stata_estimates)))
                                        
estimates <- plyr::rbind.fill(estimates,stata_estimates)

# Get estimates for main analyses and list of outcomes from active analyses
estimates <- estimates %>% select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up, source)


estimates <- estimates %>% dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))

estimates$analysis[estimates$event == "t2dm_extended_follow_up"] <- "Type 2 diabetes - main analysis (Pre-vaccination cohort)"
estimates$analysis[estimates$event == "t2dm_follow_extended_follow_up"] <- "Type 2 diabetes - still treated after 4 months (Pre-vaccination cohort)"

estimates$subgroup[estimates$subgroup == "main"] <- "All COVID-19"
estimates$subgroup[estimates$subgroup == "covid_pheno_hospitalised"] <- "Hospitalised COVID-19"
estimates$subgroup[estimates$subgroup == "covid_pheno_non_hospitalised"] <- "Non-hospitalised COVID-19"

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
estimates$linetype <- factor(estimates$linetype,levels = c("solid","dashed"))
estimates$subgroup <- factor(estimates$subgroup,levels = c("All COVID-19", "Hospitalised COVID-19","Non-hospitalised COVID-19"))


ggplot2::ggplot(data=estimates,
                mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = analysis, shape= analysis, fill= analysis))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                ymax = ifelse(conf_high>32,32,conf_high),  
                                                width = 0)
                         #,position = ggplot2::position_dodge(width = 1)
  )+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.5,1,2,4,8,16,32), trans = "log") +
  #ggplot2::scale_y_continuous(lim = c(0.5,ylim), breaks = ybreaks, trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,ceiling(max(df$median_follow_up, na.rm = T) / 8) * 8), breaks = seq(0,ceiling(max(df$median_follow_up, na.rm = T) / 8) * 8,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$analysis))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$analysis)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$analysis)) +
  ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, byrow = TRUE)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.spacing.x = ggplot2::unit(0.5, "lines"),
                 panel.spacing.y = ggplot2::unit(0, "lines"),
                 legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                 legend.title = ggplot2::element_blank(),
                 legend.position="bottom",
                 #legend.justification = "left",
                 plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                 text=element_text(size=13),
                 strip.text = element_text(face = "bold")) +
  ggplot2::facet_wrap(subgroup~., ncol = 3)

ggsave("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/generated-figures/supplementary/supp-table-6-persistent-t2dm.png",height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)



