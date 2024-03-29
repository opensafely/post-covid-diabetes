#libraries
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)

dir <- ("~/Library/CloudStorage/OneDrive-UniversityofBristol/ehr_postdoc/projects/post-covid-diabetes")
setwd(dir)

results_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/release-31-01-2023/")
output_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/generated-figures/")

#-------------------------#
# 2. Get outcomes to plot #
#-------------------------#
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))

outcomes_to_plot <- outcome_name_table$outcome_name
outcomes_to_plot <- c("t2dm_extended_follow_up", "t2dm_follow_extended_follow_up")

#---------------------------------------------#
# 3. Load all estimates #
#---------------------------------------------#

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

# keep only Stata results for all hosp analyses

# estimates <- estimates[(estimates$subgroup == "covid_pheno_hospitalised"),]

# KEep only stata results for full figure 

# estimates <- estimates[(estimates$source == "R"),] 

# Get estimates for main analyses and list of outcomes from active analyses
main_estimates <- estimates %>% filter(event %in% outcomes_to_plot 
                                       & term %in% term[grepl("^days",term)]
                                       & model == "mdl_max_adj"
                                       & cohort == "prevax"
                                       & time_points == "reduced") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up, source)


main_estimates <- main_estimates %>% dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))

main_estimates$analysis <- NA
main_estimates$analysis[main_estimates$event == "t2dm_extended_follow_up"] <- "Type 2 diabetes - main analysis (Pre-vaccination cohort)"
main_estimates$analysis[main_estimates$event == "t2dm_follow_extended_follow_up"] <- "Type 2 diabetes - still treated after 4 months (Pre-vaccination cohort)"

# We want to plot the figures using the same time-points across all cohorts so that they can be compared
# If any cohort uses reduced time points then all cohorts will be plotted with reduced time points
main_estimates <- main_estimates %>%
  group_by(event,subgroup,cohort) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_points == "normal") ~ "normal",
    TRUE ~ "reduced"))

main_estimates <- main_estimates %>%
  group_by(event,subgroup) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_period_to_plot == "reduced") ~ "reduced",
    TRUE ~ "normal"))

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
main_estimates$colour <- ifelse(main_estimates$analysis=="Type 2 diabetes - main analysis (Pre-vaccination cohort)","#d2ac47",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$analysis=="Type 2 diabetes - still treated after 4 months (Pre-vaccination cohort)","#009999",main_estimates$colour) # Black

#Specify lines
main_estimates$linetype <- ""
main_estimates$linetype <- ifelse(main_estimates$subgroup=="covid_pheno_hospitalised","solid",main_estimates$linetype)
main_estimates$linetype <- ifelse(main_estimates$subgroup=="covid_pheno_non_hospitalised","dashed",main_estimates$linetype)

# Factor variables for ordering
main_estimates$analysis <- factor(main_estimates$analysis, levels=c("Type 2 diabetes - main analysis (Pre-vaccination cohort)","Type 2 diabetes - still treated after 4 months (Pre-vaccination cohort)")) 
main_estimates$colour <- factor(main_estimates$colour, levels=c("#d2ac47","#009999"))
main_estimates$linetype <- factor(main_estimates$linetype,levels = c("solid","dashed"))
main_estimates$subgroup <- factor(main_estimates$subgroup,levels = c("main", "covid_pheno_hospitalised","covid_pheno_non_hospitalised"))

# Rename adjustment groups
# levels(main_estimates$cohort) <- list("Pre-Vaccination (2020-01-01 - 2021-06-18)"="prevax", "Vaccinated (2021-06-01 - 2021-12-14)"="vax","Unvaccinated (2021-06-01 - 2021-12-14)"="unvax")
# 
# main_estimates <- main_estimates %>%
#   dplyr::rename("outcome" = "analysis")
# 
# # Order outcomes for plotting
# # Use the nice names from active_analyses table i.e. outcome_name_table
# main_estimates <- main_estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))
# main_estimates$outcome <- str_to_title(main_estimates$outcome)
# main_estimates$outcome <- factor(main_estimates$outcome, levels=c("Type 2 Diabetes"))

df <- main_estimates %>% dplyr::filter(time_points == "reduced")

# REMOVE LAST TIME POINTS FOR FOUR MONTH FOLLOW UP AS NO EVENTS
filter_out <- function(data, ...){
  dots <- lapply( lazyeval::lazy_dots(...), function(dot){
    dot$expr <- call( "!", dot$expr )
    dot
  })
  class(dots) <- "lazy_dots"
  filter_( data, .dots = dots )
}
df <- main_estimates %>% filter_out(term == "days365_714" & event == "t2dm_follow_extended_follow_up")

# MAIN --------------------------------------------------------------------


df_main <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="main")

main <- ggplot2::ggplot(data=df_main,
                        mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = analysis, shape= analysis, fill= analysis))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(),size = 2) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                            ymax = ifelse(conf_high>64,64,conf_high),  
                                                            width = 0), 
                         position = ggplot2::position_dodge(width = 0.5))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line() +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.5,32), breaks = c(0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$analysis))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$analysis)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$analysis)) +
  #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
  ggplot2::labs(x = "\n ", y = "Hazard ratio and 95% confidence interval") +
  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, nrow = 2, byrow = TRUE)) +
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
  theme(text = element_text(size = 12)) +
  theme(legend.text = element_blank())


# HOSPITALISED ------------------------------------------------------------


df_hosp <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="covid_pheno_hospitalised")

hosp <- ggplot2::ggplot(data=df_hosp,
                        mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = analysis, shape= analysis, fill= analysis))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(),size = 2) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                            ymax = ifelse(conf_high>64,64,conf_high),  
                                                            width = 0), 
                         position = ggplot2::position_dodge(width = 0.5))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line() +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.5,32), breaks = c(0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$analysis))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$analysis)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$analysis)) +
  #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
  ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = NULL) +
  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, nrow = 2, byrow = TRUE)) +
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
  theme(text = element_text(size = 12)) +
  theme(legend.text = element_blank())

# ggplot2::ggsave(paste0(output_dir,"Figure2_covid_pheno_HOSP_all_analysiss_TEST.png"), height = 297, width = 230, unit = "mm", dpi = 600, scale = 1)


# NON HOSPITALISED --------------------------------------------------------


df_nonhosp <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="covid_pheno_non_hospitalised")

non_hosp <- ggplot2::ggplot(data=df_nonhosp,
                            mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = analysis, shape= analysis, fill= analysis))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(), size = 2) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                            ymax = ifelse(conf_high>64,64,conf_high),  
                                                            width = 0), 
                         position = ggplot2::position_dodge(width = 0.5))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line() +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.5,32), breaks = c(0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$analysis))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$analysis)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$analysis)) +
  #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
  ggplot2::labs(x = "\n ", y = NULL) +
  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, nrow = 2, byrow = TRUE)) +
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
  theme(text = element_text(size = 12)) 

# ggplot2::ggsave(paste0(output_dir,"Figure2_covid_pheno_NON_HOSP_all_cohorts_TEST.png"), height = 297, width = 230, unit = "mm", dpi = 600, scale = 1)

# PLOT WITHOUT TABLE ------------------------------------------------------

png(paste0(output_dir,"Figure_4_t2dm_4_month_follow_3panel.png"),
    units = "mm", width=330, height=195, res = 1000)
ggpubr::ggarrange(main, hosp, non_hosp, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                  labels = c("A: All COVID-19", "B: Hospitalised-COVID-19", "C: Non-Hospitalised-COVID-19"),
                  hjust = -0.1,
                  font.label = list(size = 12)) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
dev.off()


# ADD EVENT COUNTS TO PLOT TABLE  -------------------------------------------------------

table2 <- read.csv("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/generated-figures/4-month-wide-follow-table-for-figure.csv",
                   check.names = FALSE)

# table2 <- table2 %>%
#   dplyr::filter(Outcome == "Type 2 Diabetes") %>%
#   dplyr::mutate(`All COVID-19` = `After hospitalised COVID-19` + `After non-hospitalised COVID-19`) %>%
#   dplyr::rename(`Total type 2 diabetes events` = Total,
#                 `Events after COVID-19` = `All COVID-19`) %>%
#   dplyr::select(-c(`No COVID-19`)) %>%
#   mutate(`Number of people` = ifelse(Cohort == "Pre-vaccination (1 Jan 2020 to 14 Dec 2021)", 15211471,
#                                      ifelse(Cohort == "Vaccinated (1 Jun 2021 to 14 Dec 2021)", 11822640,
#                                             ifelse(Cohort == "Unvaccinated (1 Jun 2021 to 14 Dec 2021)", 2851183, NA)))) %>%
#   relocate(`Total type 2 diabetes events`, .after = `Cohort`) %>%
#   relocate(`Number of people`, .after = `Cohort`) %>%
#   relocate(`Events after COVID-19`, .after = `Total type 2 diabetes events`)
# table2[,3:6] <- format(table2[,3:6], big.mark = ",", scientific = FALSE)
# 
# table2$Outcome <- NULL
# table2$Total <- NULL

# COLOURED ROWS

# table.p <- ggtexttable(table2, rows = NULL,
#                        theme = ttheme(tbody.style = tbody_style(color = "white", fill = c("#d2ac47","#58764c","#0018a8")))) +
#   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 

# NO COLOURS

table.p <- ggtexttable(table2, rows = NULL,
                       theme = ttheme(
                         tbody.style = tbody_style(hjust=0, x=0.01, fill = "white", size = 8),
                         colnames.style = colnames_style(hjust=0, x=0.01, fill = "white", size = 8)))

# table.p <- table.p %>% tab_add_vline(at.column = 2, column.side = "left", linetype = 2) %>%
#   tab_add_vline(at.column = 5, column.side = "left", linetype = 2) %>%
#   tab_add_vline(at.column = 8, column.side = "left", linetype = 2)

# table.p <- table_cell_font(table.p, row = 2, column = 1, 
#                        face = "bold.italic", size = 9.8)
# table.p <- table_cell_font(table.p, row = 6, column = 1, 
#                            face = "bold.italic", size = 9.8)
# table.p <- table_cell_font(table.p, row = 10, column = 1, 
#                            face = "bold.italic", size = 9.8)
# tab_add_footnote(text = "Pre-vaccinated (1 Jan 2020 to 18 June 2021), Vaccinated (1 Jun 2021 to 14 Dec 2021), Unvaccinated (1 Jun 2021 to 14 Dec 2021)", size = 7, face = "italic", hjust = 1.25)
# levels(table2_merged$Cohort) <- list("Pre-vaccinated (2020-01-01 - 2021-06-18)"="Pre-Vaccination", "Vaccinated (2021-06-01 - 2021-12-14)"="Vaccinated","Unvaccinated (2021-06-01 - 2021-12-14)"="Unvaccinated")

# PLOTTING ----------------------------------------------------------------

# MAIN PLOT 

p1 <- ggpubr::ggarrange(main, hosp, non_hosp, ncol=3, nrow=1, common.legend = FALSE, legend = "none",
                        labels = c("A: All COVID-19", "B: Hospitalised COVID-19", "C: Non-Hospitalised COVID-19"),
                        hjust = -0.1,
                        font.label = list(size = 12)) 

# EXTRACT LEGEND

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<- g_legend(non_hosp) 

# table.p <- table.p + theme(plot.margin = margin(2.5,0,-0.1,0, "cm"))

# mylegend <- mylegend + theme(plot.margin = margin(0,0,5,0, "cm"))

# ADD BLANK TO GET SPACING CORRECT

blank <- grid.rect(gp=gpar(col="white"))
p2 <- ggarrange(mylegend,table.p, ncol = 1, nrow = 2,
                heights = c(0.8, 1))

# SAVE PLOT WITH TABLE

png(paste0(output_dir,"Figure_4_t2dm_4_month_follow_3panel-with-table.png"),
    units = "mm", width=330, height=200, res = 1000)
ggpubr::ggarrange(p1, 
                  p2,
                  nrow = 2,
                  heights = c(1, 0.4)) 
# annotation_custom(ggplotGrob(table.p),
#                   xmin = 5.5, ymin = 20,
#                   xmax = 8)
dev.off() 





