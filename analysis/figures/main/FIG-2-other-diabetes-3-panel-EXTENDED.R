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

results_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/model-release-16-12-22/")
output_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/generated-figures/")

#-------------------------#
# 2. Get outcomes to plot #
#-------------------------#
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))

outcomes_to_plot <- outcome_name_table$outcome_name
outcomes_to_plot <- c("t1dm_extended_follow_up", "gestationaldm_extended_follow_up", "otherdm_extended_follow_up")

#---------------------------------------------#
# 3. Load all estimates #
#---------------------------------------------#

hr_files=list.files(path = results_dir, pattern = "extended")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(results_dir,"/", hr_files)
hr_file_paths <- pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
estimates <- rbindlist(hr_file_paths, fill=TRUE)

#Calculate median follow-up for plotting
estimates$median_follow_up <- as.numeric(estimates$median_follow_up)
estimates$add_to_median <- sub("days","",estimates$term)
estimates$add_to_median <- as.numeric(sub("\\_.*","",estimates$add_to_median))

estimates$median_follow_up <- ((estimates$median_follow_up + estimates$add_to_median)-1)/7
estimates$add_to_median <- NULL

# GET ESTIMATES FROM VAX / UNVAX AND RBIND

estimates_other <- read.csv(paste0(results_dir,"hr_output_formatted.csv"))

outcomes_other <- c("t1dm", "gestationaldm", "otherdm")

estimates_other <- estimates_other %>% filter(subgroup %in% c("main") 
                                              & event %in% outcomes_other 
                                              & term %in% term[grepl("^days",term)]
                                              & model == "mdl_max_adj"
                                              & time_points == "reduced"
                                              & (cohort == "vax" | cohort == "unvax")) %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up)

# Get estimates for main analyses and list of outcomes from active analyses

main_estimates <- estimates %>% filter(subgroup %in% c("main") 
                                       & event %in% outcomes_to_plot 
                                       & term %in% term[grepl("^days",term)]
                                       & model == "mdl_max_adj"
                                       & time_points == "reduced") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up)

main_estimates$event <- gsub('_extended_follow_up', '', main_estimates$event)

main_estimates <- rbind(main_estimates, estimates_other)

main_estimates <- main_estimates %>% dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))

# remove duplicate rows
main_estimates <- main_estimates[!duplicated(main_estimates), ]

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

# Factor variables for ordering
main_estimates$cohort <- factor(main_estimates$cohort, levels=c("prevax","vax","unvax")) 
main_estimates$colour <- factor(main_estimates$colour, levels=c("#d2ac47","#58764c","#0018a8"))
main_estimates$event <- factor(main_estimates$event,levels = c("t1dm", "gestationaldm","otherdm"))

# Rename adjustment groups
levels(main_estimates$cohort) <- list("Pre-Vaccination (1 Jan 2020 to 18 Jun 2021)"="prevax", "Vaccinated (1 Jun 2021 to 14 Dec 2021)"="vax","Unvaccinated (1 Jun 2021 to 14 Dec 2021)"="unvax")

# Order outcomes for plotting
# Use the nice names from active_analyses table i.e. outcome_name_table
main_estimates <- main_estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))
main_estimates$outcome <- str_to_title(main_estimates$outcome)
main_estimates$outcome <- factor(main_estimates$outcome, levels=c("Type 2 Diabetes"))

df <- main_estimates %>% dplyr::filter(time_points == "reduced")


# TYPE 1 --------------------------------------------------------------------

# set desired dodge width
pd <- position_dodge2(width = 0.5)

df_t1dm <- df %>%
  dplyr::filter(event=="t1dm")

t1dm <- ggplot2::ggplot(data=df_t1dm,
                        mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(),size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                            ymax = ifelse(conf_high>64,64,conf_high),  
                                                            width = 0))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
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
                 legend.spacing.y = unit(0.01, 'cm'),
                 plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                 plot.margin = margin(1,1,1,1, "cm")) +   
  theme(text = element_text(size = 12)) +
  theme(legend.text = element_blank())



# GESTATIONAL ------------------------------------------------------------


df_gest <- df %>%
  dplyr::filter(event=="gestationaldm")

gest <- ggplot2::ggplot(data=df_gest,
                        mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(),size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                            ymax = ifelse(conf_high>64,64,conf_high),  
                                                            width = 0))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
  #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
  ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "\n ") +
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
  theme(text = element_text(size = 12)) 

# ggplot2::ggsave(paste0(output_dir,"Figure2_covid_pheno_HOSP_all_cohorts_TEST.png"), height = 297, width = 230, unit = "mm", dpi = 600, scale = 1)


# OTHER --------------------------------------------------------


df_other <- df %>%
  # hospitalise 
  dplyr::filter(event=="otherdm")

other <- ggplot2::ggplot(data=df_other,
                         mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(), size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                            ymax = ifelse(conf_high>64,64,conf_high),  
                                                            width = 0))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,67), breaks = seq(0,64,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
  #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
  ggplot2::labs(x = "\n ", y = "\n ") +
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
  theme(text = element_text(size = 12)) 

# ggplot2::ggsave(paste0(output_dir,"Figure2_covid_pheno_NON_HOSP_all_cohorts_TEST.png"), height = 297, width = 230, unit = "mm", dpi = 600, scale = 1)


# COMBINE TO MULTIPANEL ---------------------------------------------------
# PLOT WITHOUT TABLE ------------------------------------------------------

# png(paste0(output_dir,"Figure_2_subtypes_3panel.png"),
#     units = "mm", width=330, height=180, res = 1000)
# ggpubr::ggarrange(t1dm, gest, other, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
#                   labels = c("A: Type 1 Diabetes", "B: Gestational Diabetes", "C: Other or Non-Specified Diabetes"),
#                   hjust = -0.1,
#                   font.label = list(size = 12)) +
#   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
# dev.off() 

# ADD EVENT COUNTS TO PLOT TABLE  -------------------------------------------------------

table2 <- read.csv("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/generated-figures/formatted_table_2.csv",
                   check.names = FALSE)

# temporarily use type 2 diabetes as gestational results until I get table 2 gestational diabetes results out
table2$Outcome <- gsub("Type 2 Diabetes", "Gestational Diabetes", table2$Outcome)

table2 <- table2 %>%
  dplyr::filter(Outcome != "Type 2 Diabetes") %>%
  dplyr::mutate(`All COVID-19` = `After hospitalised COVID-19` + `After non-hospitalised COVID-19`) %>%
  dplyr::rename(`Total events` = Total,
                `Events after COVID-19` = `All COVID-19`) %>%
  dplyr::select(-c(`No COVID-19`)) %>%
  mutate(`Number of people` = ifelse(Cohort == "Pre-vaccination (1 Jan 2020 to 14 Dec 2021)", 15211471,
                                     ifelse(Cohort == "Vaccinated (1 Jun 2021 to 14 Dec 2021)", 11822640,
                                            ifelse(Cohort == "Unvaccinated (1 Jun 2021 to 14 Dec 2021)", 2851183, NA)))) %>%
  relocate(`Total events`, .after = `Cohort`) %>%
  relocate(`Number of people`, .after = `Cohort`) %>%
  relocate(`Events after COVID-19`, .after = `Total events`) %>%
  relocate(Outcome, .after = `Cohort`) %>%
  mutate(Outcome=factor(Outcome)) %>% 
  mutate(Outcome=fct_relevel(Outcome,c("Type 1 Diabetes","Gestational Diabetes","Other Or Non-Specified Diabetes"))) %>%
  arrange(Outcome) %>%
  replace(is.na(.), "-") %>%
  select(-c(`After hospitalised COVID-19`, `After non-hospitalised COVID-19`))

table2[,3:5] <- format(table2[,3:5], big.mark = ",", scientific = FALSE)

# table2$Cohort <- as.factor(table2$Cohort)
# levels(table2$Cohort) <- list("Pre-Vaccination"="Pre-vaccination (1 Jan 2020 to 18 Jun 2021)",
#                               "Vaccinated"="Vaccinated (1 Jun 2021 to 14 Dec 2021)", "Unvaccinated"="Unvaccinated (1 Jun 2021 to 14 Dec 2021)")

## MAKE SEPARATE TABLES

table_cohort <- table2 %>%
  dplyr::filter(Outcome == "Type 1 Diabetes") %>%
  dplyr::select(Cohort, `Number of people`) 

table_t1dm <- table2 %>%
  dplyr::filter(Outcome == "Type 1 Diabetes") %>%
  dplyr::select(`Total events`, `Events after COVID-19`) %>%
  dplyr::rename(`Total Type 1 Diabetes events` = `Total events`)

table_gdm <- table2 %>%
  dplyr::filter(Outcome == "Gestational Diabetes") %>%
  dplyr::select(`Total events`, `Events after COVID-19`) %>%
  dplyr::rename(`Total Gestational Diabetes events` = `Total events`)

table_otherdm <- table2 %>%
  dplyr::filter(Outcome == "Other Or Non-Specified Diabetes") %>%
  dplyr::select(`Total events`, `Events after COVID-19`) %>%
  dplyr::rename(`Total Other Diabetes events` = `Total events`)

table.p_cohort <- ggtexttable(table_cohort, rows = NULL,
                              theme = ttheme(
                                tbody.style = tbody_style(hjust=0, x=0.01, fill = "white", size = 8),
                                colnames.style = colnames_style(hjust=0, x=0.01, fill = "white", size = 7))) 

table.p_t1dm <- ggtexttable(table_t1dm, rows = NULL,
                            theme = ttheme(
                              tbody.style = tbody_style(hjust=0, x=0.01, fill = "white", size = 8),
                              colnames.style = colnames_style(hjust=0, x=0.01, fill = "white", size = 7))) 

table.p_gdm <- ggtexttable(table_gdm, rows = NULL,
                           theme = ttheme(
                             tbody.style = tbody_style(hjust=0, x=0.01, fill = "white", size = 8),
                             colnames.style = colnames_style(hjust=0, x=0.01, fill = "white", size = 7))) 

table.p_otherdm <- ggtexttable(table_otherdm, rows = NULL,
                               theme = ttheme(
                                 tbody.style = tbody_style(hjust=0, x=0.01, fill = "white", size = 8),
                                 colnames.style = colnames_style(hjust=0, x=0.01, fill = "white", size = 7))) 

# tab_add_footnote(text = "Pre-vaccinated (1 Jan 2020 to 18 June 2021), Vaccinated (1 Jun 2021 to 14 Dec 2021), Unvaccinated (1 Jun 2021 to 14 Dec 2021)", size = 7, face = "italic", hjust = 1.25)
# levels(table2_merged$Cohort) <- list("Pre-vaccinated (2020-01-01 - 2021-06-18)"="Pre-Vaccination", "Vaccinated (2021-06-01 - 2021-12-14)"="Vaccinated","Unvaccinated (2021-06-01 - 2021-12-14)"="Unvaccinated")

# PLOTTING ----------------------------------------------------------------

# T1DM 

t1dm_plot <- ggpubr::ggarrange(t1dm, table.p_t1dm, ncol=1, nrow=2, 
                               heights = c(1, 0.2),
                               legend = "none") 

gdm_plot <- ggpubr::ggarrange(gest, table.p_gdm, ncol=1, nrow=2, 
                              heights = c(1, 0.2),
                              legend = "none") 

otherdm_plot <- ggpubr::ggarrange(other, table.p_otherdm, ncol=1, nrow=2, 
                                  heights = c(1, 0.2),
                                  legend = "none") 

# ADD BLANK TO GET SPACING CORRECT

blank <- grid.rect(gp=gpar(col="white"))
cohort_tab <- ggarrange(blank, table.p_cohort, ncol = 1, nrow = 2, heights = c(1,0.2))

# MAIN PLOT 

p1 <- ggpubr::ggarrange(t1dm, gest, other, ncol=3, nrow=1, common.legend = FALSE, legend = "none",
                        labels = c("A: Type 1 Diabetes", "B: Gestational Diabetes", "C: Other or Non-Specified Diabetes"),
                        hjust = -0.1,
                        font.label = list(size = 12)) 

p1a <- ggpubr::ggarrange(cohort_tab, p1,
                         ncol = 2,
                         widths = c(0.2,1))

# EXTRACT LEGEND

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<- g_legend(t1dm) 


# ARRANGE TABLES

tablesplot <- ggpubr::ggarrange(table.p_cohort,
                                table.p_t1dm,
                                table.p_gdm,
                                table.p_otherdm,
                                ncol = 4)

tablesplot <- tablesplot + theme(plot.margin = margin(0,0,0.6,0, "cm"))

# ADD BLANK TO GET SPACING CORRECT

blank <- grid.rect(gp=gpar(col="white"))
p2 <- ggarrange(mylegend, tablesplot, ncol = 2, widths = c(0.07,1))


# SAVE PLOT WITH TABLE

png(paste0(output_dir,"Figure_2_other_3panel_with_table_EXTENDED.png"),
    units = "mm", width=315, height=150, res = 1000)
ggpubr::ggarrange(p1, 
                  p2,
                  nrow = 2,
                  heights = c(1, 0.15)) 
# annotation_custom(ggplotGrob(table.p),
#                   xmin = 5.5, ymin = 20,
#                   xmax = 8)
dev.off() 

