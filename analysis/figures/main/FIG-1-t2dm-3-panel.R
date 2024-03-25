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
output_dir <- "C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/generated-figures/"

#-------------------------#
# 1. Restrict outcomes and models to plot #
#-------------------------#
# Load all estimates
estimates <- read.csv(paste0(results_dir,"/master_hr_file.csv"))
unique(estimates$event)
unique(estimates$outcome_name)

outcomes_to_plot <- c("t2dm_extended_follow_up","t2dm")

# Remove days_pre
estimates <- estimates %>% filter(!(term == "days_pre"))

# Restrict to outcomes needed for Table 3
estimates <- estimates %>% filter(estimates$event %in% c(outcomes_to_plot))

# restrict models
main_estimates <- estimates %>% filter(subgroup %in% c("main", "sub_covid_hospitalised","sub_covid_nonhospitalised") & model=="mdl_max_adj") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,model,median_follow_up,outcome_name)

#------------------------------------------#
# 2. Specify groups and their line colours #
#------------------------------------------#
# Specify colours
main_estimates$colour <- ""
main_estimates$colour <- ifelse(main_estimates$cohort=="prevax","#d2ac47",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$cohort=="vax","#58764c",main_estimates$colour) # Grey
main_estimates$colour <- ifelse(main_estimates$cohort=="unvax","#0018a8",main_estimates$colour) # Black

#Specify lines
main_estimates$linetype <- ""
main_estimates$linetype <- ifelse(main_estimates$subgroup=="sub_covid_hospitalised","solid",main_estimates$linetype)
main_estimates$linetype <- ifelse(main_estimates$subgroup=="sub_covid_nonhospitalised","dashed",main_estimates$linetype)

# Factor variables for ordering
main_estimates$cohort <- factor(main_estimates$cohort, levels=c("prevax","vax","unvax")) 
main_estimates$colour <- factor(main_estimates$colour, levels=c("#d2ac47","#58764c","#0018a8"))
main_estimates$linetype <- factor(main_estimates$linetype,levels = c("solid","dashed"))
main_estimates$subgroup <- factor(main_estimates$subgroup,levels = c("main", "sub_covid_hospitalised","sub_covid_nonhospitalised"))

# Rename adjustment groups
levels(main_estimates$cohort) <- list("Pre-Vaccination (2020-01-01 - 2021-06-18)"="prevax", "Vaccinated (2021-06-01 - 2021-12-14)"="vax","Unvaccinated (2021-06-01 - 2021-12-14)"="unvax")

# Order outcomes for plotting
# Use the nice names from active_analyses table i.e. outcome_name_table
#main_estimates <- main_estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))
main_estimates$outcome_name <- str_to_title(main_estimates$outcome_name)
main_estimates$outcome_name <- factor(main_estimates$outcome_name, levels=c("Type 2 Diabetes"))

df <- main_estimates 


# MAIN --------------------------------------------------------------------

# set desired dodge width
pd <- position_dodge2(width = 0.5)

df_main <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="main")

main <- ggplot2::ggplot(data=df_main,
                        mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(),size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.50,0.50,conf_low), 
                                                            ymax = ifelse(conf_high>64,64,conf_high),  
                                                            width = 0), 
                         position = pd)+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  #ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.5,64), breaks = c(0.5,1,2,4,8,16,32,64), trans = "log") +
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
  theme(text = element_text(size = 12)) +
  theme(legend.text = element_blank())



# HOSPITALISED ------------------------------------------------------------

df_hosp <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="sub_covid_hospitalised")

hosp <- ggplot2::ggplot(data=df_hosp,
                        mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(),size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.50,0.50,conf_low), 
                                                            ymax = ifelse(conf_high>64,64,conf_high),  
                                                            width = 0))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  #ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.5,64), breaks = c(0.5,1,2,4,8,16,32,64), trans = "log") +
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
  theme(text = element_text(size = 12)) +
  theme(legend.text = element_blank())

# ggplot2::ggsave(paste0(output_dir,"Figure2_covid_pheno_HOSP_all_cohorts_TEST.png"), height = 297, width = 230, unit = "mm", dpi = 600, scale = 1)


# NON HOSPITALISED --------------------------------------------------------


df_nonhosp <- df %>%
  # hospitalise 
  dplyr::filter(subgroup=="sub_covid_nonhospitalised")

non_hosp <- ggplot2::ggplot(data=df_nonhosp,
                            mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point(aes(), size = 2, position = pd) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.50,0.50,conf_low), 
                                                            ymax = ifelse(conf_high>64,64,conf_high),  
                                                            width = 0))+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line(position = pd) +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  #ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.5,64), breaks = c(0.5,1,2,4,8,16,32,64), trans = "log") +
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
  theme(text = element_text(size = 12)) +
  theme(legend.text = element_blank())

# ggplot2::ggsave(paste0(output_dir,"Figure2_covid_pheno_NON_HOSP_all_cohorts_TEST.png"), height = 297, width = 230, unit = "mm", dpi = 600, scale = 1)

# PLOT WITHOUT TABLE ------------------------------------------------------

 png(paste0(output_dir,"Figure_1_t2dm_3panel.png"),
     units = "mm", width=330, height=195, res = 1000)
 ggpubr::ggarrange(main, hosp, non_hosp, ncol=3, nrow=1, common.legend = TRUE, legend="bottom",
                   labels = c("A: All COVID-19", "B: Hospitalised-COVID-19", "C: Non-Hospitalised-COVID-19"),
                   hjust = -0.1,
                   font.label = list(size = 12)) +
   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 
 dev.off() 

# ADD EVENT COUNTS TO PLOT TABLE  -------------------------------------------------------

table2 <- read.csv("/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/generated-tables/formatted_table_2.csv",
                   check.names = FALSE, col.names = c("Outcome","Analysis","Prevax_events","Prevax_IR","Vax_events","Vax_IR","Unvax_events","Unvax_IR"))
 
ls(table2) 
rm()

table2[c('n_prevax', 'fu_prevax')] <- str_split_fixed(table2$Prevax_events, '/', 2)
table2[c('n_vax', 'fu_vax')] <- str_split_fixed(table2$Vax_events, '/', 2)
table2[c('n_unvax', 'fu_unvax')] <- str_split_fixed(table2$Unvax_events, '/', 2)

table2_short <- table2 %>% 
  dplyr::select(Outcome,Analysis,n_prevax,n_vax,n_unvax,fu_prevax,fu_vax,fu_unvax) %>%
  dplyr::filter(Outcome == "Type 2 Diabetes") 

table2_short <- pivot_wider(table2_short, names_from = Analysis, values_from = c(n_prevax,n_vax,n_unvax,fu_prevax,fu_vax,fu_unvax))

table2_totaldm <- table2_short %>% pivot_longer(cols=c("n_prevax_No COVID-19","n_unvax_No COVID-19","n_vax_No COVID-19" ),
                                    names_to='cohort',
                                    values_to='No_Covid_T2DM') %>%
                  mutate(`Cohort` = case_when(cohort == "n_prevax_No COVID-19" ~ "Pre-vaccination (1 Jan 2020 to 14 Dec 2021)", 
                                              cohort == "n_vax_No COVID-19" ~ "Vaccinated (1 Jun 2021 to 14 Dec 2021)", 
                                              cohort == "n_unvax_No COVID-19" ~ "Unvaccinated (1 Jun 2021 to 14 Dec 2021)")) %>% 
                  select(Cohort, 'No_Covid_T2DM')
  
table2_hospdm <- table2_short %>% pivot_longer(cols=c("n_prevax_Hospitalised COVID-19","n_unvax_Hospitalised COVID-19","n_vax_Hospitalised COVID-19" ),
                                                names_to='cohort',
                                                values_to='After hospitalised COVID-19') %>%
                  mutate(`Cohort` = case_when(cohort == "n_prevax_Hospitalised COVID-19" ~ "Pre-vaccination (1 Jan 2020 to 14 Dec 2021)", 
                                              cohort == "n_vax_Hospitalised COVID-19" ~ "Vaccinated (1 Jun 2021 to 14 Dec 2021)", 
                                              cohort == "n_unvax_Hospitalised COVID-19" ~ "Unvaccinated (1 Jun 2021 to 14 Dec 2021)"))%>%
                  select(Cohort, 'After hospitalised COVID-19')
  
table2_nonhospdm <- table2_short %>% pivot_longer(cols=c("n_prevax_Non-hospitalised COVID-19","n_unvax_Non-hospitalised COVID-19","n_vax_Non-hospitalised COVID-19" ),
                                               names_to='cohort',
                                               values_to='After non-hospitalised COVID-19') %>%
                  mutate(`Cohort` = case_when(cohort == "n_prevax_Non-hospitalised COVID-19" ~ "Pre-vaccination (1 Jan 2020 to 14 Dec 2021)", 
                                    cohort == "n_vax_Non-hospitalised COVID-19" ~ "Vaccinated (1 Jun 2021 to 14 Dec 2021)", 
                                    cohort == "n_unvax_Non-hospitalised COVID-19" ~ "Unvaccinated (1 Jun 2021 to 14 Dec 2021)")) %>%
                  select(Cohort, 'After non-hospitalised COVID-19')

cols <- list(table2_totaldm, table2_hospdm, table2_nonhospdm)
table2_short <- cols %>% reduce(full_join, by='Cohort')
rm(table2_totaldm,table2_hospdm,table2_nonhospdm)

table2_short$'After non-hospitalised COVID-19' <- gsub(",","",table2_short$'After non-hospitalised COVID-19') 
table2_short$'After hospitalised COVID-19' <- gsub(",","",table2_short$'After hospitalised COVID-19') 
table2_short$'No_Covid_T2DM' <- gsub(",","",table2_short$'No_Covid_T2DM') 
table2_short$'After non-hospitalised COVID-19' <- as.numeric(as.character(table2_short$'After non-hospitalised COVID-19'))
table2_short$'After hospitalised COVID-19' <- as.numeric(as.character(table2_short$'After hospitalised COVID-19'))
table2_short$'No_Covid_T2DM' <- as.numeric(as.character(table2_short$'No_Covid_T2DM'))

table2_short$"Events after COVID-19" <- table2_short$"After non-hospitalised COVID-19" + table2_short$"After hospitalised COVID-19"
table2_short$"Total type 2 diabetes events" <- table2_short$"Events after COVID-19" + table2_short$"No_Covid_T2DM"

table2_short <- table2_short %>% 
  dplyr::select(Cohort,"Total type 2 diabetes events","Events after COVID-19",'After hospitalised COVID-19','After non-hospitalised COVID-19')  %>% 
  dplyr::arrange(factor(Cohort, levels = c("Pre-vaccination (1 Jan 2020 to 14 Dec 2021)", "Vaccinated (1 Jun 2021 to 14 Dec 2021)", "Unvaccinated (1 Jun 2021 to 14 Dec 2021)")))

table2_short <- table2_short %>% 
                mutate(`Number of people` = case_when(Cohort == "Pre-vaccination (1 Jan 2020 to 14 Dec 2021)" ~ 15211471,
                                                      Cohort == "Unvaccinated (1 Jun 2021 to 14 Dec 2021)" ~ 2851183,
                                                      Cohort == "Vaccinated (1 Jun 2021 to 14 Dec 2021)" ~ 11822640)) 
  
table2_short <- table2_short %>% 
  relocate(`Total type 2 diabetes events`, .after = `Cohort`) %>%
  relocate(`Number of people`, .after = `Cohort`) %>%
  relocate(`Events after COVID-19`, .after = `Total type 2 diabetes events`)


# COLOURED ROWS

# table.p <- ggtexttable(table2, rows = NULL,
#                        theme = ttheme(tbody.style = tbody_style(color = "white", fill = c("#d2ac47","#58764c","#0018a8")))) +
#   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 

# NO COLOURS

table.p <- ggtexttable(table2_short, rows = NULL,
                       theme = ttheme(
                         tbody.style = tbody_style(hjust=0, x=0.01, fill = "white", size = 9.8),
                         colnames.style = colnames_style(hjust=0, x=0.01, fill = "white", size = 9.8))) 
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

table.p <- table.p + theme(plot.margin = margin(0,1,0.7,0, "cm"))

# ADD BLANK TO GET SPACING CORRECT

blank <- grid.rect(gp=gpar(col="white"))
p2 <- ggarrange(blank, mylegend, table.p, ncol = 3, widths = c(0.02,0.05,1))

# SAVE PLOT WITH TABLE

png(paste0(output_dir,"Figure_1_t2dm_3panel_with_table.png"),
    units = "mm", width=330, height=195, res = 1000)
ggpubr::ggarrange(p1, 
                  p2,
                  nrow = 2,
                  heights = c(1, 0.2)) 
# annotation_custom(ggplotGrob(table.p),
#                   xmin = 5.5, ymin = 20,
#                   xmax = 8)
dev.off() 



