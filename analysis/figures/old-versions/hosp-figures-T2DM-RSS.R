#------------------#
# 1. Load argument #
#------------------#
# NOTE: No action written in project.yaml for running figures so no need for arguments
#args = commandArgs(trailingOnly=TRUE)

#-------------------------#
# 2. Get outcomes to plot #
#-------------------------#
hosp_fig <- function(cohort) {
  
  active_analyses <- read_rds("lib/active_analyses.rds")
  active_analyses_table <- subset(active_analyses, active_analyses$active =="TRUE" & !grepl("type 2 diabetes -", active_analyses$outcome)) 
  outcome_name_table <- active_analyses_table %>% 
    select(outcome, outcome_variable,outcome_group) %>% 
    mutate(outcome_name=active_analyses_table$outcome_variable %>% str_replace("out_date_", ""))
  
  outcome_to_plot <- outcome_name_table$outcome_name
  
  #---------------------------------------------#
  # 3. Load and combine all estimates in 1 file #
  #---------------------------------------------#
  hr_files=list.files(path = results_dir, pattern = paste0("_",cohort))
  hr_files=hr_files[endsWith(hr_files,".csv")]
  hr_files=paste0(results_dir,"/", hr_files)
  hr_file_paths <- pmap(list(hr_files),
                        function(fpath){
                          df <- fread(fpath)
                          return(df)
                        })
  estimates <- rbindlist(hr_file_paths, fill=TRUE)
  
  # Get estimates for main analyses and list of outcomes from active analyses
  main_estimates <- subset(estimates, (subgroup == "covid_pheno_hospitalised" | subgroup == "covid_pheno_non_hospitalised") & event %in% outcome_to_plot & term %in% term[grepl("^days",term)])
  rm(estimates,hr_file_paths)
  
  #--------------------------Format the results-----------------------------------
  main_estimates <- main_estimates %>% mutate_at(c("estimate","conf.low","conf.high"), as.numeric)
  #--------------------------------------#
  # 4. Specify time in weeks (mid-point) #
  #--------------------------------------#
  if(cohort == "prevax"){
    
    term_to_time <- data.frame(term = c("days0_7","days7_14", "days14_28", "days28_56", "days56_84", "days84_197", "days197_365", "days365_535",
                                        "days0_28", "days28_197", "days197_535"),
                               time = c(0.5,1.5,3,6,10,20,40,65,
                                        2,16,65))
    
  } else if (cohort == "prevax_compare" | cohort == "vax" | cohort == "unvax"){
    
    term_to_time <- data.frame(term = c("days0_7","days7_14", "days14_28", "days28_56", "days56_84", "days84_197", 
                                        "days0_28","days28_197"),
                               time = c(0.5,1.5,3,6,10,20,
                                        2,16))
    
  }
  
  main_estimates <- merge(main_estimates, term_to_time, by = c("term"), all.x = TRUE)
  
  #------------------------------------------#
  # 4. Specify groups and their line colours #
  #------------------------------------------#
  # Specify colours
  main_estimates$colour <- ""
  main_estimates$colour <- ifelse(main_estimates$subgroup=="covid_pheno_hospitalised","#bababa",main_estimates$colour) # Grey
  main_estimates$colour <- ifelse(main_estimates$model=="covid_pheno_non_hospitalised","#000000",main_estimates$colour) # Black
  
  # subset to just the main model if looking OK. If not, subset as appropriate.
  main_estimates <- main_estimates %>%
    dplyr::filter(model == "mdl_max_adj")
  
  # Factor variables for ordering
  main_estimates$subgroup <- factor(main_estimates$subgroup, levels=c("covid_pheno_hospitalised", "covid_pheno_non_hospitalised")) 
  main_estimates$colour <- factor(main_estimates$colour, levels=c("#bababa", "#000000"))
  print("Uses  mdl_max_adj by default. If you wish to plot different models, change line 76")
  # Rename adjustment groups
  levels(main_estimates$subgroup) <- list("Hospitalised COVID-19"="covid_pheno_hospitalised", "Non-hospitalised COVID-19"="covid_pheno_non_hospitalised")
  
  #-------------------------#
  # 5. Specify outcome name #
  #-------------------------#
  # Use the nice names from active_analyses table i.e. outcome_name_table
  main_estimates <- main_estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name, outcome_group), by = c("event"="outcome_name"))
  
  main_estimates$cohort <- cohort
  #---------#
  # 6. Plot #
  #---------#
  
  c=cohort
  
  main_estimates <- subset(main_estimates,event == "t2dm")
  
  main_estimates_reduced <- subset(main_estimates, time_points == "reduced")
  
  for(c in cohort){
      df=main_estimates_reduced %>% filter(cohort == c)
      
      p <- ggplot2::ggplot(data=df,
                      mapping = ggplot2::aes(x=time, y = estimate, color = subgroup, shape=subgroup, fill=subgroup))+
        ggplot2::geom_point(position = ggplot2::position_dodge(width = 0)) +
        geom_point(size=1.2) +
        ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
        ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                                      ymax = ifelse(conf.high>64,64,conf.high),  
                                                      width = 0), 
                               position = ggplot2::position_dodge(width = 0))+   
        ggplot2::geom_line(position = ggplot2::position_dodge(width = 0)) +    
        #    ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
        ggplot2::scale_y_continuous(lim = c(0.25,48), breaks = c(0.5,1,2,4,8,16,32,48), trans = "log") +
        # ggplot2::scale_x_continuous(lim = c(0,28), breaks = seq(0,28,4)) +
        ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$subgroup))+ 
        ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
        ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$subgroup)) +
        #   ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") 
        ggplot2::labs(x = ifelse(df$event=="t2dm","\nWeeks since COVID-19 diagnosis\n", # only plot axis labels at the bottom of the plot where gestational dm is
                                 " "),
                      y = ifelse(df$cohort=="prevax","Hazard ratio and 95% confidence interval",
                                 " ")) +
        ggplot2::guides(fill=ggplot2::guide_legend(ncol = 2, byrow = TRUE)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.spacing.x = ggplot2::unit(0.5, "lines"),
                       panel.spacing.y = ggplot2::unit(0, "lines"),
                       legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                       legend.title = ggplot2::element_blank(),
                       plot.background = ggplot2::element_rect(fill = "white", colour = "white")) +
        ggtitle(ifelse(c == "prevax",
                       paste0(str_to_title(df$outcome), " ", str_to_title(cohort),"\nJanuary 2020 - June 2021"),
                       ifelse(c == "prevax_compare",
                              paste0(str_to_title(df$outcome), " Prevax ","\n2020 Cohort"),
                              paste0(str_to_title(df$outcome), " ", str_to_title(cohort),"\nJune 2021 - December 2021")))) +
        theme(plot.title = element_text(size = 8, face = "bold", hjust = 0.5)) +
        theme(text=element_text(size=10)) +
        theme(legend.text = element_text(face="bold", size = 12),
              legend.position = "bottom") +
        if(cohort == "prevax"){
          ggplot2::scale_x_continuous(lim = c(0,88), breaks = seq(0,88,8)) 
        } else if (cohort == "vax" | cohort == "unvax"){
          ggplot2::scale_x_continuous(lim = c(0,28), breaks = seq(0,28,4)) 
        }
      # ggplot2::facet_wrap(outcome~., ncol = 2)
      
      # ggplot2::ggsave(paste0(output_dir,"Figure1","_",c,"_","hospitalised_pheno_reduced.png"), height = 297, width = 210, unit = "mm", dpi = 600, scale = 1)
  }
  return(p)
}