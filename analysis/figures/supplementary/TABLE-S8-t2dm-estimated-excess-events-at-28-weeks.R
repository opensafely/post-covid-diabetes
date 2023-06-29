library(dplyr)
library(readr)
library(stringr)
library(tidyr)

#aer_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/AER/compiled_results/"
#results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"

#Read in AER
#df <- readr::read_csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/model/AER/AER_compiled_results_extended.csv")
df <- readr::read_csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/model/AER/compiled_results_for_plotting/15_06_2023/AER_compiled_results.csv")

#Select max day of follow up (196 for vax/unvax, 534 for pre-vax)
# df <- df %>%
#   group_by(event, subgroup, time_points, cohort) %>%
#   filter(days == max(days) & subgroup != "aer_overall" & !is.na(AER_main) ) %>%
#   select(event, subgroup, cohort, subgroup, AER_main, time_points, days)%>% ungroup()

df <- df %>% filter(days == 196 & subgroup == "aer_overall" & time_points == "reduced")

# The column cumulative_difference_absolute_excess_risk is weighted by th prevax population size
# See lines 144 onwards in analysis/model/Absolute_excess_risk_function.R

df$time_points <- NULL

# Get excess events at 28 weeks per 100,000 COVID-19 diagnosis
df$cumulative_difference_absolute_excess_risk <- df$cumulative_difference_absolute_excess_risk * 100000
df <- df %>% select(cohort,cumulative_difference_absolute_excess_risk)
colnames(df) <- c("cohort","Excess events after 100,000 COVID-19 diagnosis")

write.csv(df,"C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/results/generated-tables/Estimated_excess_events_at_28_weeks.csv", row.names = F )


