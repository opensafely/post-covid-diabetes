# Specify directories -----------------------------------------------------------
results_dir <- "C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results"

# Specify parameters -----------------------------------------------------------
print('Specify parameters')

perpeople <- 100000 # per X people

# Load data --------------------------------------------------------------------
print('Load data')

df <- read.csv(paste0(results_dir,"/AER/lifetables_compiled.csv"))


# Filter data ------------------------------------------------------------------
print("Filter data")

df <- df[df$aer_age=="overall" &
           df$aer_sex=="overall" &
           df$analysis=="day0_main" & 
           df$days==196,]

# Format data ------------------------------------------------------------------
print("Format data")

df$excess_risk <- df$cumulative_difference_absolute_excess_risk*perpeople
df <- df[,c("cohort","day0","excess_risk")]

# Pivot table ------------------------------------------------------------------
print("Pivot table")

df$day0 <- paste0("day0",df$day0)

df <- tidyr::pivot_wider(df, 
                         names_from = c("cohort","day0"),
                         values_from = c("excess_risk"))

# Difference attributable to day0 ----------------------------------------------
print("Difference attributable to day0")

df$prevax_extf_day0diff <- (1-(df$prevax_day0FALSE/df$prevax_day0TRUE))*100
df$vax_day0diff <- (1-(df$vax_day0FALSE/df$vax_day0TRUE))*100
df$unvax_extf_day0diff <- (1-(df$unvax_day0FALSE/df$unvax_day0TRUE))*100

# Round numerics ---------------------------------------------------------------
print("Round numerics")

df <- df %>% 
  dplyr::mutate_if(is.numeric, ~round(., 0))

# Save table -------------------------------------------------------------------
print("Save table")

write.csv(df, paste0("C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results/AER/lifetables_compiled.csv"),na = "-")
