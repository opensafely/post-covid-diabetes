# Developing standardised incidence rates for Table 2

require(dplyr)


# Specify directories -----------------------------------------------------------
results_dir <- "C:/Users/rd16568/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-diabetes/results"

# Specify parameters -----------------------------------------------------------
print('Specify parameters')

perpeople <- 100000 # per X people

# Load data --------------------------------------------------------------------
print('Load data')

df <- read.csv(paste0(results_dir,"/model/AER/aer_input-main-rounded.csv"))

df$outcome <- strsplit(df$outcome,'_extended_follow_up', fixed=TRUE) 

df <- df[df$unexposed_person_days!=0,]

# Specify reference data (prevax age and sex structure) --------------------------------------------------------------------
print('Specify ref data')

ref <- subset(df, select = c (aer_sex, aer_age, outcome, cohort, sample_size))
ref <- ref[ref$cohort=="prevax" &
           ref$outcome=="t1dm",]
ref <- subset(ref, select = c (aer_sex, aer_age, sample_size))

pop <- aggregate(x = ref$sample_size,                # Specify data column
                    by = list(ref$aer_sex),          # Specify group indicator
                    FUN = sum) 

ref$pop <- pop$x[match(ref$aer_sex, pop$Group.1)]
ref$per <- (ref$sample_size/ref$pop) # proportion of prevax cohort age and sex stratified

# Specify raw data --------------------------------------------------------------------
print('Specify raw data')

raw <- subset(df, select = c (aer_sex, aer_age, outcome, cohort, unexposed_person_days, unexposed_events, exposed_person_days, exposed_events))
names(raw)[names(raw)=="unexposed_person_days"] <- "persondays_0"
names(raw)[names(raw)=="exposed_person_days"] <- "persondays_1"
names(raw)[names(raw)=="unexposed_events"] <- "events_0"
names(raw)[names(raw)=="exposed_events"] <- "events_1"


raw <- raw %>% pivot_longer('persondays_0': 'events_1',
                            names_to=c('.value', 'exposure'),
                            names_pattern = "(.*)_(.*)",
                            values_to = c("var1, var2"))

# Calculate age and sex-specific incidence rate (per 100000 PY) --------------------------------------------------------------------
print('Calculate age and sex-specific incidence rate')

raw$IR <- round(raw$events/((raw$persondays/365.25)/100000))
raw <- subset(raw, select = c (aer_sex, aer_age, outcome, cohort, exposure, IR))
raw <- raw %>% pivot_wider(names_from = c('cohort'),
                          values_from = 'IR')

# Standardised IR data --------------------------------------------------------------------
print('Calculate standardised incidence rate (Age and sex-specific IR multiplied by proportion in prevax age and sex strata)')

sir <- merge(x=raw,y=ref,by=c("aer_sex", "aer_age"),all.x=TRUE)

sir$vax_asir <- sir$vax * sir$per
sir$unvax_asir <- sir$unvax * sir$per
sir$prevax_asir <- sir$prevax * sir$per

# sum over age and sex
sir <- subset(sir, select = c (outcome, exposure, vax_asir, unvax_asir, prevax_asir))

table2 <- sir %>% group_by(outcome, exposure) %>%
            summarize (across(vax_asir:prevax_asir, list(sum = sum)))