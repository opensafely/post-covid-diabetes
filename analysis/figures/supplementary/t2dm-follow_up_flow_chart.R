library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)
library(data.table)

# DIRECTORIES -------------------------------------------------------------

results_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/descriptive/")
output_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v3/generated-figures/")

# READ DATA ---------------------------------------------------------------

values <- fread(paste0(results_dir,"diabetes_posthoc_analysis_res_4mnths.txt"))

# DEFINE MAIN BOXES -------------------------------------------------------


# BOX 1A 

t2dm_prevax <- boxGrob(glue("Pre-vaccination cohort type 2 diabetes events following a COVID-19 infection",
                                    "N all COVID = {pop}",
                                    "N hospitalised COVID = {pop2}",
                                    "N non-hospitalised COVID = {pop2}",
                                    pop = txtInt(values[1,1]),
                                    pop2 = txtInt(values[1,2]),
                                    pop3 = txtInt(values[1,3]),
                                    .sep = "\n"))

# BOX 1B

t2dm_vax <- boxGrob(glue("Vaccinated cohort type 2 diabetes events following a COVID-19 infection",
                            "N all COVID = {pop}",
                            "N hospitalised COVID = {pop2}",
                            "N non-hospitalised COVID = {pop2}",
                            pop = txtInt(values[2,1]),
                            pop2 = txtInt(values[2,2]),
                            pop3 = txtInt(values[2,3]),
                            .sep = "\n"))

# BOX 1C

t2dm_unvax <- boxGrob(glue("Unvaccinated cohort type 2 diabetes events following a COVID-19 infection",
                         "N all COVID = {pop}",
                         "N hospitalised COVID = {pop2}",
                         "N non-hospitalised COVID = {pop2}",
                         pop = txtInt(values[3,1]),
                         pop2 = txtInt(values[3,2]),
                         pop3 = txtInt(values[3,3]),
                         .sep = "\n"))

# BOX 2A 

t2dm_prevax <- boxGrob(glue("Pre-vaccination cohort type 2 diabetes events following a COVID-19 infection",
                            "N all COVID = {pop}",
                            "N hospitalised COVID = {pop2}",
                            "N non-hospitalised COVID = {pop2}",
                            pop = txtInt(values[1,1]),
                            pop2 = txtInt(values[1,2]),
                            pop3 = txtInt(values[1,3]),
                            .sep = "\n"))

# BOX 2B

t2dm_vax <- boxGrob(glue("Vaccinated cohort type 2 diabetes events following a COVID-19 infection",
                         "N all COVID = {pop}",
                         "N hospitalised COVID = {pop2}",
                         "N non-hospitalised COVID = {pop2}",
                         pop = txtInt(values[2,1]),
                         pop2 = txtInt(values[2,2]),
                         pop3 = txtInt(values[2,3]),
                         .sep = "\n"))

# BOX 2C

t2dm_unvax <- boxGrob(glue("Unvaccinated cohort type 2 diabetes events following a COVID-19 infection",
                           "N all COVID = {pop}",
                           "N hospitalised COVID = {pop2}",
                           "N non-hospitalised COVID = {pop2}",
                           pop = txtInt(values[3,1]),
                           pop2 = txtInt(values[3,2]),
                           pop3 = txtInt(values[3,3]),
                           .sep = "\n"))

# DRAW FLOW ---------------------------------------------------------------

png(paste0(output_dir,"t2dm_follow_up_flow.png"),
    width = 10,
    height = 18,
    units = "in",
    res = 300)
grid.newpage()

# DEFINE VERTICAL BOXES

vert_prevax <- spreadVertical(t2dm_prevax = t2dm_prevax)

vert_vax <- spreadVertical(t2dm_vax = t2dm_vax)

vert_unvax <- spreadVertical(t2dm_unvax = t2dm_unvax)

# DEFINE EXCLUSION BOXES PLACEMENT

# exclude1 <- moveBox(exclude1,
#                     x = .8,
#                     y = coords(vert$study_sample_post_qa)$top + distance(vert$study_sample_pre_qa, vert$study_sample_post_qa, half = TRUE, center = FALSE))
# 
# exclude2 <- moveBox(exclude2,
#                     x = .8,
#                     y = coords(vert$criteria_1)$top + distance(vert$study_sample_post_qa, vert$criteria_1, half = TRUE, center = FALSE))
# 
# exclude3 <- moveBox(exclude3,
#                     x = .8,
#                     y = coords(vert$criteria_2a)$top + distance(vert$criteria_1, vert$criteria_2a, half = TRUE, center = FALSE))
# 
# exclude4 <- moveBox(exclude4,
#                     x = .8,
#                     y = coords(vert$criteria_2b)$top + distance(vert$criteria_2a, vert$criteria_2b, half = TRUE, center = FALSE))
# 
# exclude5 <- moveBox(exclude5,
#                     x = .8,
#                     y = coords(vert$criteria_3)$top + distance(vert$criteria_2b, vert$criteria_3, half = TRUE, center = FALSE))
# 
# exclude6 <- moveBox(exclude6,
#                     x = .8,
#                     y = coords(vert$criteria_4)$top + distance(vert$criteria_3, vert$criteria_4, half = TRUE, center = FALSE))
# 
# exclude7 <- moveBox(exclude7,
#                     x = .8,
#                     y = coords(vert$criteria_5)$top + distance(vert$criteria_4, vert$criteria_5, half = TRUE, center = FALSE))
# 
# exclude8 <- moveBox(exclude8,
#                     x = .8,
#                     y = coords(vert$criteria_6)$top + distance(vert$criteria_5, vert$criteria_6, half = TRUE, center = FALSE))
# 
# exclude9 <- moveBox(exclude9,
#                     x = .8,
#                     y = coords(vert$criteria_7)$top + distance(vert$criteria_6, vert$criteria_7, half = TRUE, center = FALSE))
# 
# exclude10 <- moveBox(exclude10,
#                      x = .8,
#                      y = coords(vert$criteria_diabetes)$top + distance(vert$criteria_7, vert$criteria_diabetes, half = TRUE, center = FALSE))
# 
# exclude11 <- moveBox(exclude11,
#                      x = .8,
#                      y = coords(vert$criteria_cov_history)$top + distance(vert$criteria_diabetes, vert$criteria_cov_history, half = TRUE, center = FALSE))


# GAPS BETWEEN VERTICAL BOXES

for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}

# CONNECTIONS

# connectGrob(vert$study_sample_pre_qa, exclude1, type = "L")
# connectGrob(vert$study_sample_pre_qa, exclude2, type = "L")
# connectGrob(vert$study_sample_pre_qa, exclude3, type = "L")
# connectGrob(vert$study_sample_pre_qa, exclude4, type = "L")
# connectGrob(vert$study_sample_pre_qa, exclude5, type = "L")
# connectGrob(vert$study_sample_pre_qa, exclude6, type = "L")
# connectGrob(vert$study_sample_pre_qa, exclude7, type = "L")
# connectGrob(vert$study_sample_pre_qa, exclude8, type = "L")
# connectGrob(vert$study_sample_pre_qa, exclude9, type = "L")
# connectGrob(vert$study_sample_pre_qa, exclude10, type = "L")
# connectGrob(vert$study_sample_pre_qa, exclude11, type = "L")

# PRINT BOXES

vert

dev.off()

# END
