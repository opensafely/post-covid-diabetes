library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)

# DIRECTORIES -------------------------------------------------------------

results_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/descriptive/")
output_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/generated-figures/")

# READ DATA ---------------------------------------------------------------

values <- read.csv(paste0(results_dir,"/","Cohort_flow_unvax_diabetes.csv"))
# calculate N removed with diabetes
values[14,2] <- values[13,1] - values[14,1]
values$N_removed <- as.numeric(values$N_removed)

# DEFINE MAIN BOXES -------------------------------------------------------


# BOX 1 

study_sample_pre_qa <- boxGrob(glue("Study defined sample size before QA checks",
                                    "N = {pop}",
                                    pop = txtInt(values[1,1]),
                                    .sep = "\n"))

# BOX 2

study_sample_post_qa <- boxGrob(glue("Study defined sample size after QA checks",
                                     "N = {pop}",
                                     pop = txtInt(values[2,1]),
                                     .sep = "\n"))

# BOX 3

criteria_1 <- boxGrob(glue("Criteria 1 (Inclusion): Alive on the first day of follow up",
                           "N = {pop}",
                           pop = txtInt(values[3,1]),
                           .sep = "\n"))

# BOX 4

criteria_2a <- boxGrob(glue("Criteria 2a (Inclusion): Aged 18 and over on index date",
                            "N = {pop}",
                            pop = txtInt(values[4,1]),
                            .sep = "\n"))

# BOX 5

criteria_2b <- boxGrob(glue("Criteria 2b (Inclusion): Aged 110 and under on index date",
                            "N = {pop}",
                            pop = txtInt(values[5,1]),
                            .sep = "\n"))

# BOX 6

criteria_3 <- boxGrob(glue("Criteria 3 (Inclusion): Known sex",
                           "N = {pop}",
                           pop = txtInt(values[6,1]),
                           .sep = "\n"))

# BOX 7

criteria_4 <- boxGrob(glue("Criteria 4 (Inclusion): Known deprivation",
                           "N = {pop}",
                           pop = txtInt(values[7,1]),
                           .sep = "\n"))

# BOX 8

criteria_5 <- boxGrob(glue("Criteria 5 (Inclusion): Registered in an English GP with TPP software for at least 6 months prior to the study start date",
                           "N = {pop}",
                           pop = txtInt(values[8,1]),
                           .sep = "\n"))

# BOX 9

criteria_6 <- boxGrob(glue("Criteria 6 (Exclusion): Not deregistered from all support practices between index and end of study date",
                           "N = {pop}",
                           pop = txtInt(values[9,1]),
                           .sep = "\n"))

# BOX 10

criteria_7 <- boxGrob(glue("Criteria 7 (Inclusion): Known region",
                           "N = {pop}",
                           pop = txtInt(values[10,1]),
                           .sep = "\n"))

# BOX 11

criteria_8 <- boxGrob(glue("Criteria 8 (Exclusion): Have a record of a first vaccination prior index date",
                           "N = {pop}",
                           pop = txtInt(values[11,1]),
                           .sep = "\n"))

# BOX 12

criteria_9 <- boxGrob(glue("Criteria 9 (Exclusion): Missing or unknown JCVI group",
                           "N = {pop}",
                           pop = txtInt(values[12,1]),
                           .sep = "\n"))

# BOX 13

criteria_10 <- boxGrob(glue("Criteria 10 (Inclusion): Patient index date is within the study start and end dates i.e patients eligibility date + 84 days is before the study end date",
                           "N = {pop}",
                           pop = txtInt(values[13,1]),
                           .sep = "\n"))

# BOX 14

criteria_diabetes <- boxGrob(glue("Diabetes specific criteria: Remove those with diabetes prior to study start date",
                                  "N = {pop}",
                                  pop = txtInt(values[14,1]),
                                  .sep = "\n"))

# BOX 15

criteria_cov_history <- boxGrob(glue("Remove those with a COVID-19 diagnosis prior to the study start date",
                                     "N = {pop}",
                                     pop = txtInt(values[15,1]),
                                     .sep = "\n"))

# DEFINE EXCLUSION BOXES --------------------------------------------------

exclude1 <- boxGrob(glue("Excluded (n = {tot})",
                         tot = txtInt(values[2,2]),
                         .sep = "\n"),
                    just = "left")

exclude2 <- boxGrob(glue("Excluded (n = {tot})",
                         tot = txtInt(values[3,2]),
                         .sep = "\n"),
                    just = "left")

exclude3 <- boxGrob(glue("Excluded (n = {tot})",
                         tot = txtInt(values[4,2]),
                         .sep = "\n"),
                    just = "left")

exclude4 <- boxGrob(glue("Excluded (n = {tot})",
                         tot = txtInt(values[5,2]),
                         .sep = "\n"),
                    just = "left")

exclude5 <- boxGrob(glue("Excluded (n = {tot})",
                         tot = txtInt(values[6,2]),
                         .sep = "\n"),
                    just = "left")

exclude6 <- boxGrob(glue("Excluded (n = {tot})",
                         tot = txtInt(values[7,2]),
                         .sep = "\n"),
                    just = "left")

exclude7 <- boxGrob(glue("Excluded (n = {tot})",
                         tot = txtInt(values[8,2]),
                         .sep = "\n"),
                    just = "left")

exclude8 <- boxGrob(glue("Excluded (n = {tot})",
                         tot = txtInt(values[9,2]),
                         .sep = "\n"),
                    just = "left")

exclude9 <- boxGrob(glue("Excluded (n = {tot})",
                         tot = txtInt(values[10,2]),
                         .sep = "\n"),
                    just = "left")

exclude10 <- boxGrob(glue("Excluded (n = {tot})",
                          tot = txtInt(values[11,2]),
                          .sep = "\n"),
                     just = "left")

exclude11 <- boxGrob(glue("Excluded (n = {tot})",
                          tot = txtInt(values[12,2]),
                          .sep = "\n"),
                     just = "left")

exclude12 <- boxGrob(glue("Excluded (n = {tot})",
                          tot = txtInt(values[13,2]),
                          .sep = "\n"),
                     just = "left")

exclude13 <- boxGrob(glue("Excluded (n = {tot})",
                          tot = txtInt(values[14,2]),
                          .sep = "\n"),
                     just = "left")

exclude14 <- boxGrob(glue("Excluded (n = {tot})",
                          tot = txtInt(values[15,2]),
                          .sep = "\n"),
                     just = "left")

# DRAW FLOW ---------------------------------------------------------------

png(paste0(output_dir,"cohort_flow_unvax.png"),
    width = 12,
    height = 20,
    units = "in",
    res = 300)
grid.newpage()

# DEFINE VERTICAL BOXES

vert <- spreadVertical(study_sample_pre_qa = study_sample_pre_qa,
                       study_sample_post_qa = study_sample_post_qa,
                       criteria_1 = criteria_1,
                       criteria_2a = criteria_2a,
                       criteria_2b = criteria_2b,
                       criteria_3 = criteria_3,
                       criteria_4 = criteria_4,
                       criteria_5 = criteria_5,
                       criteria_6 = criteria_6,
                       criteria_7 = criteria_7,
                       criteria_8 = criteria_8,
                       criteria_9 = criteria_9,
                       criteria_10 = criteria_10,
                       criteria_diabetes = criteria_diabetes,
                       criteria_cov_history = criteria_cov_history)

# DEFINE EXCLUSION BOXES PLACEMENT

exclude1 <- moveBox(exclude1,
                    x = .8,
                    y = coords(vert$study_sample_post_qa)$top + distance(vert$study_sample_pre_qa, vert$study_sample_post_qa, half = TRUE, center = FALSE))

exclude2 <- moveBox(exclude2,
                    x = .8,
                    y = coords(vert$criteria_1)$top + distance(vert$study_sample_post_qa, vert$criteria_1, half = TRUE, center = FALSE))

exclude3 <- moveBox(exclude3,
                    x = .8,
                    y = coords(vert$criteria_2a)$top + distance(vert$criteria_1, vert$criteria_2a, half = TRUE, center = FALSE))

exclude4 <- moveBox(exclude4,
                    x = .8,
                    y = coords(vert$criteria_2b)$top + distance(vert$criteria_2a, vert$criteria_2b, half = TRUE, center = FALSE))

exclude5 <- moveBox(exclude5,
                    x = .8,
                    y = coords(vert$criteria_3)$top + distance(vert$criteria_2b, vert$criteria_3, half = TRUE, center = FALSE))

exclude6 <- moveBox(exclude6,
                    x = .8,
                    y = coords(vert$criteria_4)$top + distance(vert$criteria_3, vert$criteria_4, half = TRUE, center = FALSE))

exclude7 <- moveBox(exclude7,
                    x = .8,
                    y = coords(vert$criteria_5)$top + distance(vert$criteria_4, vert$criteria_5, half = TRUE, center = FALSE))

exclude8 <- moveBox(exclude8,
                    x = .8,
                    y = coords(vert$criteria_6)$top + distance(vert$criteria_5, vert$criteria_6, half = TRUE, center = FALSE))

exclude9 <- moveBox(exclude9,
                    x = .8,
                    y = coords(vert$criteria_7)$top + distance(vert$criteria_6, vert$criteria_7, half = TRUE, center = FALSE))

exclude10 <- moveBox(exclude10,
                     x = .8,
                     y = coords(vert$criteria_8)$top + distance(vert$criteria_7, vert$criteria_8, half = TRUE, center = FALSE))

exclude11 <- moveBox(exclude11,
                     x = .8,
                     y = coords(vert$criteria_9)$top + distance(vert$criteria_8, vert$criteria_9, half = TRUE, center = FALSE))

exclude12 <- moveBox(exclude12,
                     x = .8,
                     y = coords(vert$criteria_10)$top + distance(vert$criteria_9, vert$criteria_10, half = TRUE, center = FALSE))

exclude13 <- moveBox(exclude13,
                     x = .8,
                     y = coords(vert$criteria_diabetes)$top + distance(vert$criteria_10, vert$criteria_diabetes, half = TRUE, center = FALSE))

exclude14 <- moveBox(exclude14,
                     x = .8,
                     y = coords(vert$criteria_cov_history)$top + distance(vert$criteria_diabetes, vert$criteria_cov_history, half = TRUE, center = FALSE))

# GAPS BETWEEN VERTICAL BOXES

for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}

# CONNECTIONS

connectGrob(vert$study_sample_pre_qa, exclude1, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude2, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude3, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude4, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude5, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude6, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude7, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude8, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude9, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude10, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude11, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude12, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude13, type = "L")
connectGrob(vert$study_sample_pre_qa, exclude14, type = "L")

# PRINT BOXES

vert
exclude1
exclude2
exclude3
exclude4
exclude5
exclude6
exclude7
exclude8
exclude9
exclude10
exclude11
exclude12
exclude13
exclude14

dev.off()

# END
