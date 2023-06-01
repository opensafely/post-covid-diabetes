#Format stata output ready for plotting
library(stringi)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(tidyverse)

fs::dir_create(here::here("output", "review", "model","prevax"))
fs::dir_create(here::here("output", "review", "model","vax"))
fs::dir_create(here::here("output", "review", "model","unvax"))

#Read in R HRs

# PREVAX 

print(getwd())

hr_files=list.files(path = "output/review/model/prevax/", pattern = "to_release")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0("output/review/model/prevax/", hr_files)
temp <- lapply(hr_files, read_csv)
estimates_prevax <- rbindlist(temp, fill=TRUE)

print("prevax estimates read successfully")

# VAX

hr_files_vax=list.files(path = "output/review/model/vax/", pattern = "to_release")
hr_files_vax=hr_files_vax[endsWith(hr_files_vax,".csv")]
hr_files_vax=paste0("output/review/model/vax/", hr_files_vax)

temp <- lapply(hr_files_vax, read_csv)
estimates_vax <- rbindlist(temp, fill=TRUE)

print("vax estimates read successfully")

# UNVAX

hr_files_unvax=list.files(path = "output/review/model/unvax/", pattern = "to_release")
hr_files_unvax=hr_files_unvax[endsWith(hr_files_unvax,".csv")]
hr_files_unvax=paste0("output/review/model/unvax/", hr_files_unvax)

temp <- lapply(hr_files_unvax, read_csv)
estimates_unvax <- rbindlist(temp, fill=TRUE)

print("unvax estimates read successfully")

estimates <- rbindlist(list(estimates_prevax, estimates_vax, estimates_unvax), fill = TRUE)

estimates$source <- "R"

estimates$redacted_results <- factor(estimates$redacted_results, levels = c("Redacted results",
                                                                            "No redacted results"))
estimates <- estimates[order(estimates$redacted_results),]

write.csv(estimates, file = paste0("output/review/model/R_hr_output.csv"),row.names = FALSE)

# Read in event count files

# Event counts prevax
event_counts_prevax=list.files(path = "output/review/model/prevax/", pattern = "suppressed_compiled_event_counts")
event_counts_prevax=event_counts_prevax[endsWith(event_counts_prevax,".csv")]
event_counts_prevax=paste0("output/review/model/prevax/","/", event_counts_prevax)

temp <- lapply(event_counts_prevax, read_csv)
event_counts_prevax <- rbindlist(temp, fill=TRUE)

# Event counts vax
event_counts_vax=list.files(path = "output/review/model/vax/", pattern = "suppressed_compiled_event_counts")
event_counts_vax=event_counts_vax[endsWith(event_counts_vax,".csv")]
event_counts_vax=paste0("output/review/model/vax/","/", event_counts_vax)

temp <- lapply(event_counts_vax, read_csv)
event_counts_vax <- rbindlist(temp, fill=TRUE)

# Event counts unvax
event_counts_unvax=list.files(path = "output/review/model/unvax/", pattern = "suppressed_compiled_event_counts")
event_counts_unvax=event_counts_unvax[endsWith(event_counts_unvax,".csv")]
event_counts_unvax=paste0("output/review/model/unvax/","/", event_counts_unvax)

temp <- lapply(event_counts_unvax, read_csv)
event_counts_unvax <- rbindlist(temp, fill=TRUE)

event_counts <- rbindlist(list(event_counts_prevax, event_counts_vax, event_counts_unvax), fill = TRUE)

event_counts$redacted_results <- factor(event_counts$redacted_results, levels = c("Redacted results",
                                                                                        "No redacted results"))
event_counts <- event_counts[order(event_counts$redacted_results),]

write.csv(event_counts,"output/review/model/R_event_count_output.csv", row.names=F)
