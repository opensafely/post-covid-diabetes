# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_rds("output/model_input-cohort_prevax-main-t2dm_extended_follow_up.rds")
study_dates <- jsonlite::fromJSON("output/study_dates.json")

# Create directory -------------------------------------------------------------
print("Create directory")

fs::dir_create(here::here("output", "explore"))

# Plot exposure ----------------------------------------------------------------
print("Plot exposure")

ggplot2::ggplot(df[!is.na(df$exp_date),], ggplot2::aes(x=exp_date)) + 
  ggplot2::geom_histogram(binwidth = 1) +
  ggplot2::geom_vline(xintercept = as.Date(study_dates$pandemic_start), colour = "#4daf4a") +
  ggplot2::geom_vline(xintercept = as.Date(study_dates$all_eligible), colour = "#377eb8") +
  ggplot2::geom_vline(xintercept = as.Date(study_dates$omicron_date), colour = "#e41a1c") +
  ggplot2::xlim(as.Date("2019-12-01"),as.Date("2022-02-01")) +
  ggplot2::labs(x = "Exposure date", y = "Frequency") +
  ggplot2::theme_minimal()

ggplot2::ggsave("output/explore/hist-all-exp_date.png", unit="mm", width = 297, height = 210, bg = "white")

# Plot outcome -----------------------------------------------------------------
print("Plot outcome")

ggplot2::ggplot(df[!is.na(df$out_date),], ggplot2::aes(x=out_date)) + 
  ggplot2::geom_histogram(binwidth = 1) +
  ggplot2::geom_vline(xintercept = as.Date(study_dates$pandemic_start), colour = "#4daf4a") +
  ggplot2::geom_vline(xintercept = as.Date(study_dates$all_eligible), colour = "#377eb8") +
  ggplot2::geom_vline(xintercept = as.Date(study_dates$omicron_date), colour = "#e41a1c") +
  ggplot2::xlim(as.Date("2019-12-01"),as.Date("2022-02-01")) +
  ggplot2::labs(x = "Outcome date", y = "Frequency") +
  ggplot2::theme_minimal()

ggplot2::ggsave("output/explore/hist-all-out_date.png", unit="mm", width = 297, height = 210, bg = "white")

# Plot outcome among exposed ---------------------------------------------------
print("Plot outcome among exposed")

ggplot2::ggplot(df[!is.na(df$exp_date) & !is.na(df$out_date),], ggplot2::aes(x=out_date)) + 
  ggplot2::geom_histogram(binwidth = 1) +
  ggplot2::geom_vline(xintercept = as.Date(study_dates$pandemic_start), colour = "#4daf4a") +
  ggplot2::geom_vline(xintercept = as.Date(study_dates$all_eligible), colour = "#377eb8") +
  ggplot2::geom_vline(xintercept = as.Date(study_dates$omicron_date), colour = "#e41a1c") +
  ggplot2::xlim(as.Date("2019-12-01"),as.Date("2022-02-01")) +
  ggplot2::labs(x = "Outcome date", y = "Frequency") +
  ggplot2::theme_minimal()

ggplot2::ggsave("output/explore/hist-exposed-out_date.png", unit="mm", width = 297, height = 210, bg = "white")