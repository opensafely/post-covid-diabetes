files <- list.files(path = "output/", pattern = "stata_median_fup")

df <- NULL

for (f in files) {
  
  tmp <- readr::read_csv(file = paste0("output/",f))
  tmp$source <- f
  df <- rbind(df,tmp)
  
}

readr::write_csv(df, "output/mediantte_and_events.csv")