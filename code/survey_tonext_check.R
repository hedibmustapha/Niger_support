library(lubridate)
library(dplyr)
library(purrr)
source("./code/functions.R")

data <- read.csv("./input/data.csv", sep = ";", 
                 na.strings = c(""," ","n/a","NA",NA),
                 stringsAsFactors = F)


data$starttime = ymd_hms(data[["start"]])
data$endtime = ymd_hms(data[["end"]])
  

list_data_byenum <- data %>% 
  split(.$global_enum_id) %>%
  map(~.[order(.$start),])

i <- 1
time_diff <- vector("double")
surveys_uuid <- vector("character")
enum <- vector("character")

results <- map(list_data_byenum, survey_tonext_loop,
    i,"starttime", "endtime","mins","X_uuid"," | ", time_diff, surveys_uuid, enum, "global_enum_id")

results <- results[sapply(results, function(x) dim(x)[1]) > 0] %>% bind_rows()

results <- results %>% mutate(
  issue = case_when(
    between(diff, -5, 0) ~ "temps entre deux enquetes succesives court",
    diff >0 ~ "l'enquete a commencé avant la fin de l'enquete precedente",
    TRUE ~ "Rien a signalé"
  )
)

write.csv(results, "./output/results.csv")

