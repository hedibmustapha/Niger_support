library(lubridate)
library(dplyr)
library(purrr)
source("./code/functions.R")

data <- read.csv("./input/data.csv", 
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
date <- vector()
start <- vector()
nom_site <- vector()
question <- vector()

results <- map(list_data_byenum, survey_tonext_loop,
    i,"starttime", "endtime",
    "mins","X_uuid"," | ", time_diff, uuid, id_enqueteur, 
    "global_enum_id", date_enquete, "today", start, "start", nom_site, "liste_enquete", nom_question)

results <- results[sapply(results, function(x) dim(x)[1]) > 0] %>% bind_rows()

results <- results %>% mutate(
  probleme = case_when(
    between(diff, -5, 0) ~ "temps entre deux enquêtes successives court",
    diff >0 ~ "l'enquete a commencé avant la fin de l'enquete precedente",
    TRUE ~ "Rien a signalé"
  )
) %>% select(-diff) %>% filter(probleme == "temps entre deux enquêtes successives court")

write.csv(results, "./output/results.csv")

