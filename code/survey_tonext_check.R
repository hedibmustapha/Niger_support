library(lubridate)
library(dplyr)
library(purrr)

survey_tonext_duration <- function(df, starttime, endtime, pointer, unit){
  
  difftime(as.POSIXct(df[[endtime]][pointer]), 
           as.POSIXct(df[[starttime]][pointer+1]), 
           units = unit)
  
}

survey_tonext_uuid <- function(df, key, pointer, sep){
  
  paste(df[[key]][pointer],
        df[[key]][pointer+1],
        sep = sep)
  
}

survey_tonext_loop <- function(df, pointer, starttime, 
                               endtime, unit, key, sep, diff, uuid, enum, enum_id){
  
  while (pointer < nrow(df)) {
  diff <- c(diff,
           survey_tonext_duration(df, starttime, endtime, pointer, unit)
  )
  uuid <- c(uuid,
            survey_tonext_uuid(df, key, pointer, sep)
  )
  enum <- c(enum, df[[enum_id]][i])
  
  pointer <- pointer+1
  
  }
  
  out <- data.frame(diff,uuid,enum)
  return(out)
}

data <- read.csv("./input/data.csv", sep = ";", 
                 na.strings = c(""," ","n/a","NA",NA),
                 stringsAsFactors = F)


data$starttime = ymd_hms(data[["start"]])
data$endtime = ymd_hms(data[["end"]])
  

# data <- data[order(data$starttime),]

list_data_byenum <- data %>% 
  split(.$global_enum_id) %>%
  map(~.[order(.$start),])

i <- 1
time_diff <- vector("double")
surveys_uuid <- vector("character")
enum <- vector("character")

# while (i < nrow(data)) {
#   diff <- c(diff,
#                survey_tonext_duration(data,"starttime","endtime", i, "mins"))
#   uuid <- c(uuid,
#                survey_tonext_uuid(data, "X_uuid", i, " | "))
#              i <- i+1
# 
# }

# result <-survey_tonext_loop(df = data, i, "start", "end","mins","X_uuid"," | ", diff, uuid )


results <- map(list_data_byenum, survey_tonext_loop,
    i,"starttime", "endtime","mins","X_uuid"," | ", time_diff, surveys_uuid, enum, "global_enum_id")

results <- results[sapply(results, function(x) dim(x)[1]) > 0]
results <- bind_rows(results)
results <- results %>% mutate(
  issue = case_when(
    between(diff, -5, 0) ~ "temps entre deux enquetes succesives court",
    diff >0 ~ "l'enquete a commencé avant la fin de l'enquete precedente",
    TRUE ~ "Rien a signalé"
  )
)

write.csv(data.frame(diff, uuid), "./diff3.csv")
write.csv(results, "./output/results.csv")


newdata2 <- mtcars[order(mtcars$mpg, -mtcars$cyl),]
