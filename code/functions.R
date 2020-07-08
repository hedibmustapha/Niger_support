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
                               endtime, unit, key, sep, diff, uuid, 
                               id_enqueteur, enum_id, date_enquete, date,
                               start,start_time, nom_site, site,
                               nom_question){
  
  while (pointer < nrow(df)) {
    diff <- c(diff,
              survey_tonext_duration(df, starttime, endtime, pointer, unit)
    )
    uuid <- c(uuid,
              survey_tonext_uuid(df, key, pointer, sep)
    )
    id_enqueteur <- c(id_enqueteur, df[[enum_id]][i])
    date_enquete <- c(date_enquete, df[[date]][i])
    start <- c(start, str_split(ymd_hms(df[[start_time]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, df[[site]][i])
    nom_question <- c(nom_question, "start")
    pointer <- pointer+1
  }

  out <- data.frame(diff,id_enqueteur,uuid,date_enquete,start,nom_site,nom_question)
  return(out)
}

time_check <- function(df, starttime, endtime, key){
  check <- df %>% transmute(start = ymd_hms(df[[starttime]]), end = ymd_hms(df[[endtime]]),
                            interview_duration = difftime(as.POSIXct(end), as.POSIXct(start), units = "mins"
                            ),
                            uuid = df[[key]]
  )
  return(check)
}

`%!in%` = Negate(`%in%`)