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
