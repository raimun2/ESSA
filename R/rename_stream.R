#' rename GPS streams according to convention long, lat, elev, timestamp and time
#'
#' @param data a dataframe or tibble containing a GPS stream
#' rename_stream()
 
rename_stream <- function(data){
  
  # rename variables as convention long, lat, elev, timestamp
  data <- plyr::rename(data,c("position_long"="long","lng"="long","lon"="long","longitude"="long"),warn_missing=F)
  data <- plyr::rename(data,c("position_lat"="lat","latitude"="lat"),warn_missing=F)
  data <- plyr::rename(data,c("ele"="elev","altitude"="elev"),warn_missing=F)
  
  if(is.character(data$time)){
    data <- plyr::rename(data,c("time"="timestamp"),warn_missing=F)
    
    # time is defined as seconds from start
    data$time <- as.numeric(lubridate::as_datetime(data$timestamp) - 
                              min(lubridate::as_datetime(data$timestamp)))
  } 
  
  # reorder renamed columns to the front
  columnas_aux <- c("time",colnames(data)[!colnames(data) %in% c("long","lat","elev", "time")])
  
  data <- cbind(data[,c("long","lat","elev")], 
                data[,c(columnas_aux)])
  
  # return uniformed stream
  return(data)
}
