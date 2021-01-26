#' Processes gpx files and stores the result in a data frame
#'
#' Processes gpx files and stores the result in a data frame. The code is adapted from the blog post \href{https://rcrastinate.blogspot.com/2014/09/stay-on-track-plotting-gps-tracks-with-r.html}{Stay on track: Plotting GPS tracks with R} by Sascha W.
#' @param path The file path to the directory containing the gpx files
#' @param old_gpx_format If TRUE, uses the old format for gpx files (for files bulk exported from Strava prior to ~May 2018)
#' @keywords
#' @export
#' @examples
#' process_data()
 
rename_stream <- function(data){
  
  # rename recorded variables as convention long, lat, elev, timestamp
  data <- plyr::rename(data,c("position_long"="long","lng"="long","lon"="long","longitude"="long"),warn_missing=F)
  data <- plyr::rename(data,c("position_lat"="lat","latitude"="lat"),warn_missing=F)
  data <- plyr::rename(data,c("ele"="elev","altitude"="elev"),warn_missing=F)
  data <- plyr::rename(data,c("time"="timestamp"),warn_missing=F)
  
  # time is calculated as seconds from start
  data$time <- as.numeric(lubridate::as_datetime(data$timestamp) - 
                            min(lubridate::as_datetime(data$timestamp)))
  
  # reorder rename columns to the front
  columnas_aux <- c("time",colnames(data)[!colnames(data) %in% c("long","lat","elev","time")])
  
  data <- cbind(data[,c("long","lat","elev")], 
                data[,c(columnas_aux)])
  
  # return enhanced dataframe
  return(data)
}