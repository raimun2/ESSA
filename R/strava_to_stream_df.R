#' Processes gpx files and stores the result in a data frame
#'
#' Processes gpx files and stores the result in a data frame. The code is adapted from the blog post \href{https://rcrastinate.blogspot.com/2014/09/stay-on-track-plotting-gps-tracks-with-r.html}{Stay on track: Plotting GPS tracks with R} by Sascha W.
#' @param path The file path to the directory containing the gpx files
#' @param old_gpx_format If TRUE, uses the old format for gpx files (for files bulk exported from Strava prior to ~May 2018)
#' @keywords
#' @export
#' @examples
#' process_data()

strava_to_stream_df <- function(stoken, nacts){
  if(nacts > 100) message("se traeran las 100 actividades mas recientes")
  
  act_metadata <- rStrava::get_activity_list(stoken) %>% 
    rStrava::compile_activities() %>%
    rStrava::chk_nopolyline()
  
  nacts <- min(nacts,100)
  act_metadata <- act_metadata[1:nacts,]
  streams <- rStrava::get_activity_streams(act_metadata, stoken) %>% 
    rename_stream()  
  
  streams <- merge(streams,act_metadata[,c("id","start_date_local")], by="id")
  
  streams$timestamp <- streams$time + lubridate::as_datetime(streams$start_date_local)
  streams$start_date_local <- NULL
  
  return(list(streams = streams,
              metadata = act_metadata))
}
