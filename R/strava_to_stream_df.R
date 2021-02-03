#' Gathers GPS streams from Strava's API. returns a list containing the activity stream, and activity summary data
#'
#' @param stoken authorization token obtained from strava (see rStrava documentation)
#' @param nacts number of recent activities to get, default is 1, maximum value is 100
#' strava_to_stream_df()

strava_to_stream_df <- function(stoken, nacts = 1){
  if(nacts > 100) message("the 100 most recent activities will be fetched")
  
  # retrieve metadata, exclude activities without GPS recording
  act_metadata <- rStrava::get_activity_list(stoken) %>% 
    rStrava::compile_activities() %>%
    rStrava::chk_nopolyline()
  
  # sets maximum number of activities to 100 (due to Strava's API limitations)
  nacts <- min(nacts,100)
  act_metadata <- act_metadata[1:nacts,]
  
  # get selected activities streams and rename variables
  streams <- rStrava::get_activity_streams(act_metadata, stoken) %>% 
    rename_stream()  
  
    # reconstruct time stamp according to file streams convention
  streams <- merge(streams,act_metadata[,c("id","start_date_local")], by="id")
  streams$timestamp <- streams$time + lubridate::as_datetime(streams$start_date_local)
  streams$start_date_local <- NULL
  
  return(list(streams = streams,
              metadata = act_metadata))
}
