#' Processes gpx files and stores the result in a data frame
#'
#' Processes gpx files and stores the result in a data frame. The code is adapted from the blog post \href{https://rcrastinate.blogspot.com/2014/09/stay-on-track-plotting-gps-tracks-with-r.html}{Stay on track: Plotting GPS tracks with R} by Sascha W.
#' @param path The file path to the directory containing the gpx files
#' @param old_gpx_format If TRUE, uses the old format for gpx files (for files bulk exported from Strava prior to ~May 2018)
#' @keywords
#' @export
#' @examples
#' process_data()

load_stream_file <- function(filename){
  
  if(length(grep(".fit.gz",filename))>0)
  {
    R.utils::gunzip(filename,remove=FALSE, overwrite=TRUE)
    filename <- gsub(".gz$","",filename)
  }
  if(length(grep(".fit$",filename))>0){
    
    file_data <- FITfileR::readFitFile(filename)
    stream_df <- FITfileR::records(file_data)
    stream_df$timestamp <- lubridate::as_datetime(stream_df$timestamp)
    
  } else if(grep(".gpx$",filename)==1) {
    file_data <- plotKML::readGPX(filename)
    stream_df <- tibble::as_tibble(file_data$tracks[[1]][[1]])
    stream_df$ele <- as.numeric(stream_df$ele)
    stream_df$time <- lubridate::as_datetime(stream_df$time)
  } else{
    message("unsupported format")
  }
  return(stream_df)
}


file_to_stream_df <- function(filelist){
  streams <- NULL
  
  if(!is.null(filelist)){
    for(file in filelist){
      stream <- load_stream_file(file)
      stream <- rename_stream(stream)
      stream$id <- as.numeric(min(stream$timestamp))
      streams <- plyr::rbind.fill(streams,stream)
    }
  }
  return(streams)
}
