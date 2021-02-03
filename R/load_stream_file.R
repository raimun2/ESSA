#' Load GPS streams from a single gpx or fit file and stores it in a Tibble
#' Allows to process files from Strava's bulk export, which includes compressed fit files (*.fit.gz)
#' @param filename The file path to the directory containing file
#' load_stream_file()

load_stream_file <- function(filename){
  # if file ends in "*fit.gz", then unzip it, maintaining the original file
  if(length(grep(".fit.gz",filename))>0)
  {
    R.utils::gunzip(filename,remove=FALSE, overwrite=TRUE)
    filename <- gsub(".gz$","",filename) # remove ".gz" from filename
  }
  # if fit file (including uncompressed file) use FITfileR library
  if(length(grep(".fit$",filename))>0){
    
    file_data <- FITfileR::readFitFile(filename)
    stream_df <- FITfileR::records(file_data)
    stream_df$timestamp <- lubridate::as_datetime(stream_df$timestamp) #parse datetime
    
  } else if(grep(".gpx$",filename)==1) { # if gpx file use plotKML library
    file_data <- plotKML::readGPX(filename)
    stream_df <- tibble::as_tibble(file_data$tracks[[1]][[1]]) 
    stream_df$ele <- as.numeric(stream_df$ele) #parse elevation as numeric
    stream_df$time <- lubridate::as_datetime(stream_df$time) #parse datetime
  } else{
    message("unsupported format")
  }
  return(stream_df) #return tibble with file stream
}