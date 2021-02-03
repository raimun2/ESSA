#' Processes list of files containing GPS streams into a single dataframe
#'
#' @param filelist list of paths to the files containing the gpx or fit streams
#' file_to_stream_df()

filelist_to_stream_df <- function(filelist){
  streams <- NULL
    
  if(!is.null(filelist)){
    for(file in filelist){
      stream <- load_stream_file(file) # load stream from file
      stream <- rename_stream(stream) # rename all streams according to convention
      stream$id <- as.numeric(min(stream$timestamp)) # generate numeric id for this stream
      streams <- plyr::rbind.fill(streams,stream) # bind rows, mantaining all available columns
    }
  }
  return(streams)
}
