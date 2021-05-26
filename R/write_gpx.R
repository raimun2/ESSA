writeGPX <- function(stream, file="file.gpx"){
  # Write gpx data to a new file.
  lat <- stream$lat
  lon <- stream$lon
  ele <- stream$ele
  time <- stream$timestamp

  o   <- c('<gpx version="1.1" creator="raimun2">','<trk>','<trkseg>')
  o   <- c(o, paste('<trkpt lat="',lat,'" lon="',lon,'"><time>',
                  paste("<ele>",ele,"</ele>",sep=""),
                  paste(gsub(' ','T', as.character(time)), 'Z', sep=''),'</time></trkpt>', sep=''))
  o   <- c(o, '</trkseg>', '</trk>', '</gpx>')
  cat(o, file=file, sep='\n')
}

