smooth_stream<-function(data, alpha = 0.05, interpolate = FALSE){
  
  # generate model for long, lat and elev in function of time
  # for routes without time uses the GPS readings cumulative count
  if(is.null(data$time)){
    data$reading <- 1:nrow(data)
    longmodel <- loess(data$long~data$reading,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
    latmodel <- loess(data$lat~data$reading,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
    elevmodel <- loess(data$elev~data$reading,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
    data$reading <- NULL
    
  } else{
    longmodel <- loess(data$long~data$time,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
    latmodel <- loess(data$lat~data$time,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
    elevmodel <- loess(data$elev~data$time,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
  }
  
  # if interpolate is false smooth recorded coordinates
  if(!interpolate){
    data$smoothLong = predict(longmodel)
    data$smoothLat = predict(latmodel)
    data$smoothElev = predict(elevmodel)
    return(data) #return enhanced dataframe
    
  } else{ # if interpolate, generate interpolated route with 1 second difference between points
    
    smoothActivity <- data.frame("time"=1:(max(data$time)))
    smoothActivity$long <- predict(longmodel,smoothActivity$time)
    smoothActivity$lat <- predict(latmodel,smoothActivity$time)
    smoothActivity$elev <- predict(elevmodel,smoothActivity$time)
    smoothActivity <- smoothActivity[,c("long","lat","elev","time")]
    return(smoothActivity) # return interpolated dataframe
    
  }

}
