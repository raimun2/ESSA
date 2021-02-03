differential_stream <- function(data_acts){

  data_diff <- NULL
  
  for(act in unique(data_acts$id)){
    
    data <- data_acts %>%  filter(id == act)
    # previous long / lat
    data$point2_lat = c(0,data$lat[-nrow(data)])
    data$point2_long = c(0,data$long[-nrow(data)])
    
    # haversise distance between each point and previous one
    data$delta_distance = geosphere::distHaversine(cbind(data$long,data$lat),
                                                   cbind(data$point2_long,data$point2_lat))
    data$delta_distance[1] = 0
    
    ## 3d distance tambien
    
    #data <- streams_rai$streams[streams_rai$streams$id == 3945967318,]
    
    # calculate cumulative distance in kms
    data$distance <- cumsum(data$delta_distance) / 1000
    
    data$delta_elev <- c(0,diff(data$elev,lag=1))  
    data$delta_elev[1] <- 0
    data$dplus <- ifelse(data$delta_elev>0, data$delta_elev, 0)
    data$dminus <- ifelse(data$delta_elev<0, data$delta_elev, 0)
    
    # 2 point previous
    data$point3_lat = c(0,data$point2_lat[-nrow(data)])
    data$point3_long = c(0,data$point2_long[-nrow(data)])
    
    vector1 <- cbind(data$long-data$point2_long, data$lat-data$point2_lat)
    vector2 <- cbind(data$point3_long-data$point2_long,data$point3_lat-data$point2_lat)
    num <- vector1[,1]*vector2[,1] + vector1[,2]*vector2[,2]
    den <- sqrt(vector1[,1]^2 + vector1[,2]^2)*sqrt(vector2[,1]^2 + vector2[,2]^2)
    data$angle <- ifelse(den == 0, 180, (360*acos(num/den))/(2*pi) )
    
    data$point2_lat = NULL
    data$point2_long = NULL
    data$point3_lat = NULL
    data$point3_long = NULL
    
    data$grade = ( data$delta_elev / data$delta_distance )
    data$grade[1] = 0
    data[data$delta_distance == 0,]$grade = 0
    
    if(!is.null(data$time)){
      # calculate difference in time and elevation between readings
      data$delta_time = c(0,diff(data$time,lag=1)) 
      data$delta_time[1] = 0
      
      # calculate derivatives: vertical and horizontal velocity, in meters per second
      data$vert_velocity = data$delta_elev / data$delta_time
      data$vert_velocity[1] = 0
      
      data$hz_velocity = data$delta_distance / data$delta_time
      data$hz_velocity[1] = 0
      
      #3d velocity
      data$velocity <- sqrt(data$vert_velocity^2 + data$hz_velocity^2)
      
      # calculate pace in minutes per kilometer
      data$pace <- c(0,1000/60/data$hz_velocity[-1])
    }
    
    data_diff <- rbind(data_diff, data)
  }
  
  
  # return enriched dataset
  return(data_diff)
}
