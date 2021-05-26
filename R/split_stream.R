split_stream = function(data, size = 60, value = "time", windowed = FALSE){

  if(windowed){
    segmentos <- rolling_window(data, size)
  } else {
    # if time split segments according to elapsed time, else cumulative distance
    if(value == "time"){
      data$id_seg <- floor(data$time / size)
    } else if(value == "distance"){
      data$id_seg <- floor(data$distance*1000 / size) #cumulative distance in meters
    }

    segmentos <- data %>%
      agg_stream() %>%
      filter(delta_time > size - 10)
  }

  # return segments dataframe
  return(segmentos)
}

agg_stream <- function(data){
  if(nrow(data)>0){
    segment <- aggregate(cbind(delta_time, delta_distance, dplus, dminus) ~ id_seg, data, sum)

    aggmeans <- aggregate(cbind(time, distance, ele) ~ id_seg, data, mean)

    segment <- cbind(segment,aggmeans[,-(1)])

    segment$speed <- segment$delta_distance / segment$delta_time

    return(segment)
  }
}


rolling_window <- function(data, largo){

  windowed_data <- NULL
  for(act in unique(data$id_seg)){

    act_data <- data %>% filter(id_seg == act)
    act_data$timeseg <- cumsum(act_data$delta_time)

    for(t in act_data$timeseg){
      act_wind <- act_data %>% filter(timeseg >= t, timeseg < (t + largo))

      if((sum(act_wind$delta_time) + 10) > largo & nrow(act_wind)>0){
        subwind <- agg_stream(act_wind)
        windowed_data <- rbind(windowed_data, subwind)
      }

    }
    windowed_data <- unique(windowed_data)
  }
  return(windowed_data)
}
