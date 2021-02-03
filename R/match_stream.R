match_stream <- function(data, route, tolerance = 20){
  
  # extrae bbox ruta
  bbox <- sp::bbox(sp::SpatialPoints(route[,c("long","lat")]))
  
  route$id <- 1:nrow(route)
  data$id_row <- 1:nrow(data)
  
  inicio_fin <- route %>% filter(id <= 4 | id >= (nrow(route)-3))
  
  
  distances <- geosphere::distm(cbind(data$long,data$lat), 
                                cbind(inicio_fin$long,inicio_fin$lat), 
                                fun = geosphere::distHaversine)
  
  
  act_filt <- data %>% filter(id_row >= which(distances[,1] < tolerance)[1],
                              id_row <= max(which(distances[,6] < tolerance)))
    
  
  distances <- geosphere::distm(cbind(act_filt$long,act_filt$lat), 
                                cbind(route$long,route$lat), 
                                fun = geosphere::distHaversine)
  

  min_act <- apply(distances,1,min)
  
  # verifica contiguidad temporal de los puntos entre el punto inicial y los siguientes que tienen una distancia menor a tolerance
  act_filt <- act_filt[((min_act < tolerance) %>% 
                as.numeric() %>% 
                diff() %>% 
                abs() %>% 
                cumsum() == 0),]
  
  # verifica distancia de cada punto de la ruta a streams
  distances <- geosphere::distm(cbind(act_filt$long,act_filt$lat), 
                                cbind(route$long,route$lat), 
                                fun = geosphere::distHaversine)

  min_dist <- apply(distances,2,min)
  


  # comprueba tolerancia de contiguidad temporal y espacial
  cumplimiento  <- (min_dist  < tolerance) %>% 
                        as.numeric() %>% 
                        mean()
  
  if(cumplimiento == 1){
    return(TRUE) } else { return(FALSE)
  }
  
}

