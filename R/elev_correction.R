elev_correction_point <- function(data, z = 13){
  # define map projection
  ll_prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # generate spatial data frame with coordinates
  puntos = sp::SpatialPointsDataFrame(coords = data.frame(cbind(data$long, data$lat)),
                                  data = data.frame(cbind(data$long, data$lat)),
                                  proj4string = sp::CRS(ll_prj))

  # retrieve elevation from Digital Elevation Model (DEM)
  data$elev_DEM = elevatr::get_elev_point(locations = puntos, units = "meters",src="aws",z=z)@data$elevation

  # return enhanced data
  return(data)
}

#rast <- try(raster::getData("alt", country = country, download = TRUE, mask = mask))


elev_correction_raster <- function(data, z = 13){
  # define map projection
  ll_prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # generate spatial data frame with coordinates
  puntos = sp::SpatialPointsDataFrame(coords = data.frame(cbind(data$long, data$lat)),
                                  data = data.frame(cbind(data$long, data$lat)),
                                  proj4string = sp::CRS(ll_prj))

  # retrieve elevation from Digital Elevation Model (DEM)
  raster <- elevatr::get_elev_raster(locations = puntos, units = "meters",src="aws",z=z)

  data$elev_DEM = raster::extract(raster, puntos)


  # return dataframe
  return(data)
}

elev_correction_raster_local <- function(data, raster, z = 13){
  # define map projection
  ll_prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # generate spatial data frame with coordinates
  puntos = sp::SpatialPointsDataFrame(coords = data.frame(cbind(data$long, data$lat)),
                                      data = data.frame(cbind(data$long, data$lat)),
                                      proj4string = sp::CRS(ll_prj))

  # retrieve elevation from Digital Elevation Model (DEM)
  #raster <- elevatr::get_elev_raster(locations = puntos, units = "meters",src="aws",z=z)

  data$elev_DEM = raster::extract(raster, puntos)


  # return dataframe
  return(data)
}

## global function if datapoints < 200, retrieve each one, else retrieve raster
## if local raster available, extract elevations directly
elev_correction <- function(data, z = 13, raster=NULL){
  if(is.null(raster)){
      if(nrow(data)<200){
        elev_correction_point(data, z)
      } else {
        elev_correction_raster(data, z)
      }
  } else {
    elev_correction_raster_local(data, z, raster)
  }
}
