Endurance Sports Data
================

## Strava Bulk Export

Para iniciar el trabajo con datos deportivos, primero debemos
conseguirlos. Strava es una de las plataformas mas populares para
registrar esfuerzos con el smartphone o reloj GPS.

Llevo usando Strava desde 2014, registrando cada sesion de
entrenamiento, abarcando mas de 800 actividades a la fecha, cuyos
resumenes se pueden ver en el archivo
“datasets/export\_4085175/activities.csv”.

La carpeta export\_4085175 se genero automaticamente al exportar mis
actividades desde Strava (este link). En esta carpeta se pueden
encontrar archivos con 2 tipos de extensiones, “.gpx” y “.fit.gz”. Para
reducir el volumen solo mantuve 12 actividades en la carpeta.

``` r
path <- "datasets/export_4085175/"

archivos <- list.files(paste0(path,"activities"))

archivos
```

    ##  [1] "1471545002.gpx"    "1733584588.gpx"    "2509984347.fit"   
    ##  [4] "2509984347.fit.gz" "267706239.gpx"     "276629586.gpx"    
    ##  [7] "2811714314.fit.gz" "2829618918.fit.gz" "283941744.gpx"    
    ## [10] "2949519602.fit.gz" "365079513.gpx"     "3870184736.gpx"   
    ## [13] "4355281959.fit.gz"

Los archivos pueden contener informacion en forma de tracks (lecturas
consecutivas), waypoints (puntos de interes), routes, bounds y metadata.
En el caso de las actividades provenientes de Strava, la metadata de las
actividades no viene en el archivo GPX, sino que en un resumen de
actividades llamado activities.csv que esta en la carpeta del export

``` r
act_data <- read.csv(paste0(path,"activities.csv"), header=T)

summary(act_data[,1:15])
```

    ##  Id..de.actividad    Fecha.de.la.actividad Nombre.de.la.actividad
    ##  Min.   :1.196e+08   Length:882            Length:882            
    ##  1st Qu.:6.304e+08   Class :character      Class :character      
    ##  Median :1.680e+09   Mode  :character      Mode  :character      
    ##  Mean   :1.755e+09                                               
    ##  3rd Qu.:2.751e+09                                               
    ##  Max.   :4.083e+09                                               
    ##                                                                  
    ##  Tipo.de.actividad  DescripciÃ³n.de.la.actividad Tiempo.transcurrido
    ##  Length:882         Length:882                   Min.   :  205      
    ##  Class :character   Class :character             1st Qu.: 3500      
    ##  Mode  :character   Mode  :character             Median : 4634      
    ##                                                  Mean   : 5788      
    ##                                                  3rd Qu.: 6910      
    ##                                                  Max.   :70407      
    ##                                                                     
    ##    Distancia      Esfuerzo.relativo Viaje.al.trabajo   Equipo.de.la.actividad
    ##  Min.   :  0.00   Min.   :  1.0     Length:882         Mode:logical          
    ##  1st Qu.:  6.77   1st Qu.: 75.0     Class :character   NA's:882              
    ##  Median : 10.81   Median :130.0     Mode  :character                         
    ##  Mean   : 12.06   Mean   :148.4                                              
    ##  3rd Qu.: 15.61   3rd Qu.:195.0                                              
    ##  Max.   :147.00   Max.   :976.0                                              
    ##                   NA's   :217                                                
    ##  Nombre.de.archivo  Peso.del.atleta Peso.de.la.bicicleta Tiempo.transcurrido.1
    ##  Length:882         Min.   :68.00   Mode:logical         Min.   :  205        
    ##  Class :character   1st Qu.:68.00   NA's:882             1st Qu.: 3475        
    ##  Mode  :character   Median :71.00                        Median : 4630        
    ##                     Mean   :70.53                        Mean   : 5695        
    ##                     3rd Qu.:71.00                        3rd Qu.: 6796        
    ##                     Max.   :76.00                        Max.   :70407        
    ##                     NA's   :73                           NA's   :25           
    ##  Tiempo.en.movimiento
    ##  Min.   :  205       
    ##  1st Qu.: 3274       
    ##  Median : 4361       
    ##  Mean   : 5348       
    ##  3rd Qu.: 6368       
    ##  Max.   :56747       
    ## 

En act data hay mucha data

``` r
# analisis act data
```

Los archivos GPX son el estandar de la industria y la gran mayoria de
las aplicaciones lo soporta. Los archivos FIT son un formato mas nuevo,
y Strava lo comenzo a usar para comprimir las actividades provenientes
de otras App. La extension .gz corresponde a un archivo comprimido, por
lo que se deben descomprimir antes de cargar en R.

``` r
archivosGPX <- archivos[grep(".gpx",archivos)]

archivosfitgz <- archivos[grep(".fit.gz",archivos)]
```

Ambos tipos de archivos vienen en formatos diferentes, por lo que se
utilizan diferentes procedimientos para cargarlos. En el primer caso,
para abrir los archivos GPX utilizaremos la libreria plotKML, y
abriremos el primero de los archivos en la base.

Para efecto de la mayoria de los analisis de archivo GPX, la informacion
de interes se encuentra en la variable “tracks”

``` r
library(plotKML)

actividadGPX <- readGPX(paste0(path,"activities/",archivosGPX[3]))

gpx_stream <- (actividadGPX$tracks[[1]][[1]])

head(gpx_stream)
```

    ##         lon       lat   ele                 time
    ## 1 -70.55405 -33.38074 737.1 2015-03-13T22:42:58Z
    ## 2 -70.55390 -33.38072 737.2 2015-03-13T22:43:05Z
    ## 3 -70.55386 -33.38072 737.2 2015-03-13T22:43:15Z
    ## 4 -70.55385 -33.38070 737.3 2015-03-13T22:43:41Z
    ## 5 -70.55382 -33.38072 737.2 2015-03-13T22:44:01Z
    ## 6 -70.55385 -33.38073 737.2 2015-03-13T22:44:11Z

Los archivos con extension .fit.gz vienen comprimidos, por lo que
debemos extraer el archivo .fit y luego cargarlo a R. Para descomprimir
utilizamos la funcion gunzip, de la libreria R.utils. Esta funcion
descomprime el archivo en la carpeta original.

``` r
library(R.utils)

archivosfitgz[1]
```

    ## [1] "2509984347.fit.gz"

``` r
### remove = FALSE permite preservar el archivo .gz original
gunzip(paste0(path,"activities/",archivosfitgz[1]),remove=FALSE, overwrite=TRUE)

### vuelve a contar los archivos, aparece solo el que acabamos de descomprimir
archivos <- list.files(paste0(path,"activities"))

archivosfit <- archivos[grep(".fit$",archivos)]

archivosfit
```

    ## [1] "2509984347.fit"

Para cargarlos en R existen algunas librerias, algunas en CRAN, pero la
libreria FITfileR es la que tiene menores dependencias. El problema es
que no esta en CRAN, por lo que hay que instalarlas desde Github

``` r
## la funcion install_github esta en la libreria remotes, y tambien en devtools, ambas hacen lo mismo
remotes::install_github("grimbough/FITfileR")

library(FITfileR)

actividadfit <- readFitFile(paste0(path,"activities/",archivosfit[1]))

fit_stream <- records(actividadfit)

head(fit_stream)
```

    ## # A tibble: 6 x 9
    ##   timestamp           position_lat position_long distance altitude speed cadence
    ##   <dttm>                     <dbl>         <dbl>    <dbl>    <dbl> <dbl>   <int>
    ## 1 2019-05-11 00:01:22        -33.4         -70.6     0        941.  0          0
    ## 2 2019-05-11 00:01:26        -33.4         -70.6     1.17     939.  0          0
    ## 3 2019-05-11 00:01:32        -33.4         -70.6     8.87     938.  1.02       0
    ## 4 2019-05-11 00:01:38        -33.4         -70.6    24.1      937.  2.35      81
    ## 5 2019-05-11 00:01:39        -33.4         -70.6    26.6      937   2.39      80
    ## 6 2019-05-11 00:01:42        -33.4         -70.6    34.5      936   2.42      79
    ## # ... with 2 more variables: temperature <int>, fractional_cadence <dbl>

Tenemos 2 streams de GPS, provenientes de diferentes archivos. Para
procesar estos archivos debemos homologar los nombres, y por convencion
usaremos los nombres: long, lat, elev y time.

``` r
source("R/RenameStream.R")

fit_stream <- records(actividadfit)
gpx_stream <- (actividadGPX$tracks[[1]][[1]])


fit_stream <- RenameStream(fit_stream)

gpx_stream <- RenameStream(gpx_stream)

head(gpx_stream)
```

    ##        long       lat  elev time            timestamp
    ## 1 -70.55405 -33.38074 737.1    0 2015-03-13T22:42:58Z
    ## 2 -70.55390 -33.38072 737.2    7 2015-03-13T22:43:05Z
    ## 3 -70.55386 -33.38072 737.2   17 2015-03-13T22:43:15Z
    ## 4 -70.55385 -33.38070 737.3   43 2015-03-13T22:43:41Z
    ## 5 -70.55382 -33.38072 737.2   63 2015-03-13T22:44:01Z
    ## 6 -70.55385 -33.38073 737.2   73 2015-03-13T22:44:11Z

``` r
head(fit_stream)
```

    ##        long       lat  elev time           timestamp distance speed cadence
    ## 1 -70.56637 -33.36489 940.8    0 2019-05-11 00:01:22     0.00 0.000       0
    ## 2 -70.56638 -33.36489 939.4    4 2019-05-11 00:01:26     1.17 0.000       0
    ## 3 -70.56642 -33.36495 938.4   10 2019-05-11 00:01:32     8.87 1.017       0
    ## 4 -70.56643 -33.36509 937.4   16 2019-05-11 00:01:38    24.10 2.351      81
    ## 5 -70.56643 -33.36511 937.0   17 2019-05-11 00:01:39    26.65 2.389      80
    ## 6 -70.56645 -33.36518 936.0   20 2019-05-11 00:01:42    34.46 2.417      79
    ##   temperature fractional_cadence
    ## 1          19                  0
    ## 2          19                  0
    ## 3          19                  0
    ## 4          19                  0
    ## 5          19                  0
    ## 6          19                  0

Vamos a visualizar ambos trayectos en el mapa, utilizando Leaflet, que
es un framework de mapas abiertos. Para esto, debemos crear un objeto de
datos espaciales, llamados SpatialPoints, utilizando la libreria sp.
Para crear los objetos debemos pasar las coordenadas en orden longitud
latitud

``` r
library(leaflet)
library(sp)

## creo objeto espacial para ambos tracks por separado 
gpx_espacial <- SpatialPointsDataFrame(data=gpx_stream,  coords=gpx_stream[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))

fit_espacial <- SpatialPointsDataFrame(data=fit_stream,  coords=fit_stream[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))

### creo mapa de leaflet
mapa <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$OpenTopoMap) %>%
  addCircleMarkers(data = gpx_espacial, radius = 1, color = "blue") %>%
    addCircleMarkers(data = fit_espacial, radius = 1, color = "red")

mapa
```

![](Readme_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Por errores de medicion del GPS, que pueden deberse a diversos factores,
algunas trayectorias muestran espacios sin datos, y a veces presentan
comporamientos erraticos. Para corregir esto se aplicara una funcion de
suavizacion gaussiana, la cual puede regularse el ancho de suavizado, y
tambien permite interpolar valores entre mediciones.

``` r
source("R/SmoothCoords.R")

fit_smooth <- SmoothCoords(fit_stream)

gpx_smooth <- SmoothCoords(gpx_stream)
gpx_interpolado <- SmoothCoords(gpx_stream,interpolate = TRUE)


gpx_espacial_smooth <- SpatialPointsDataFrame(data=gpx_smooth,  coords=gpx_smooth[,6:7], proj4string=CRS("+proj=longlat +datum=WGS84"))

gpx_espacial_interpolado <- SpatialPointsDataFrame(data=gpx_interpolado,  coords=gpx_interpolado[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))


### creo mapa de leaflet con ruta interpolada versus ruta original y ruta suavizada
mapa2 <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$OpenTopoMap) %>%
  addCircleMarkers(data = gpx_espacial_interpolado, radius = 1, color = "green") %>%
  addCircleMarkers(data = gpx_espacial, radius = 1, color = "blue") %>%
  addCircleMarkers(data = gpx_espacial_smooth, radius = 1, color = "red")

mapa2
```

![](Readme_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Para poder analizar esta informacion, calculamos algunas metricas como
diferencias de distancia, tiempo y elevacion entre lecturas, y con eso
calculamos la velocidad, ritmo de carrera, pendiente y velocidad
vertical.

``` r
source("R/Derivatives.R")
library(ggplot2)

fit_interpolado <- SmoothCoords(fit_stream,interpolate = TRUE)

stream_fit_completo <- Derivatives(fit_interpolado)

fig1 <- ggplot(stream_fit_completo) + 
  geom_point(aes(x=hz_velocity,y=grade))

fig1
```

![](Readme_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

El analisis de velocidad horizontal versus pendiente muestra mucha
variabilidad, debido a que se realizo a nivel de lectura del
dispositivo, pero muchas veces el objeto de estudio son segmentos de una
distancia superior a una lectura. si separamos el track en segmentos de
100 metros (con pendiente equivalente), veremos que cambia el patron
observado en la figura anterior

``` r
source("R/SplitSegments.R")

segmentos <- SplitSegments(stream_fit_completo, value = "distance", size = 100)

fig1 + 
  geom_point(data=segmentos, aes(x=hz_velocity,y=grade), col= "red")
```

![](Readme_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
source("R/ElevationCorrection.R")

stream_fit_corregido14 <- ElevationCorrection(fit_stream, z=14)
stream_fit_corregido12 <- ElevationCorrection(fit_stream, z=12)
stream_fit_corregido10 <- ElevationCorrection(fit_stream, z=10)
stream_fit_corregido8 <- ElevationCorrection(fit_stream, z=8)
stream_fit_corregido6 <- ElevationCorrection(fit_stream, z=6)

ggplot(fit_stream) + 
  geom_line(aes(x=distance, y=elev)) +
  geom_line(data=stream_fit_corregido14, aes(x=distance, y = elev_DEM),col="red") +
  geom_line(data=stream_fit_corregido12, aes(x=distance, y = elev_DEM),col="orange") +
  geom_line(data=stream_fit_corregido10, aes(x=distance, y = elev_DEM),col="purple") +
  geom_line(data=stream_fit_corregido8, aes(x=distance, y = elev_DEM),col="blue") +
  geom_line(data=stream_fit_corregido6, aes(x=distance, y = elev_DEM),col="green") +
  theme_minimal()
```

![](Readme_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
