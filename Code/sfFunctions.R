#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: misc R functions         ##
## created: July 9, 2017             ##
## updated: September 13, 2017       ##
#######################################



##
##
##
##


## load required packages
require(sf)

## downgrade a simplefeature geometry object to a dataframe
st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}


## determine which UTM zone a longitude value falls in
long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

## determine which UTM zone(s) a vector of longitudes falls in
UTMzones <- function(long) {
  unique(long2UTM(long))
}

## choose a UTM zone based on on the average of UTM zones of a vector of points
chooseUTM <- function(long) {
  zone <- round(mean(long2UTM(long)))
  zone
}

## !!!! this is a bigass hammer to drive a small nail. picking a single UTM zone for features
## that lie in many zones results in distortion of distance estimates. this is especially
## problematic in large countries like the US or China that span several UTM zones and in
## areas far from the equator

## determine which UTM zone the majority of a spatial object falls within and project the
## object using that UTM zone.
projectUTM <- function(sf.object) {
  
  ## determine if spatial object is a SpatialPoints or SpatialPolygons object and use either
  ## the coords or polygons slot to access the longitude(s) of the spatial object
  if (attr(st_geometry(sf.object), 'class')[1] == 'sfc_POINT') {
    
    ## find average UTM zone using longitude(s) of SpatialPoints object
    zone <- chooseUTM(st_coordinates(sf.object)[1])
    
    ## save latitude mean to determine if features falls in southern hemisphere
    lat.mean <- mean(st_coordinates(sf.object)[2])
    
  }
  
  if (attr(st_geometry(sf.object), 'class')[1] %in% c('sfc_POLYGON', 'sfc_MULTIPOLYGON')) {
    
    ## find average UTM zone using longitude(s) of SpatialPolygons object
    zone <- chooseUTM(mean(st_coordinates(sf.object)[, 1]))
    
    ## save latitude mean to determine if features falls in southern hemisphere
    lat.mean <- mean(st_coordinates(sf.object)[, 2])
    
  }
  
  ## if average of latitude values is negative, add +south the coordinate reference system
  if (lat.mean >= 0) {
    
    ## create coordinate reference system object to project spatial object
    zone <- st_crs(paste('+proj=utm +zone=', zone, sep = ''))
    
  } else {
    
    ## create coordinate reference system object to project spatial object
    zone <- st_crs(paste('+proj=utm +south +zone=', zone, sep = ''))
    
  }
  
  ## project spatial object
  sf.object <- st_transform(sf.object, zone)
  
  ## return projected spatial object
  sf.object
  
}

## calculate the maximum possible distance from a point to the edge of a given polygon
max.polydist <- function(poly, point) {
  
  require(ggplot2)

  ## project point using polygon CRS
  point <- st_transform(point, st_crs(poly))

  ## check to ensure polygon and point have same CRS
  if (st_crs(poly) != st_crs(point)) stop('polygon and point do not share same CRS')
  
  ## extract border vertices and point coordinates
  border <- st_coordinates(poly)[, 1:2]
  point <- st_coordinates(point)
  
  ## extract eastings and northings of all border vertices
  longs <- border[, 1]
  lats <- border[, 2]
  
  ## euclidean distance
  dist_fx <- function(long, lat, cap = point) {
    
    return(sqrt((long - cap[, 1])^2 + (lat - cap[, 2])^2))
    
  }
  
  ## calculate distance from point to every border vertex
  dists <- mapply(dist_fx, longs, lats, MoreArgs = list(point))
  
  ## return maximum distance from point to border
  return(max(dists))
  
}



###################
## end of script ##
###################