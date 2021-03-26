## this script clips off a few problematic sub-polygons from ethnic group
## polygons that straddle the antimeridian and throw things off by stretching
## across the entire 360 degrees of longitude

## print script to identify in log
print(paste('GeoEPR Cleaning Started', Sys.time()))

## load packages
library(sp) # basic spatial data handling
library(rgdal) # spatial data I/O
library(rgeos) # spatial topology operations
library(dplyr)

## data temporal range
data_start <- 1946
data_end <- 2013

## read in GeoEPR
GeoEPR <- readOGR(here::here('Datasets/EPR'), 'GeoEPR-2014')

## subset GeoEPR to polygons that end after start of sample and recode
## from <= 1990 to 1990 and to >= 2013 to 2013
GeoEPR <- GeoEPR %>%
  filter(to >= data_start & to <= data_end) %>% 
  mutate(from = ifelse(from <= data_start, data_start, from),
         to = ifelse(to >= data_end, data_end, to))



## polygon removal for whites in United States
territory <- GeoEPR %>% filter(group == 'Whites' & statename == 'United States')

## extract list of polygons in Polygons object
territory_pols <- slot(territory, 'polygons')[[1]]
territory_pol <- slot(territory_pols, 'Polygons')

## subset list of polygons by whether they cross antimeridian
res <- territory_pol[sapply(territory_pol, function(x) {x@labpt[1]}) > -130]

## replace polygons in territory object
slot(territory_pols, "Polygons") <- res
slot(territory, 'polygons')[[1]] <- territory_pols

## get territory object again
territory_orig <- GeoEPR %>% filter(group == 'Whites' & statename == 'United States') 

## intersect original territory polygon with reduced one
territory_orig_int <- gIntersection(territory_orig, territory, byid=c(TRUE, FALSE))

## rename polygon ID so that data can be added back on
territory_orig_int@polygons[[1]]@ID <- '1'

## add data to convert back to spatialpolygonsdataframe
territory <- SpatialPolygonsDataFrame(territory_orig_int, territory_orig@data, match.ID = F)

## assign territory to temporary object
temp <- territory



## polygon removal for american indians in United States
territory <- GeoEPR %>% filter(group == 'American Indians' & statename == 'United States')

## extract list of polygons in Polygons object
territory_pols <- slot(territory, 'polygons')[[1]]
territory_pol <- slot(territory_pols, 'Polygons')

## subset list of polygons by whether they cross antimeridian
res <- territory_pol[sapply(territory_pol, function(x) {x@labpt[1]}) > -130]

## replace polygons in territory object
slot(territory_pols, "Polygons") <- res
slot(territory, 'polygons')[[1]] <- territory_pols

## get territory object again
territory_orig <- GeoEPR %>% filter(group == 'American Indians' & statename == 'United States') 

## intersect original territory polygon with reduced one
territory_orig_int <- gIntersection(territory_orig, territory, byid=c(TRUE, FALSE))

## rename polygon ID so that data can be added back on
territory_orig_int@polygons[[1]]@ID <- '1'

## add data to convert back to spatialpolygonsdataframe
territory <- SpatialPolygonsDataFrame(territory_orig_int, territory_orig@data, match.ID = F)

## assign territory to temporary object
temp <- rbind(temp, territory)



## polygon removal for russians in russia 1990-1991
territory <- GeoEPR %>% filter(group == 'Russians' & statename == 'Russia' & from < 1990)

## extract list of polygons in Polygons object
territory_pols <- slot(territory, 'polygons')[[1]]
territory_pol <- slot(territory_pols, 'Polygons')

## subset list of polygons by whether they cross antimeridian
res <- territory_pol[sapply(territory_pol, function(x) {x@labpt[1]}) > 20]

## replace polygons in territory object
slot(territory_pols, "Polygons") <- res
slot(territory, 'polygons')[[1]] <- territory_pols

## get territory object again
territory_orig <- GeoEPR %>% filter(group == 'Russians' & statename == 'Russia' & from < 1990)

## intersect original territory polygon with reduced one
territory_orig_int <- gIntersection(territory_orig, territory, byid=c(TRUE, FALSE))

## rename polygon ID so that data can be added back on
territory_orig_int@polygons[[1]]@ID <- '1'

## add data to convert back to spatialpolygonsdataframe
territory <- SpatialPolygonsDataFrame(territory_orig_int, territory_orig@data, match.ID = F)

## concatenate territory to temporary object
temp <- rbind(temp, territory)




## polygon removal for russians in russia 1992-2013
territory <- GeoEPR %>% filter(group == 'Russians' & statename == 'Russia' & from == 1992)

## extract list of polygons in Polygons object
territory_pols <- slot(territory, 'polygons')[[1]]
territory_pol <- slot(territory_pols, 'Polygons')

## subset list of polygons by whether they cross antimeridian
res <- territory_pol[sapply(territory_pol, function(x) {x@labpt[1]}) > 20]

## replace polygons in territory object
slot(territory_pols, "Polygons") <- res
slot(territory, 'polygons')[[1]] <- territory_pols

## get territory object again
territory_orig <- GeoEPR %>% filter(group == 'Russians' & statename == 'Russia' & from == 1992) 

## intersect original territory polygon with reduced one
territory_orig_int <- gIntersection(territory_orig, territory, byid=c(TRUE, FALSE))

## rename polygon ID so that data can be added back on
territory_orig_int@polygons[[1]]@ID <- '1'

## add data to convert back to spatialpolygonsdataframe
territory <- SpatialPolygonsDataFrame(territory_orig_int, territory_orig@data, match.ID = F)

## concatenate territory to temporary object
temp <- rbind(temp, territory)




## polygon removal for chukchi in russia 1992-2013
territory <- GeoEPR %>% filter(group == 'Chukchi' & statename == 'Russia' & to == 2013)

## extract list of polygons in Polygons object
territory_pols <- slot(territory, 'polygons')[[1]]
territory_pol <- slot(territory_pols, 'Polygons')

## subset list of polygons by whether they cross antimeridian
res <- territory_pol[sapply(territory_pol, function(x) {x@labpt[1]}) > 20]

## replace polygons in territory object
slot(territory_pols, "Polygons") <- res
slot(territory, 'polygons')[[1]] <- territory_pols

## get territory object again
territory_orig <- GeoEPR %>% filter(group == 'Chukchi' & statename == 'Russia' & to == 2013)

## intersect original territory polygon with reduced one
territory_orig_int <- gIntersection(territory_orig, territory, byid=c(TRUE, FALSE))

## rename polygon ID so that data can be added back on
territory_orig_int@polygons[[1]]@ID <- '1'

## add data to convert back to spatialpolygonsdataframe
territory <- SpatialPolygonsDataFrame(territory_orig_int, territory_orig@data, match.ID = F)

## concatenate territory to temporary object
temp <- rbind(temp, territory)



## polygon removal for fijians in fiji
territory <- GeoEPR %>% filter(group == 'Fijians' & statename == 'Fiji')

## extract list of polygons in Polygons object
territory_pols <- slot(territory, 'polygons')[[1]]
territory_pol <- slot(territory_pols, 'Polygons')

## subset list of polygons by whether they cross antimeridian
res <- territory_pol[sapply(territory_pol, function(x) {x@labpt[1]}) > 0]

## replace polygons in territory object
slot(territory_pols, "Polygons") <- res
slot(territory, 'polygons')[[1]] <- territory_pols

## get territory object again
territory_orig <- GeoEPR %>% filter(group == 'Fijians' & statename == 'Fiji')

## intersect original territory polygon with reduced one
territory_orig_int <- gIntersection(territory_orig, territory, byid=c(TRUE, FALSE))

## rename polygon ID so that data can be added back on
territory_orig_int@polygons[[1]]@ID <- '1'

## add data to convert back to spatialpolygonsdataframe
territory <- SpatialPolygonsDataFrame(territory_orig_int, territory_orig@data, match.ID = F)

## concatenate territory to temporary object
temp <- rbind(temp, territory)




## drop observations in temporary object from GeoEPR
GeoEPR_temp <- GeoEPR %>% filter(!(gwgroupid %in% temp$gwgroupid))

## concatenate temporary object to GeoEPR
GeoEPR_temp <- rbind(GeoEPR_temp, temp)

## sort by gwgroupid
GeoEPR_temp <- GeoEPR_temp[order(GeoEPR_temp$gwgroupid), ]

## write GeoEPR for use by later scripts
writeOGR(GeoEPR_temp, here::here('Datasets/EPR'), 'GeoEPR-2014 Cleaned',
         driver = 'ESRI Shapefile', overwrite_layer = T)



## print script to verify successful execution in log
print(paste('GeoEPR Cleaning Completed', Sys.time()))

## quit R
quit(save = 'no')

###################
## End of Script ##
###################