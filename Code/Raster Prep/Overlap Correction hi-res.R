## this script weights raster files by the number of ethnic group polygons that
## overlap each cell in the raster. this is the high resolution version for
## nightlights rasters. a cell covered by one group polygon in a given year is
## unchanged, while a cell covered by two polygons in a given year is
## multiplied by .5

## print script to identify in log
print(paste('High Resolution Overlap Correction Started', Sys.time()))

## load packages
library(sf) # new unified spatial package
library(raster) # pixel based data
library(dplyr)
library(data.table)

## register parallel backend
library(doParallel)

## get number of cores from SLURM submission script
registerDoParallel(as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')))

## create output directories for corrected rasters
dir.create(here::here('Datasets/Nightlights/Corrected'), showWarnings = F)



## read in data ####

## read in GeoEPR
GeoEPR <- st_read(here::here('Datasets/EPR'), 'GeoEPR-2014 Cleaned')

## read in nightlights rasters
nightlights <- stack(list.files(here::here('Datasets/Nightlights/Output'), '.tif',
                                full.names = T))



## iterate through raster layers, calculating overlapping polygons in cells, and
## then weighting cell values
foreach (i = 1:dim(nightlights)[3], .packages = c('sf', 'raster')) %dopar% {
  
  ## get raster i from raster stack
  rast <- nightlights[[i]]
  
  ## get year for raster i
  year <- as.numeric(sub('nl_', '', names(rast)))
  
  ## subset GeoEPR to polygons in existence in year i
  GeoEPR_contemp <- GeoEPR %>% filter(year %between% GeoEPR[, c('from', 'to')])
  
  ## create list to hold rasters
  rasts <- list()
  
  ## loop through polygons in existence in year i
  for (j in 1:nrow(GeoEPR_contemp)) {
    
    ## get polygon j
    polygon <- GeoEPR_contemp[j,]
    
    ## crop raster to polygon
    rast_poly <- crop(rast, polygon)
    
    ## set all cell values to 1 to represent polygon presence
    rast_poly[] <- 1
    
    ## mask out cells outside polygon
    rast_poly <- mask(rast_poly, polygon)
    
    ## append to list
    rasts[[j]] <- rast_poly
    
  }
  
  ## set function to mosaic rasters
  rasts$fun <- sum
  
  ## mosaic rasters, summing overlapping cells
  raster_poly_count <- do.call(mosaic, rasts)
  
  ## then divide by polygon count. this leaves raster cells not covered by any
  ## group with their default value, also leaves cells covered by only one group
  ## with their default value, and divides cells covered by more than one group
  ## by the number of groups. this divides the value of the cell evenly between
  ## groups when cell values are aggregated to the group level.
  raster_poly_count <- 1 / raster_poly_count
  
  ## multiply rasters by weights and save output
  writeRaster(nightlights[[i]] * raster_poly_count,
              filename = here::here('Datasets/Nightlights/Corrected',
                                    paste0('nl_', year)),
              format = 'GTiff', overwrite = T)
  
  rm(raster_poly_count, rasts, rast, rast_poly, GeoEPR_contemp, year)
  
}



## print script to verify successful execution in log
print(paste('High Resolution Overlap Correction Completed', Sys.time()))

## quit R
quit(save = 'no')

###################
## End of Script ##
###################