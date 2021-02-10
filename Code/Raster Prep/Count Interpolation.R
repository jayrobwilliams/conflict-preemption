################################
## author: Rob Williams       ##
## project: dissertation      ##
## created: February 27, 2018 ##
## updated: December 11, 2018 ##
################################

## this script uses a linear temporal interpolation to interpolate missing
## population values between quinquennial population estimates.



## print script to identify in log
print(paste('Population Count Interpolation Started', Sys.time()))

## load packages
library(sp) # basic spatial data handling
library(raster) # pixel based data
library(rgdal) # spatial data I/O
library(rgeos) # spatial topology operations

## linear temporal interpolation of raster values
## from https://gist.github.com/johnbaums/10465462
source(here::here('Code/lintemp.R'))



## read in data ####

## read in cshapes
cshapes <- readOGR(here::here('Datasets/cshapes/cshapes_0.6'), 'cshapes')

## read in quinquennial population data
pop1990 <- raster(here::here('Datasets/Population/glcount90/glp90ag'))
pop1995 <- raster(here::here('Datasets/Population/glcount95/glp95ag'))
pop2000 <- raster(here::here('Datasets/Population',
                             'gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev10_2000_30_sec_tif',
                             'gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev10_2000_30_sec.tif'))
pop2005 <- raster(here::here('Datasets/Population',
                             'gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev10_2005_30_sec_tif',
                             'gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev10_2005_30_sec.tif'))
pop2010 <- raster(here::here('Datasets/Population',
                             'gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev10_2010_30_sec_tif',
                             'gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev10_2010_30_sec.tif'))
pop2015 <- raster(here::here('Datasets/Population',
                             'gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev10_2015_30_sec_tif',
                             'gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev10_2015_30_sec.tif'))



## spatial preprocessing ####

## extract WGS84 coordinate reference system object from ged
WGS84 <- CRS(proj4string(cshapes))

## convert population data to came CRS as cshapes
proj4string(pop1990) <- WGS84
proj4string(pop1995) <- WGS84
proj4string(pop2000) <- WGS84
proj4string(pop2005) <- WGS84
proj4string(pop2010) <- WGS84
proj4string(pop2015) <- WGS84

## create raster directory in working directory to hold results of crop operations. this is
## necessary so that the files can be explicitly referenced for combining them into a 
## rasterbrick, which can't be done if they're in a random temp directory
dir.create(here::here('Processed/Raster'), recursive = T)

## clip bottom 2 degrees off GPWv4 rasters (2000 and up) so they're same extent as earlier rasters
pop2000 <- crop(pop2000, pop1990,
                filename = here::here('Processed/Raster/pop2000'),
                format = 'GTiff')
pop2005 <- crop(pop2005, pop1990,
                filename = here::here('Processed/Raster/pop2005'),
                format = 'GTiff')
pop2010 <- crop(pop2010, pop1990,
                filename = here::here('Processed/Raster/pop2010'),
                format = 'GTiff')
pop2015 <- crop(pop2015, pop1990,
                filename = here::here('Processed/Raster/pop2015'),
                format = 'GTiff')

## resample GPWv3 data to same resolution as GPWv4 data to allow interpolation across date range
pop1990 <- resample(pop1990, pop2000,
                    filename = here::here('Processed/Raster/pop2000'),
                    format = 'GTiff', overwrite = T)
pop1995 <- resample(pop1995, pop2000,
                    filename = here::here('Processed/Raster/pop2005'),
                    format = 'GTiff', overwrite = T)

## combine 5 year population measures into raster stack
population <- stack(pop1990, pop1995, pop2000, pop2005, pop2010, pop2015)

## vectors of observed and unobserved years. treat 1990 population data as 1989 data because we 
## have cases in sample from 1989 and function doesn't do backwards interpolation. may be able to 
## get backwards interpolation if there is some form of spatio-temporal kriging we could use (if 
## there is, apply before cropping data to sample countries sothere is more information included 
## in calculations)
pop_observed <- c(1990, 1995, 2000, 2005, 2010, 2015) 
pop_predict <- seq(1990, 2015, by = 1)

## create output directory for interpolated rasters
dir.create(here::here('Datasets/Population/Count Interpolated'),
           showWarnings = F)

## linear temporal interpolation of population -- create from scratch
population <- interpolateTemporal(population, xin = pop_observed,
                                  xout = pop_predict,
                                  outdir = here::here('Datasets/Population',
                                                      'Count Interpolated'),
                                  prefix = 'pop_cnt', writechange = F,
                                  returnstack = T, overwrite = T)

## remove files generated by cropping as these are now duplicated by interpolation, only works on *nix systems
unlink(here::here('Processed'), recursive = T)

## print script to verify successful execution in log
print(paste('Population Count Interpolation Completed', Sys.time()))

## quit R
quit(save = 'no')

###################
## End of Script ##
###################