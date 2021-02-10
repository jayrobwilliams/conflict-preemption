#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: dissertation             ##
## created: July 31, 2018            ##
## updated: July 31, 2018            ##
#######################################

## this script extracts the spatial components of the territorial governability
## measure from each ethnic group-year polygon, including population, nightlights,
## travel times, and various statistics calculated based on them


## naga in myanmar have gwgroupid for naga in india; need to check if script is
## pulling the indian or myanmar polygon for these observations


## print script to identify in log
print(paste('Map Creation Started', Sys.time()))

## load packages
library(sf) # new unified spatial package
library(sp) # basic spatial data handling
library(raster) # pixel based data
library(rgdal) # spatial data I/O
library(rgeos) # spatial topology operations
library(dplyr)
library(spdplyr)
library(stringr)
library(data.table)

source(here::here('Code/sfFunctions.R'))
source(here::here('Code/cshapes Recode.R'))




## read in data ####

## read in cshapes
cshapes <- st_read(here::here('Datasets/cshapes/cshapes_0.6'), 'cshapes')

## read in GADM
GADM <- st_read(dsn = here::here('Datasets/gadm/gadm34_levels_gpkg/gadm34_levels.gpkg'),
                layer = 'level1')

## read in GeoEPR
GeoEPR <- st_read(here::here('Datasets/EPR'), 'GeoEPR-2014 Cleaned')

## read in EPR
EPR <- read.csv(here::here('Datasets/EPR/EPR-2014.csv'))

## read in population rasters
population_cnt <- stack(list.files(here::here('Datasets/Population',
                                              'Count Corrected'),
                                   '.tif', full.names = T))

## read in nightlights rasters
nightlights <- stack(list.files(here::here('Datasets/Nightlights/Corrected'),
                                '.tif', full.names = T))

## spatial pre-processing ####

## recode start and end dates based on 6 months of a year rule
cshapes <- cshapes.rc(cshapes)

## assign WGS84 CRS to GeoEPR
GeoEPR <- st_transform(GeoEPR, st_crs(cshapes))

## create object of capitals
capitals <- st_as_sf(st_drop_geometry(cshapes), coords = c('CAPLONG', 'CAPLAT'),
                     crs = st_crs(cshapes), agr = 'constant')



## data preprocessing ####

## get China polygon
china <- cshapes %>% filter(CNTRY_NAME == 'China', GWSYEAR >= 1949)

## get xinjiang polygon
xinjiang <- GADM %>% filter(NAME_1 == 'Xinjiang Uygur')

## get Beijing point
beijing <- capitals %>% filter(CAPNAME == 'Beijing', GWSYEAR >= 1949)

## get Uyghur polygon
uyghur <- GeoEPR %>% filter(group == 'Uyghur')

## crop and mask nightlights
nl_china <- crop(nightlights[[22]], china)
nl_china <- mask(nl_china, china)

## crop and mask population
pop_china <- crop(population_cnt, china)
pop_china <- mask(pop_china, china)

## plot population
pdf(here::here('Figures/china_pop.pdf'), width = 8, height = 8)
plot(log1p(pop_china), axes = F, box = F, legend = F)
plot(xinjiang$geom, add = T, border = 'gray65', lty = 2)
plot(beijing$geometry, add = T, pch = 18, cex = 2.5, col = 'dodgerblue')
dev.off()

## plot nightlights
pdf(here::here('Figures/china_nl.pdf'), width = 8, height = 8)
plot(nl_china, col = gray.colors(10, start = 0.1, end = 1, gamma = 2.2, alpha = NULL),
     axes = F, box = F, legend = F)
plot(xinjiang$geom, add = T, border = 'gray65', lty = 2)
plot(beijing$geometry, add = T, pch = 18, cex = 2.5, col = 'dodgerblue')
dev.off()



## quit script
quit(save = 'no')

###################
## End of Script ##
###################