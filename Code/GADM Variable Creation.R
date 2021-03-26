#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: dissertation             ##
## created: December 11, 2017        ##
## updated: June 6, 2018             ##
#######################################

## this script extracts the spatial components of the territorial governability
## measure from each ethnic group-year polygon, including population, nightlights,
## travel times, and various statistics calculated based on them


## naga in myanmar have gwgroupid for naga in india; need to check if script is
## pulling the indian or myanmar polygon for these observations


## print script to identify in log
print(paste('GADM Variable Creation Started', Sys.time()))

## clear environment
rm(list = ls())

## set working directory
setwd("~/Dropbox/UNC/Dissertation/Onset")

## load packages
library(sf) # new unified spatial package
library(raster) # pixel based data
library(rgdal) # spatial data I/O
library(rgeos) # spatial topology operations
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

source(here::here('Code/sfFunctions.R'))
source(here::here('Code/cshapes Recode.R'))

library(doParallel)
registerDoParallel(as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')))



## read in data ####

## read in cshapes
cshapes <- st_read(here::here('Datasets/cshapes/cshapes_0.6'), 'cshapes')

## read in GADM
GADM <- st_read(dsn = here::here('Datasets/gadm/gadm34_levels.gpkg'),
                layer = 'level1') %>% select(NAME_0, GID_0, NAME_1, GID_1)

## read in population rasters
population_cnt <- stack(list.files(here::here('Datasets/Population',
                                              'Interpolated'),
                                    '.tif', full.names = T))

## read in nightlights rasters
nightlights <- stack(list.files(here::here('Datasets/Nightlights/Output'),
                                '.tif', full.names = T))



## spatial pre-processing ####

## recode start and end dates based on 6 months of a year rule
cshapes <- cshapes.rc(cshapes)

## create object of capitals
capitals <- st_as_sf(st_drop_geometry(cshapes), coords = c('CAPLONG', 'CAPLAT'),
                     crs = st_crs(cshapes), agr = 'constant')



## data preprocessing ####

## temporal range
data_start <- 1990
data_end <- 2013

## expand GADM to one row per administrative unit-year and code country
GADM_df <- GADM %>%
  expand(GID_1, year = 1990:2013) %>%
  mutate(GID_0 = gsub('\\..*$', '', GID_1)) %>% 
  left_join(st_drop_geometry(GADM), by = c('GID_0', 'GID_1')) %>% 
  democracyData::country_year_coder(country_col = NAME_0,
                                    code_col = GID_0, date_col = year,
                                    code_type = 'iso3c', to_system = 'GW',
                                    include_in_output = 'GWn') %>%
  filter(year >= 1992, !is.na(GWn))



## onset related variables
gadm_data <- foreach(i = 1:nrow(GADM_df), # replace w/ nrow(GeoEPR_df) after figuring out
                     .packages = c('sf', 'sp', 'raster', 'rgeos', 'ineq', 'dplyr',
                                  'data.table'),
                     .combine = rbind, .errorhandling = 'remove') %dopar% {
  
  ## get administrative unit-year i
  adm <- GADM_df[i, ]
  
  ## get polygons for group-year i's state
  state_poly <- cshapes[cshapes$GWCODE == adm$GWn &
                          adm$year %between% st_drop_geometry(cshapes[, c('GWSYEAR', 'GWEYEAR')]), ]
  
  ## get polygon for administrative unit-year i
  terr <- GADM[GADM$GID_1 == adm$GID_1, ]
  
  ## get point for group-year i's capital
  capital <- capitals[as.character(cshapes$CNTRY_NAME) == as.character(adm$NAME_0) &
                        adm$year %between% st_drop_geometry(capitals[, c('GWSYEAR', 'GWEYEAR')]), ]
  
  ## get population for group-year i; 1989 b/c 1 indexing
  pop_cnt_terr <- crop(population_cnt[[adm$year - 1989]], terr)
  
  ## get nightlights for group-year i; use 1992 data for 1990 and 1991; not ideal but
  ## still better than just using nightlights for only one year; 1991 b/c 1 indexing
  nl_terr <- crop(nightlights[[max(adm$year - 1991, 1)]], terr)
  
  ## mask population and nightlights for inequality calculation
  pop_cnt_terr <- mask(pop_cnt_terr, terr)
  nl_terr <- mask(nl_terr, terr)
  
  ## calculate total population
  pop_terr_tot <- cellStats(pop_cnt_terr, 'sum')
  
  ## calculate total nightlights
  nl_terr_tot <- cellStats(nl_terr, 'sum')
  
  ## if no nightlights for group-year i, recode to 1 to preserve inequality measure
  nl_terr_tot <- ifelse(nl_terr_tot == 0, 1, nl_terr_tot)
  
  ## project territory and rasters
  terr <- projectUTM(terr)
  state_poly <- st_transform(state_poly, st_crs(terr))
  capital <- st_transform(capital, st_crs(terr))
  pop_cnt_terr <- projectRaster(pop_cnt_terr, crs = CRS(st_crs(terr)$proj4string))
  nl_terr <- projectRaster(nl_terr, crs = CRS(st_crs(terr)$proj4string))
  
  ## redraw polygons w/ GEOS, fixing topology errors
  terr <- st_simplify(terr, preserveTopology = T, dTolerance = 0)
  state_poly <- st_simplify(state_poly, preserveTopology = T, dTolerance = 0)
  terr <- st_buffer(terr, dist = 0)
  state_poly <- st_buffer(state_poly, dist = 0)
  
  ## check whether territory abuts an international border by checking whether
  ## it is fully covered by the state's polygon buffered 1km inward
  border <- !st_within(terr, st_buffer(state_poly, -1e3), sparse = F)[[1]]
  
  ## calculate area of group territory
  area_terr <- units::drop_units(units::set_units(st_area(terr), 'km^2'))
  
  ## calculate distance from territory centroid to capital in km
  cap_dist <- units::drop_units(units::set_units(st_distance(st_centroid(terr),
                                                             capital), 'km'))
  
  ## return all measures for concatenation by foreach
  data.frame(id = adm$GID_1, COWcode = state_poly$COWCODE,
             gwid = state_poly$GWCODE, year = adm$year, state = adm$NAME_0,
             region = adm$NAME_1, pop_tot = pop_terr_tot, nl = nl_terr_tot,
             area = area_terr, border = border, cap_dist = cap_dist)
  
}

## save group data
saveRDS(gadm_data, here::here('Input Data/gadm data.RDS'))



## print script to verify successful execution in log
print(paste('GADM Variable Creation Completed', Sys.time()))

## quit R
quit(save = 'no')

###################
## End of Script ##
###################