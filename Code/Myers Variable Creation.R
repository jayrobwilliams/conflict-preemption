###############################
## author: Rob Williams      ##
## project: dissertation     ##
## created: February 4, 2018 ##
## updated: Janurary 2, 2019 ##
###############################

## this script measures nightlights in regions with Myers numbers available and
## produces the correlation between the two reported on p. 14 (accompanied by
## footnote 5)

## print script to identify in log
print(paste('Myers Nightlights Comparison Started', Sys.time()))

## load packages
library(tidyverse)
library(RWmisc)
library(sf)
library(raster)

import::from('data.table', '%between%')

source(here::here('Code/sfFunctions.R'))
source(here::here('Code/cshapes Recode.R'))

library(doParallel)
## get number of cores from SLURM submission script
registerDoParallel(as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')))

## load data
myers <- rio::import(here::here('Datasets/Myers Numbers/subnational_myers.dta'))

## read in cshapes
cshapes <- st_read(here::here('Datasets/cshapes/cshapes_0.6'), 'cshapes')

## read in 2nd order GADM
gadm <- st_read('Datasets/gadm/gadm34_levels.gpkg',
                layer = 'LEVEL1')

## read in population rasters
population_cnt <- stack(list.files(here::here('Datasets/Population',
                                              'Count Interpolated'),
                                   '.tif', full.names = T))

## read in nightlights rasters
nightlights <- stack(list.files(here::here('Datasets/Nightlights/Output'), '.tif',
                                full.names = T))

## recode start and end dates based on 6 months of a year rule
cshapes <- cshapes.rc(cshapes)

## create object of capitals
capitals <- st_as_sf(st_drop_geometry(cshapes), coords = c('CAPLONG', 'CAPLAT'),
                     crs = st_crs(cshapes), agr = 'constant')

## filter Myers numbers to nightlights coverage
myers <- myers %>% filter(year >= 1992 & year <= 2013)

## join Myers numbers to GADM and add GW codes
myers <- left_join(myers, gadm,
                   by = c('countryname' = 'NAME_0', 'regionname' = 'NAME_1')) %>% 
  democracyData::country_year_coder(country_col = countryname,
                                    code_col = GID_0, date_col = year,
                                    code_type = 'iso3c', to_system = 'GW',
                                    include_in_output = 'GWn') %>%
  st_as_sf()

## extract nightlights for GADM regions with Myers numbers
group_data <- foreach(i = 1:nrow(myers),
                      .packages = c('sf', 'sp', 'raster', 'rgeos', 'dplyr',
                                    'data.table'),
                      .combine = rbind, .errorhandling = 'remove') %dopar% {
  
  ## get group-year i
  adm1 <- myers[i, ]
  
  ## get polygons for group-year i's state
  state_poly <- cshapes[cshapes$GWCODE == adm1$GWn &
                          adm1$year %between% st_drop_geometry(cshapes[, c('GWSYEAR', 'GWEYEAR')]), ]
  
  ## get polygon for administrative unit-year i
  terr <- gadm[gadm$GID_1 == adm1$GID_1, ]
  
  ## get point for group-year i's capital
  capital <- capitals[as.character(cshapes$GWCODE) == as.character(adm1$GWn) &
                        adm1$year %between% st_drop_geometry(capitals[, c('GWSYEAR', 'GWEYEAR')]), ]
  
  ## get population for group-year i; 1989 b/c 1 indexing
  pop_cnt_adm1 <- crop(population_cnt[[adm1$year - 1989]], adm1)
  
  ## get nightlights for group-year i; use 1992 data for 1990 and 1991; not ideal but
  ## still better than just using nightlights for only one year; 1991 b/c 1 indexing
  nl_adm1 <- crop(nightlights[[max(adm1$year - 1991, 1)]], adm1)
  
  ## mask population and nightlights for inequality calculation
  pop_cnt_adm1 <- mask(pop_cnt_adm1, adm1)
  
  ## calculate total population
  pop_adm1_tot <- cellStats(pop_cnt_adm1, 'sum')
  
  ## mask nightlights for aggregations
  nl_adm1 <- mask(nl_adm1, adm1)
  
  ## calculate total nightlights
  nl_adm1_tot <- cellStats(nl_adm1, 'sum')
  
  ## calculate area of group territory
  area_terr <- st_area(terr)[[1]] / 1e6
  
  ## calculate distance from territory centroid to capital in km
  cap_dist <- st_distance(st_centroid(terr), capital)[[1]] / 1e3
  
  ## return all measures for concatenation by foreach
  data.frame(COWcode = adm1$ccode, gwid = adm1$GWn,
             year = adm1$year, country = adm1$countryname,
             region = adm1$regionname, myers = adm1$myers,
             nl = nl_adm1_tot, pop = pop_adm1_tot,
             area = area_terr, cap_dist)
  
}

## print script to verify successful execution in log
print(paste('Myers Nightlights Comparison Completed', Sys.time()))

## calculate correlation b/w Myers numbers and nightlights
cor(log1p(group_data$nl), log(group_data$myers), use = 'complete.obs')

## quit R
quit(save = 'no')

###################
## end of script ##
###################