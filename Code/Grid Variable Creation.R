#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: dissertation             ##
## created: December 11, 2017        ##
## updated: December 30, 2018        ##
#######################################

## this script extracts the spatial components of the territorial governability
## measure from each ethnic group-year polygon, including population, nightlights,
## travel times, and various statistics calculated based on them


## naga in myanmar have gwgroupid for naga in india; need to check if script is
## pulling the indian or myanmar polygon for these observations


## print script to identify in log
print(paste('Grid Variable Creation Started', Sys.time()))

## clear environment
rm(list = ls())

## set working directory
setwd("~/Dropbox/UNC/Dissertation/Onset")

## load packages
library(sf) # new unified spatial package
library(sp) # basic spatial data handling
library(raster) # pixel based data
library(rgdal) # spatial data I/O
library(rgeos) # spatial topology operations
library(tidyverse)
library(spdplyr)
library(stringr)
library(data.table)

source(here::here('Code/sfFunctions.R'))
source(here::here('Code/cshapes Recode.R'))

library(doParallel)
## get number of cores from SLURM submission script
registerDoParallel(as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')))



## read in data ####

## read in cshapes
cshapes <- st_read(here::here('Datasets/cshapes/cshapes_0.6'), 'cshapes')

## read in population rasters
population_cnt <- stack(here::here(list.files('Datasets/Population',
                                              'Count Interpolated'),
                                    '.tif', full.names = T))

## read in nightlights rasters
nightlights <- stack(list.files(here::here('Datasets/Nightlights',
                                           'Output'), '.tif',
                                full.names = T))



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

## generate global lat-long grid
ll_grid <- cshapes %>%
  st_bbox() %>%
  round() %>%
  st_as_sfc() %>% 
  st_make_grid(., cellsize = .5, what = 'polygons')

## convert from sfc to sf (old way, revert to simple w/ sf update)
ll_df <- data.frame(id = 1:length(ll_grid))
ll_df$geometry <- ll_grid
ll_grid <- st_as_sf(ll_df)

## subset grid to countries in cshapes and assign ID
ll_grid <- ll_grid[cshapes, ] %>% 
  mutate(id = 1:n())

## get years object for loop
years <- data_start:data_end

## this cant be done in a nested foreach loop b/c needs to create the
## appropriate grid at the start and %:% only works w/ two pure loops w/ no
## code not in both. could maybe get around by lapply-ing to generate the yearly
## grids beforehand...

## create placeholder list for loop output
grid_data <- list()

## calculate population, nightlights, and capital distance 
for(i in 1:length(years)) {
  
  ## subset cshapes to countries in existence in year i
  cshapes_i <- cshapes %>% filter(GWSYEAR <= years[i] & GWEYEAR >= years[i])
  
  capitals_i <- capitals %>% filter(GWSYEAR <= years[i] & GWEYEAR >= years[i])
  
  ## calculate which state falls the most in each grid cell
  ll_country_i <- ll_grid %>%
    st_intersection(cshapes_i) %>%
    mutate(int_area = st_area(.)) %>%
    group_by(id) %>%
    mutate(border = n() > 1) %>% 
    slice(which.max(int_area)) %>%
    select(id, GWCODE, border) %>%
    st_drop_geometry()
  
  ## associate each grid cell w/ the state it falls most in during year i
  ll_grid_i <- left_join(ll_grid, ll_country_i, by = 'id') %>% filter(!is.na(GWCODE))
  
  ## 
  grid_data[[i]] <- foreach(j = 1:nrow(ll_grid_i), .combine = 'rbind',
                            .errorhandling = 'remove') %dopar%{
    
    ## get cell j
    cell <- ll_grid_i[j, ]
    
    ## get capital associated w/ cell j
    capital <- capitals_i[capitals_i$GWCODE == cell$GWCODE &
                            years[i] %between%
                            st_drop_geometry(capitals_i[, c('GWSYEAR', 'GWEYEAR')]), ]
    
    ## get population
    pop_cnt_cell <- crop(population_cnt[[years[i] - 1989]], cell)
    
    ## get nightlights
    nl_cell <- crop(nightlights[[max(years[i] - 1991, 1)]], cell)
    
    ## mask population and nightlights for inequality calculation
    pop_cnt_cell <- mask(pop_cnt_cell, cshapes)
    nl_cell <- mask(nl_cell, cshapes)
    
    ## calculate total population
    pop_cell_tot <- cellStats(pop_cnt_cell, 'sum')
    
    ## calculate total nightlights
    nl_cell_tot <- cellStats(nl_cell, 'sum')
    
    ## if no nightlights for group-year i, recode to 1 to preserve inequality measure
    nl_cell_tot <- ifelse(nl_cell_tot == 0, 1, nl_cell_tot)
    
    ## project cell and rasters
    cell <- projectUTM(cell)
    capital <- st_transform(capital, st_crs(cell))
    
    ## calculate distance from territory centroid to capital in km
    cap_dist <- as.numeric(st_distance(st_centroid(cell), capital) / 1e3)
    
    ## calculate area of cell in km^2
    area_cell <- as.numeric(st_area(cell) / 1e6)
    
    c(cell$id, cell$GWCODE, years[i], pop_cell_tot, nl_cell_tot, cap_dist,
      cell$border, area_cell)
    
  }
  
}

## convert list of matrices to dataframe
grid_data <- do.call(rbind, grid_data) %>%
  unname() %>%
  data.frame() %>% 
  rename(id = X1, gwid = X2, year = X3, pop = X4,
         nl = X5, cap_dist = X6, border = X7, area = X8)

## save grid data
saveRDS(grid_data, here::here('Input Data/grid data.RDS'))

## print script to verify successful execution in log
print(paste('Grid Variable Creation Completed', Sys.time()))

## quit R
quit(save = 'no')



###################
## End of Script ##
###################