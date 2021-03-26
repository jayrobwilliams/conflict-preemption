## this script calculates correlations between nightlights and security
## personnel numbers in Southern Sudan in 2008 on p. 13

## load packages
library(tidyverse)
library(sf)
library(raster)
library(RWmisc)
options(stringsAsFactors = F)

## load data
sudan_rep <- read.csv(here::here('Datasets/de Jaun and Pierskalla 2015',
                                 'sudan_replication.csv'))

## from http://mapeastafrica.com/countries/east-africa-shapefiles/south-sudan-shapefiles/
sudan_poly <- st_read(here::here('Datasets/SouthSudan_admin_WGS84',
                                 'SouthSudan_admin_2014_WGS84.shp'))

## load 2008 nightlights
nl <- raster(here::here('Datasets/Nightlights/Output/nl_2008.tif'))

## repair names and join data to polygons
sudan <- sudan_poly %>% 
  mutate(Name = case_when(Name == 'Lapon' ~ 'Lopa',
                          Name == 'Rumbek Centre' ~ 'Rambek Centre',
                          Name == 'Canal (Khor Fulus)' ~ 'Canal',
                          Name == 'Panyijar' ~ 'Panyijiar',
                          Name == 'Nyriol' ~ 'Nyirol',
                          TRUE ~ Name)) %>% 
  right_join(sudan_rep, by = 'Name')

## crop and mask nightlights
nl <- crop(nl, sudan)
nl <- mask(nl, sudan)

## extract total nightlights by region
sudan_nl <- extract(nl, sudan, fun = sum)

## figure out appropriate variable to correlate w/ nightlights
cor(log1p(sudan_nl), sudan$Pers_Sec, use = 'complete.obs')
