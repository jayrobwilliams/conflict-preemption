library(tidyverse)
library(sf)
library(RWmisc)

## read in cshapes
cshapes <- st_read(here::here('Datasets/cshapes/cshapes_0.6'), 'cshapes')

## read in GeoEPR
GeoEPR <- st_read(here::here('Datasets/EPR'), 'GeoEPR-2014 Cleaned')

## read in GADM
GADM <- st_read(dsn = here::here('Datasets/gadm/gadm34_levels_gpkg/gadm34_levels.gpkg'),
                layer = 'level1') %>% select(NAME_0, GID_0, NAME_1, GID_1)

## filter cshapes to countries in analysis
cshapes <- cshapes %>% filter(GWEYEAR >= 1990)
GeoEPR <- GeoEPR %>% filter(to >= 1990)

## filter to Nigeria
cshapes_n <- cshapes %>% filter(CNTRY_NAME == 'Nigeria')
GeoEPR_n <- GeoEPR %>% filter(statename == 'Nigeria')
GADM_n <- GADM %>% filter(NAME_0 == 'Nigeria')

## generate global lat-long grid
ll_grid <- cshapes %>%
  st_bbox() %>%
  round() %>%
  st_as_sfc() %>% 
  st_make_grid(., cellsize = 1, what = 'polygons')

## convert from sfc to sf (old way, revert to simple w/ sf update)
ll_df <- data.frame(id = 1:length(ll_grid))
ll_df$geometry <- ll_grid
ll_grid <- st_as_sf(ll_df)

## subset grid to Nigeria in cshapes and assign ID
ll_grid <- ll_grid[filter(cshapes, CNTRY_NAME == 'Nigeria'), ]

## code grid cells in or out of Nigeria based on majority of area rule
ll_country <- ll_grid %>%
  st_intersection(cshapes) %>%
  mutate(int_area = st_area(.)) %>%
  group_by(id) %>%
  mutate(border = n() > 1) %>% 
  slice(which.max(int_area)) %>%
  select(id, CNTRY_NAME, border) %>%
  st_drop_geometry()

## join country info to grid
ll_grid <- left_join(ll_grid, ll_country, by = 'id') %>%
  filter(CNTRY_NAME == 'Nigeria')

## triplicate cshapes for faceted plot
cshapes_merge <- cshapes_n %>% select(name = CNTRY_NAME) %>% 
  mutate(country = 1)
cshapes_merge <- cshapes_merge[rep(1, 3), ]
cshapes_merge$type = factor(c('group', 'gadm', 'grid'),
                            levels = c('group', 'gadm', 'grid'))

## create faceting variable
GeoEPR_merge <- GeoEPR_n %>% select(name = statename) %>% 
  mutate(country = 0, type = 'group')

## create faceting variable
GADM_merge <- GADM_n %>% select(name = NAME_0) %>% 
  mutate(country = 0, type = 'gadm') %>% 
  st_drop_geometry() %>% 
  st_set_geometry(st_geometry(GADM_n))

## create faceting variable
grid_merge <- ll_grid %>% select(name = CNTRY_NAME) %>% 
  mutate(country = 0, type = 'grid')

## set identical CRS
st_crs(GeoEPR_merge) <- st_crs(cshapes_merge)
st_crs(GADM_merge) <- st_crs(cshapes_merge)
st_crs(grid_merge) <- st_crs(cshapes_merge)

## plot
pdf(here::here('Figures/placebo_illus.pdf'), width = 8, height = 3)
rbind(cshapes_merge, GeoEPR_merge, GADM_merge, grid_merge) %>%
  mutate(country = factor(country, levels = 1:0)) %>% 
  ggplot(aes(fill = country, lty = country)) +
  geom_sf() +
  facet_wrap(~ type,
             labeller = as_labeller(c('group' = 'Ethnic Groups',
                                      'gadm' = 'Administrative Units',
                                      'grid' = 'PRIO-GRID'))) +
  scale_fill_manual(values = c('gray90', alpha(wes_palette('GrandBudapest1', 1), .25))) +
  scale_linetype_manual(values = c('solid', 'dashed')) +
  theme_rw() +
  theme(legend.position = 'none',
        axis.text = element_blank(),
        axis.ticks = element_blank())
dev.off()