## this script extracts the spatial components of the territorial governability
## measure from each ethnic group-year polygon, including population, nightlights,
## travel times, and various statistics calculated based on them

## print script to identify in log
print(paste('Group Variable Creation Started', Sys.time()))

## load packages
library(sf) # new unified spatial package
library(raster) # pixel based data
library(rgdal) # spatial data I/O
library(rgeos) # spatial topology operations
library(tidyverse)
library(stringr)
library(data.table)
library(ineq) # Gini coefficient

source(here::here('Code/sfFunctions.R'))
source(here::here('Code/cshapes Recode.R'))

library(doParallel)
registerDoParallel(as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')))

## create directory to hold output
dir.create(here::here('Input Data'), showWarnings = F)



## read in data ####

## read in cshapes
cshapes <- st_read(here::here('Datasets/cshapes/cshapes_0.6'), 'cshapes')

## read in GeoEPR
GeoEPR <- st_read(here::here('Datasets/EPR'), 'GeoEPR-2014 Cleaned')

## read in EPR
EPR <- read.csv(here::here('Datasets/EPR/EPR-2014.csv'))

## read in oil data
oil <- st_read(here::here('Datasets/PRIO/PETRODATA/PETRODATA V1.2'), 'onshore_cleaned')

## read in population rasters
population_cnt <- stack(list.files(here::here('Datasets/Population/Corrected'),
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

## recode from <= 1990 to 1990 and >= 2013 to 2013
state_data <- cshapes %>%
  mutate(GWSYEAR = ifelse(GWSYEAR <= data_start, data_start, GWSYEAR),
         GWEYEAR = ifelse(GWEYEAR >= data_end, data_end, GWEYEAR))

## expand cshapes dataframe to yearly observations
state_data <- data.frame(setDT(state_data)[, list(GWCODE = GWCODE,
                                                  year = seq(GWSYEAR, GWEYEAR, by = 1)),
                                           by = 1:nrow(state_data)][, -1])

## drop oil fields w/o confirmed discovery date
oil <- oil %>% filter(DISC != -9999)



## data preprocessing ####

## temporal range
data_start <- 1990
data_end <- 2013

## convert group status to ordered factor in EPR; report coding in text
EPR$status <- factor(EPR$status, ordered = T, levels = c('IRRELEVANT',
                                                         'STATE COLLAPSE',
                                                         'DISCRIMINATED',
                                                         'POWERLESS',
                                                         'SELF-EXCLUSION',
                                                         'JUNIOR PARTNER',
                                                         'SENIOR PARTNER',
                                                         'DOMINANT',
                                                         'MONOPOLY'))

## subset EPR to groups that end after start of sample; recode from <= 1990 to 1990
EPR_df <- EPR %>%
  filter(to >= data_start) %>%
  mutate(from = ifelse(from <= data_start, data_start, from))

## expand EPR to yearly observations
EPR_df <- data.frame(setDT(EPR_df)[, list(gwid = gwid, groupid = groupid,
                                          gwgroupid = gwgroupid,
                                          year = as.numeric(seq(from, to, by = 1)),
                                          size = size, status = status,
                                          reg_aut = reg_aut, umbrella = umbrella),
                                   by = 1:nrow(EPR_df)][, -1])

## subset GeoEPR to polygons that end after start of sample
GeoEPR <- GeoEPR %>% filter(to >= data_start)

## extract dataframe and recode from <= 1990 to 1990
GeoEPR_df <- GeoEPR %>% mutate(from = ifelse(from <= data_start, data_start, from))

## expand GeoEPR dataframe to yearly observations
GeoEPR_df <- data.frame(setDT(GeoEPR_df)[, list(gwid = gwid,
                                                groupid = groupid,
                                                gwgroupid = gwgroupid,
                                                year = seq(from, to, by = 1),
                                                state = statename, group = group),
                                         by = 1:nrow(GeoEPR_df)][, -1])

## GeoEPR and EPR disagree on when some groups are politically relevant e.g.
## GeoEPR says that the northern groups in sierra leone are politically relevant
## in 1996-2007, while EPR says they are politically relevant in 1997-2007. I
## defer to EPR since it is the main data source. other issues arise when GeoEPR
## has a polygon for a group that moves in and out of relevance over time, but
## that relevance is entirely outside the sample period of 1990-2013 e.g. Hindus
## in Mauritius have a polygon from 1969-2013, but their last year of political
## relevance is 1987, so they get NA for all of the EPR variables (status, size,
## etc). the last example in this sample are the Shona in Zimbabwe, which do not
## become politically relevant until 1992.
GeoEPR_df <- GeoEPR_df %>%
  left_join(EPR_df) %>%
  tidyr::replace_na(list(status = 'IRRELEVANT'))

## recode croatia to start in 1992 to match cshapes coding using international
## recognition by EEC and UN, instead of EPR coding using independence referendum
## in 1991
GeoEPR_df <- GeoEPR_df %>% filter(!(state == 'Croatia' & year == 1991))

## same but for Slovenia
GeoEPR_df <- GeoEPR_df %>% filter(!(state == 'Slovenia' & year == 1991))

## same but for Russia; USSR ends on 12/20/91
GeoEPR_df <- GeoEPR_df %>% filter(!(state == 'Russia' & year == 1991))

## drop observations after 2013 b/c nightlights still end then
GeoEPR_df <- GeoEPR_df %>% filter(year <= 2013)



## onset related variables
group_data <- foreach(i = 1:nrow(GeoEPR_df), # replace w/ nrow(GeoEPR_df) after figuring out
                .packages = c('sf', 'sp', 'raster', 'rgeos', 'ineq', 'dplyr',
                              'data.table'),
                .combine = rbind, .errorhandling = 'remove') %dopar% {
  
  ## get group-year i
  group <- GeoEPR_df[i, ]
  
  ## get state-year for group-year i
  state <- state_data[state_data$GWCODE == group$gwid & state_data$year == group$year, ]
  
  ## get polygons for group-year i's state
  state_poly <- cshapes[cshapes$GWCODE == state$GWCODE &
                          state$year %between% st_drop_geometry(cshapes[, c('GWSYEAR', 'GWEYEAR')]), ]
  
  ## get point for group-year i's capital
  capital <- capitals[capitals$GWCODE == group$gwid &
                        group$year %between% st_drop_geometry(capitals[, c('GWSYEAR', 'GWEYEAR')]), ]
  
  ## get polygon for group-year i
  terr <- GeoEPR[GeoEPR$gwgroupid == group$gwgroupid &
                   group$year %between% st_drop_geometry(GeoEPR[, c('from', 'to')]), ]
  
  ## print ID message; maybe temporary?
  print(paste('Coding group variables', 'for', terr$group, 'in',
              terr$statename, 'row', i))
  
  ## get number of polygons group is spread across (includes holes, I think...)
  polygons_terr <- length(unique(st_coordinates(terr)[,4]))
  
  ## subset GeoEPR to group polygons in existence for group-year i and get
  ## adjacent and overlapping group polygons
  GeoEPR_terr <- GeoEPR[group$year %between% st_drop_geometry(GeoEPR[, c('from', 'to')]), ][terr, ]
  
  ## drop group i's polygon
  GeoEPR_terr <- GeoEPR_terr[GeoEPR_terr$gwgroupid != terr$gwgroupid, ]
  
  ## drop polygons not in group i's state
  GeoEPR_terr <- GeoEPR_terr[GeoEPR_terr$statename == terr$statename, ]
  
  ## get population for group-year i; 1989 b/c 1 indexing
  pop_cnt_terr <- crop(population_cnt[[group$year - 1989]], terr)
  
  ## get nightlights for group-year i; use 1992 data for 1990 and 1991; not ideal but
  ## still better than just using nightlights for only one year; 1991 b/c 1 indexing
  nl_terr <- crop(nightlights[[max(group$year - 1991, 1)]], terr)
  
  ## mask population and nightlights for inequality calculation
  pop_cnt_terr <- mask(pop_cnt_terr, terr)
  nl_terr <- mask(nl_terr, terr)
  
  ## calculate total population
  pop_terr_tot <- cellStats(pop_cnt_terr, 'sum')
  
  ## calculate population inequality; recode NaN to 0 b/c perfect equality
  pop_terr_gini <- Gini(pop_cnt_terr@data@values)
  pop_terr_gini <- ifelse(is.nan(pop_terr_gini), 0, pop_terr_gini)
  
  ## calculate mean nightlights
  nl_terr_mean <- cellStats(nl_terr, 'mean')
  
  ## calculate median nightlights
  nl_terr_med <- median(nl_terr@data@values, na.rm = T)
  
  ## calculate total nightlights
  nl_terr_tot <- cellStats(nl_terr, 'sum')
  
  ## if no nightlights for group-year i, recode to 1 to preserve inequality measure
  nl_terr_tot <- ifelse(nl_terr_tot == 0, 1, nl_terr_tot)
  
  ## subset oil fields to those discovered before group-year
  oil_terr <- oil %>% filter(DISC <= group$year)
  
  ## check for presence of oil in territory
  oil_terr <- max(st_intersects(terr, oil_terr, sparse = F))
  
  ## project territory and rasters
  terr <- projectUTM(terr)
  state_poly <- st_transform(state_poly, st_crs(terr))
  capital <- st_transform(capital, st_crs(terr))
  pop_cnt_terr <- projectRaster(pop_cnt_terr, crs = CRS(st_crs(terr)$proj4string))
  nl_terr <- projectRaster(nl_terr, crs = CRS(st_crs(terr)$proj4string))
  GeoEPR_terr <- st_transform(GeoEPR_terr, st_crs(terr))
  
  ## redraw polygons w/ GEOS, fixing topology errors
  terr <- st_simplify(terr, preserveTopology = T, dTolerance = 0)
  GeoEPR_terr <- st_simplify(GeoEPR_terr, preserveTopology = T, dTolerance = 0)
  state_poly <- st_simplify(state_poly, preserveTopology = T, dTolerance = 0)
  terr <- st_buffer(terr, dist = .001)
  GeoEPR_terr <- st_buffer(GeoEPR_terr, dist = .001)
  state_poly <- st_buffer(state_poly, dist = .001)
  
  ## check whether territory abuts an international border by checking whether
  ## it is fully covered by the state's polygon buffered 1km inward
  border <- !st_within(terr, st_buffer(state_poly, -1e3), sparse = F)[[1]]
  
  ## calculate area of group territory in km^2
  area_terr <- as.numeric(st_area(terr) / 1e6)
  
  ## calculate distance from territory centroid to capital in km
  cap_dist <- as.numeric(st_distance(st_centroid(terr), capital) / 1e3)
  
  ## code whether group's status has been downgraded in the previous year
  downgraded <- group$status < (EPR[EPR$gwgroupid == group$gwgroupid, ]
                                [(group$year - 1) %between%
                                    EPR[EPR$gwgroupid == group$gwgroupid,
                                        c("from", "to")], "status"])
  
  ## if state does not exist in previous year, set downgraded to 0 b/c new
  ## political context (replace w/ 2 and spot code after running script later)
  if (length(downgraded) == 0) downgraded <- FALSE
  
  ## code political exclusion for group-year i
  excluded <- as.numeric(group$status) <= 4
  
  ## get all ethnic group-years in state-year
  groups <- GeoEPR_df[GeoEPR_df$gwid == group$gwid & GeoEPR_df$year == group$year, ]
  
  if (all(group$gwgroupid == groups[groups$status == max(groups$status), 'gwgroupid'])) {
    
    ## group-year i is dominant group, so its territory overlaps w/ dominant group's
    dom_overlap <- 1
    
  } else {
    
    ## code whether group-year i's territory overlaps the territory of the dominant group
    dom_overlap <- GeoEPR %>%
      left_join(groups, by = c('gwid', 'group', 'groupid', 'gwgroupid')) %>% 
      filter(status == max(groups$status) & gwid %in% groups$gwid) %>% 
      st_transform(st_crs(terr)) %>% 
      st_intersects(terr) %>%
      as.numeric() %>%
      replace_na(0)
    
  }
  
  ## return all measures for concatenation by foreach
  data.frame(COWcode = state_poly$COWCODE, gwid = group$gwid, groupid = group$groupid,
             gwgroupid = group$gwgroupid, year = group$year, state = group$state,
             group = group$group, size = group$size, status = group$status,
             downgraded = downgraded, excluded = excluded,
             pop_tot = pop_terr_tot, pop_gini = pop_terr_gini,
             nl = nl_terr_tot, oil_terr, dom_overlap = dom_overlap,
             area = area_terr, border = border, cap_dist = cap_dist,
             polygons = polygons_terr)
  
}



## save group data
saveRDS(group_data, here::here('Input Data/group data.RDS'))

## print script to verify successful execution in log
print(paste('Group Variable Creation Completed', Sys.time()))

## quit R
quit(save = 'no')



###################
## End of Script ##
###################