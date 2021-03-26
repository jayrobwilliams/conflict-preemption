## this script is written to take advantage of structure in the names of
## DMSP-OLS raster files, if they change naming conventions in the future, the
## script may not work any longer. see the readme for more discussion of how
## this script is written

## print script to identify in log
print(paste('Nightlights Intercalibration Started', Sys.time()))

## keep original input files?
keep.input <- T

## archive the output for easier retrieval from cluster?
archive.output <- T

## load packages
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(doParallel)

## parallel setup
registerDoParallel(10)



## data access ####

## read in reference raster
nl_ref <- raster(list.files(path = here::here('Datasets/Nightlights/Calibrated'),
                            pattern='\\.tif$', full.names = T))

## read in data to be calibrated, create raster stack; different extents
nl <- stack(list.files(path = here::here('Datasets/Nightlights/Stable Lights'),
                       pattern='\\.tif$', full.names = T), quick = T)

## polygons for defining invariant regions
mauritius <- readRDS(here::here('Datasets/Nightlights/MUS_adm1.rds'))
japan <- readRDS(here::here('Datasets/Nightlights/JPN_adm1.rds'))
pr <- readRDS(here::here('Datasets/Nightlights/PRI_adm1.rds'))

## extract polygons for okinawa
okinawa <- japan[which(japan$NAME_1 == 'Okinawa'), ]; rm(japan)



## extract reference and raw data ####

## crop raw nightlights to invariant regions
mauritius_nl <- crop(nl, mauritius)
mauritius_nl <- mask(mauritius_nl, mauritius)
okinawa_nl <- crop(nl, okinawa)
okinawa_nl <- mask(okinawa_nl, okinawa)
pr_nl <- crop(nl, pr)
pr_nl <- mask(pr_nl, pr)

## crop reference nightlights to invariant regions
mauritius_ref <- crop(nl_ref, mauritius)
mauritius_ref <- mask(mauritius_ref, mauritius)
okinawa_ref <- crop(nl_ref, okinawa)
okinawa_ref <- mask(okinawa_ref, okinawa)
pr_ref <- crop(nl_ref, pr)
pr_ref <- mask(pr_ref, pr)

## extract data from invariant region, drop IDs since they are inherited
## from polygons used to crop
mauritius_data <- extract(mauritius_nl, mauritius, df = T)
mauritius_data$ID <- NULL
okinawa_data <- extract(okinawa_nl, okinawa, df = T)
okinawa_data$ID <- NULL
pr_data <- extract(pr_nl, pr, df = T)
pr_data$ID <- NULL

## extract reference data from invariant regions
mauritius_ref <- extract(mauritius_ref, mauritius, df = T)
mauritius_ref <- mauritius_ref$F152001.v4b_web.stable_lights.avg_vis
okinawa_ref <- extract(okinawa_ref, okinawa, df = T)
okinawa_ref <- okinawa_ref$F152001.v4b_web.stable_lights.avg_vis
pr_ref <- extract(pr_ref, pr, df = T)
pr_ref <- pr_ref$F152001.v4b_web.stable_lights.avg_vis

## create dataframe for intercalibration model
model_data <- data.frame(ref = c(mauritius_ref, okinawa_ref, pr_ref),
                         rbind(mauritius_data, okinawa_data, pr_data))



## intercalibration model ####

## regress raw nightlights on reference raster
foreach(i = 2:ncol(model_data), .packages = 'raster') %dopar% {
  
  assign(paste('mod', names(model_data)[i], sep = '_'),
         nls(paste('ref ~ a * ', paste(names(model_data)[i]), '^b', sep = ''),
             data = model_data, start = list(a = 1, b = 1), trace = T))
  
}

## calibrate each nightlights raster using coefficients from intercalibration
## model note that model object names need to be passed to loop w/ .export for
## use of get()s
foreach(i = 2:ncol(model_data), .export = ls(pattern = 'mod_'), .packages = 'raster') %dopar% {
  
  predict(nl[[i-1]], get(paste('mod', names(model_data)[i], sep = '_')),
          paste('Calibrated/', names(model_data)[i], '.tif', sep = ''),
          overwrite = T, na.rm = T)
  
}



## collapse to one raster per year ####

## clear environment, except keep.input
rm(list= ls()[!(ls() %in% c('keep.input', 'archive.output'))])

## read in calibrated nightlights data, create raster stack
nl <- stack(list.files(path = 'Calibrated', pattern='\\.tif$', full.names = T))

## reorder stack to put overlapping years next to one another
names(nl) <- substr(names(nl), 4, 7)
nl <- subset(nl, order(names(nl)))

## identify all layers that overlap by searching for '.'
overlap <- grep('\\.', names(nl))

## create output directory for calibrated and averaged raster files
dir.create(here::here('Datasets/Nightlights/Output'), showWarnings = F)

## loop to average overlapping years; this only works because there are only
## ever two of the same year -- this logic WILL NOT WORK if there are years with
## three rasters
foreach(i = 1:length(overlap), .packages = 'raster') %dopar% {
  
  ## skip even numbers, operating on i and i+1 captures pairs of odds and evens
  if (i %% 2 != 0) {
    
    ## get the two overlapping years
    inputs <- c(overlap[i], overlap[i+1])
    
    ## select name of first raster in overlap, drop the '.1' from the end
    output_name <- gsub('\\.[^.]*$', '', names(nl[[overlap[i]]]))
    
    ## create new raster averaging values in overlapping years
    assign(output_name, mean(nl[[inputs]]))
    
    ## write to output directory
    writeRaster(get(output_name),
                here::here('Datasets/Nightlights/Output',
                           paste0('nl_', gsub('X', '', output_name), '.tif')),
                overwrite = T)
  
  }
  
}

## identify all layers that do not overlap by selecting those without '.'
not_overlap <- grep('^[^\\.]*$', names(nl))

## write raster layers that do not overlap into output directory
foreach(i = 1:length(not_overlap), .packages = 'raster') %dopar% {
  
  ## write to output directory
  writeRaster(nl[[not_overlap[i]]],
              here::here('Datasets/Nightlights/Output',
                         paste0('nl_', gsub('X', '',
                                            names(nl[[not_overlap[i]]])),
                                '.tif')),
              overwrite = T)
  
}

## if not keeping input rasters, remove stable lights and calibrated directory
if (keep.input != T) unlink(here::here('Datasets/Nightlights',
                                       c('Stable Lights', 'Calibrated')),
                            recursive = T)

## if archiving output, tar.gz
if (archive.output == T) {
  
  tar(here::here('Datasets/Nightlights/Output.tar.gz'),
      list.files(here::here('Datasets/Nightlights/Output'), pattern = '*.tif',
                 recursive = T, full.names = T),
      compression = 'gzip')
  
}



## print script to verify successful execution in log
print(paste('Nightlights Intercalibration Completed', Sys.time()))

## quit R
quit(save = 'no')

###################
## end of script ##
###################