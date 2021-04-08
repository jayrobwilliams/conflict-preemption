## this script calculate the correlation between Myers numbers and nightlights
## reported on p. 14 (accompanied by footnote 5)

## load packages
library(tidyverse)

## read in GADM data
gadm <- readRDS(here::here('Input Data/gadm data.RDS'))

## read in Myers numbers
myers <- rio::import(here::here('Datasets/Myers Numbers/subnational_myers.dta'))

## join Myers numbers to GADM data
myers <- inner_join(myers, gadm, by = c('countryname' = 'state',
                                       'regionname' = 'region',
                                       'year')) %>% 
  mutate(across(c(nl, myers), log)) %>% 
  filter(!is.na(myers), !is.na(nl))

## save correlation b/w Myers numbers and nightlights
fileConn <- file(here::here('Tables/nl_sudan.txt'))
writeLines(paste0('The correlation between Myers numbers and nightlights is ',
                  round(cor(myers$nl, myers$myers), 2)),
           fileConn)
close(fileConn)
