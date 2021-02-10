#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: dissertation             ##
## created: May 17, 2018             ##
## updated: May 17, 2018             ##
#######################################

## this script recodes countries in cshapes that change status within the same
## year. this happens when borders change or capitals are moved and the effective
## date is anything other than december 31. it uses a rule where any change that
## occurs in the first half of a year results in that iteration of a country
## ending the previous year. if a change occurs in the second half of a year,
## that year is the last year of the previous iteration of the country and the
## new iteration begins the following year. it requires that cshapes has been
## loaded from shapefile into a spatialpointsdataframe

cshapes.rc <- function(cshapes) {
  
  ## load packages
  require(spdplyr)
  
  ## recode cshapes
  cshapes[cshapes$CNTRY_NAME == 'Germany', "GWSYEAR"] <- 1991
  cshapes[cshapes$CNTRY_NAME == 'Serbia' &
            cshapes$GWEYEAR == 2008, "GWEYEAR"] <- 2007
  cshapes[cshapes$CNTRY_NAME == 'Yugoslavia' &
            cshapes$GWSYEAR == 1991, "GWSYEAR"] <- 1992
  cshapes[cshapes$CNTRY_NAME == 'Serbia and Montenegro' &
            cshapes$GWSYEAR == 1992, "GWSYEAR"] <- 1993
  cshapes <- cshapes %>% filter(!(CNTRY_NAME == 'USSR' & GWSYEAR == 1991))
  cshapes[cshapes$CNTRY_NAME == 'Russia', "GWSYEAR"] <- 1992
  cshapes[cshapes$CNTRY_NAME == 'Tanzania' &
            cshapes$GWEYEAR == 1996, "GWEYEAR"] <- 1995
  cshapes[cshapes$CNTRY_NAME == 'Ethiopia' &
            cshapes$GWEYEAR == 1993, "GWEYEAR"] <- 1992
  cshapes <- cshapes %>% filter(!(CNTRY_NAME == 'South Africa' & GWEYEAR == 1990))
  cshapes[cshapes$CNTRY_NAME == 'Sudan' &
            cshapes$GWSYEAR == 2011, "GWSYEAR"] <- 2012
  cshapes[cshapes$CNTRY_NAME == 'Saudi Arabia' &
            cshapes$GWEYEAR == 2000, "GWEYEAR"] <- 1999
  cshapes <- cshapes %>% filter(CNTRY_NAME != 'Yemen Arab Republic')
  cshapes[cshapes$CNTRY_NAME == 'Yemen' &
            cshapes$GWEYEAR == 2000, "GWEYEAR"] <- 1999
  cshapes[cshapes$CNTRY_NAME == 'Indonesia' &
            cshapes$GWEYEAR == 2002, "GWEYEAR"] <- 2001
  
  ## return modified cshapes
  return(cshapes)
  
}
