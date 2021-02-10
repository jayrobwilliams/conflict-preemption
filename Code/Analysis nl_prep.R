############################
## author: Rob Williams   ##
## project: dissertation  ##
## created: March 8, 2020 ##
## updated: March 8, 2020 ##
############################

## this script prepares the data used in analyses

## print script to identify in log
print(paste('Nightlights Data Prep Started', Sys.time()))

## load packages
library(tidyverse)
library(vdem)
library(WDI)
library(countrycode)
library(mice)
library(brms)
library(loo)
library(xtable)
library(RWmisc)

## create output directories
dirs <- c('Stanfits', 'Tables', 'Figures', 'Figure Data')
sapply(dirs, function(x) dir.create(here::here(x), showWarnings = F))

## read in ethnic group data
groups <- readRDS(here::here('Input Data/group data.RDS'))

## read in GROWup data
GROWup <- read.csv(here::here('Datasets/GROWup/data.csv'))

## subset GROWup data to relevant variables
GROWup <- GROWup %>%
  select(gwgroupid, year, family_downgraded_regaut5) %>%
  filter(year >= 1990) %>%
  group_by(gwgroupid) %>%
  fill(nightlight_corr, .direction = 'up') %>% # use 1992 nl values for 90 and 91
  data.frame()

## read in polyarchy from VDEM
vdem <- extract_vdem(name_pattern = 'v2x_polyarchy',
                     include_sd = T, include_uncertainty = F) %>%
  select(gwid = GWn, year, v2x_polyarchy, v2x_polyarchy_sd) %>% 
  filter(year >= 1989)

## get GDP per capita for one year lag
GDP <- WDI(country = 'all', indicator = 'NY.GDP.PCAP.KD', start = 1989, end = 2013)

## merge rebel group goals and ethnic group data, recode provisional goal from
## character to numeric, drop uncertain goals, code nonviolence for peaceful
## ethnic groups, convert ethnic group status to ordered factor recoding NAs 
## to irrelevant, and  sort by nonviolence and group goal
groups <- groups %>% replace_na(list(downgraded = 0, status = 'IRRELEVANT')) %>%
  mutate(year_lag = year - 1)

## join GROWup data
groups <- left_join(groups, GROWup)

## merge lagged polyarchy scores 
groups <- left_join(groups, vdem %>% rename(year_lag = year))

## create COWcodes and lag GDP
GDP <- GDP %>% mutate(COWcode = countrycode(iso2c, 'iso2c', 'cown')) %>%
  select(-country, -iso2c, gdp = NY.GDP.PCAP.KD, year_lag = year)

## merge lagged GDP to goals data, left join to place it to the right
groups <- groups %>% left_join(GDP)

## filter out duplicated cases until this is fixed in the state/group variable
## creation scripts. only case of a duplicate caused by a territorial and
## governmental conflict starting in the same year ends before 1990 so not an
## issue now
groups <- groups %>% filter(!duplicated(paste(state, group, year)))

## create time state indicator vector for random intercepts
groups <- groups %>% mutate(state_ind = as.numeric(as.factor(gwid)),
                            state_year_ind = as.numeric(as.factor(state_ind * 1e5 + year)),
                            year_ind = year - 1989)

## save goals object for results
saveRDS(groups, here::here('Input Data/groups_nightlights.RDS'))



## descriptive statistics ####

## create logged and scaled variables for models
groups_log <- groups %>% select(nl, pop_tot, pop_gini, cap_dist, area,
                                balance, gdp) %>% 
  mutate_at(vars(-balance), log) %>%
  cbind(groups %>% select(gwgroupid, year, state_ind, year_ind, state_year_ind), .,
        groups %>% select(v2x_polyarchy, excluded, family_downgraded_regaut5,
                          dom_overlap, border)) %>% 
  group_by(gwgroupid) %>% 
  mutate_at(vars(pop_tot:dom_overlap), ~lag(., order_by = year)) %>% 
  filter(year >= 1992, !is.na(pop_tot)) %>% # drop NAs from lagging
  data.frame()

## labeller for descriptive statistics
desc_labs <- as_labeller(c('nl' = 'Nightlights',
                           'pop_tot' = 'Population',
                           'pop_gini' = 'Population Concentration',
                           'cap_dist' = 'Capital Distance',
                           'border' = 'Border',
                           'area' = 'Area',
                           'excluded' = 'Excluded',
                           'overlap' = 'Overlap',
                           'dom_overlap' = 'Dominant Group Presence',
                           'family_downgraded5' = 'Downgraded',
                           'family_downgraded_regaut5' = 'Lost Autonomy',
                           'gdp' = 'GDP',
                           'v2x_polyarchy' = 'Polyarchy',
                           '(Intercept)' = '(Constant)'))


## capital distance and political status descriptive statistics
pdf(here::here('Figures/dist_hist_excl_log_cent.pdf'), width = 8, height = 4)
groups_log %>% group_by(gwgroupid) %>% 
  slice(n()) %>% 
  select(cap_dist, excluded) %>%
  ggplot(aes(x = cap_dist)) +
  geom_histogram() +
  labs(x = 'Capital Distance', y = 'Count') +
  facet_wrap(~ excluded, ncol = 1,
             labeller = as_labeller(c('FALSE' = 'Included',
                                      'TRUE' = 'Excluded'))) +
  theme_rw()
dev.off()

pdf(here::here('Figures/dist_hist_excl.pdf'), width = 6, height = 4)
groups %>% mutate(excluded = as.numeric(status) <= 4) %>%
  select(excluded, cap_dist) %>% 
  ggplot(aes(x = cap_dist)) +
  geom_histogram() +
  labs(x = 'Capital Distance', y = 'Count') +
  facet_wrap(~ excluded, ncol = 1,
             labeller = as_labeller(c('FALSE' = 'Included',
                                      'TRUE' = 'Excluded'))) +
  theme_rw()
dev.off()

pdf(here::here('Figure/bord_hist_excl.pdf'), width = 6, height = 4)
groups %>% mutate(excluded = as.numeric(status) <= 4) %>%
  select(excluded, border) %>% 
  ggplot(aes(x = border)) +
  geom_bar() +
  labs(x = 'Capital Distance', y = 'Count') +
  facet_wrap(~ excluded, ncol = 1,
             labeller = as_labeller(c('FALSE' = 'Included',
                                      'TRUE' = 'Excluded'))) +
  theme_rw()
dev.off()

## descriptive statistics
pdf(here::here('Figures/descriptives.pdf'), width = 8, height = 5)
groups_log %>% select(nl, cap_dist, pop_tot, border, area, excluded,
                      family_downgraded_regaut5, gdp, v2x_polyarchy, dom_overlap) %>% 
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ key, scales = 'free', nrow = 2, labeller = desc_labs) +
  theme_rw()
dev.off()

## calculate bivariate correlations
fileConn <- file(here::here('Tables/bivar_cor.tex'))
writeLines(paste0('The correlation between population and nightlights is ',
                  cor(groups_log$nl, groups_log$pop_tot),
                  'The correlation between population concentration and nightlights is ',
                  cor(groups_log$nl, groups_log$pop_gini),
                  '\nthe correlation between capital distance and nightlights is ',
                  cor(groups_log$nl, groups_log$cap_dist),
                  '\nthe correlation between borders and nightlights is ',
                  cor(groups_log$nl, groups_log$border),
                  '\nthe correlation between capital distance and population is ',
                  cor(groups_log$cap_dist, groups_log$pop_tot)), fileConn)
close(fileConn)



## missing variables ####

## extract missing variables
miss_vars <- apply(groups_log, 2, function(x) mean(is.na(x)))
miss_vars <- sort(miss_vars[sapply(miss_vars, function(x) x > 0)])
miss_vars <- data.frame(miss_vars[!(names(miss_vars) %in% c('balance', 'family_downgraded5'))] * 100)

## name dimensions
rownames(miss_vars) <- c('Polyarchy', 'Lost Autonomy', 'GDP per capita')
colnames(miss_vars) <- '\\% Missing'

## print missing data table
print.xtable(xtable(miss_vars, align = c('l', 'l'),
                    caption = 'Missingness of control variables.',
                    label = 'tab:missing'),
             sanitize.colnames.function = function(x) x,
             file = here::here('Tables/pd_lm_missing.tex'),
             table.placement = 'h!')

## print script to verify successful execution in log
print(paste('Nightlights Data Prep Completed', Sys.time()))

## quit script
quit(save = 'no')

###################
## end of script ##
###################