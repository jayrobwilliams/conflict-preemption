###############################
## author: Rob Williams      ##
## project: dissertation     ##
## created: October 10, 2019 ##
## updated: October 10, 2019 ##
###############################

## this script produces the proportion of separatist groups that are ethnic or
## religious in nature on p. 12 (accompanied by footnote 3) and Table K.1

## load packages
library(tidyverse)
library(RWmisc)
library(wesanderson)

set.seed(1234)

## load data
groups <- readRDS(here::here('Input Data/groups_nightlights.RDS'))

forge <- rio::import(here::here('Datasets/FORGE/forge_v1.0_public.xlsx'))

acd <- read.csv(here::here('Datasets/UCDP/ucdp-prio-acd-181.csv'))

acd_dyad <- read.csv(here::here('Datasets/UCDP/ucdp-dyadic-181.csv'))

acd2epr <- read.csv(here::here('Datasets/EPR/ACD2EPR-2018.csv'))

## proportion of separatist conflicts that are ethnic or religious
forge <- forge %>% filter(goalindep == 1)

mean(forge$ethnic == 1 | forge$religious == 1)

## subset to post 1992
forge <- forge %>%
  filter(fightyear >= 1992)

## proportion of separatist conflicts that are ethnic or religious post 1992
mean(forge$ethnic == 1 | forge$religious == 1)

## subset to variables for onset analysis
forge <- forge %>% select(conflict_id, dyad_id = dyadid, gname, fightyear,
                          goalindep)

acd <- acd %>% select(conflict_id, year)

acd_dyad <- acd_dyad %>% select(conflict_id, dyad_id, side_a, side_a_id, side_b,
                                side_b_id, year) %>% 
  separate_rows(side_b, side_b_id, sep = ',', convert = T)

groups <- groups %>% select(gwid, groupid, gwgroupid, year, excluded, pop_tot,
                            nl, oil, dom_overlap, area, border, cap_dist,
                            family_downgraded_regaut5, v2x_polyarchy, gdp,
                            state_ind, state_year_ind, year_ind)

## prepare ACD2epr for merging
acd2epr <- acd2epr %>%
  mutate(year = str_c(from, to, sep = ',')) %>%
  filter(!is.na(gwgroupid)) %>% 
  select(dyad_id = dyadid, side_a = sidea, side_b = sideb,
         side_a_id = sidea_id, side_b_id = sideb_id,
         group, gwgroupid, year) %>% 
  group_by(dyad_id, side_a, side_a_id, side_b, side_b_id, group, gwgroupid) %>%
  separate_rows(year, sep = ',', convert = T) %>%
  complete(year = seq(min(year), max(year), by = 1)) %>%
  fill(-year, .direction = 'down') %>% 
  distinct()

## merge FORGE and ACD and drop non-ethnic group observations
acd_dat <- left_join(acd, acd_dyad, by = c('conflict_id', 'year')) %>% 
  left_join(acd2epr, by = c('dyad_id', 'year', 'side_a', 'side_a_id', 'side_b', 'side_b_id')) %>% 
  left_join(forge, by = c('conflict_id', 'dyad_id')) %>% 
  filter(!is.na(gwgroupid))

## merge group data onto ACD-FORGE
acd_epr_onset <- left_join(groups, acd_dat, by = c('gwgroupid', 'year')) %>% 
  mutate(goalindep = replace_na(goalindep, 0),
         gname = case_when(is.na(goalindep) ~ NA_character_,
                           TRUE             ~ gname)) %>% 
  group_by(gwgroupid, year) %>% 
  arrange(desc(goalindep)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate_at(vars(pop_tot, nl, cap_dist, area, gdp), log) %>% 
  
  group_by(gwgroupid) %>% 
  mutate_at(vars(excluded:gdp, -nl), ~lag(., order_by = year)) %>% 
  filter(year >= 1992,
         gwgroupid != 34502000) %>% # serbs in croatia on in data in 1992 
  stevemisc::sbtscs(goalindep, year, gwgroupid) %>% 
  filter(!(fightyear < year & spell == 0)) %>% 
  mutate(spell2 = spell^2, spell3 = spell^3)

## fit logit of conflict onset
mod <- glm(goalindep ~ nl + cap_dist + as.factor(state_ind) + spell + spell2 + spell3,
               family = binomial(link = 'logit'), data = acd_epr_onset)

## logit conflict onset table
tabstr <- texreg::texreg(mod, stars = .05,
                         custom.coef.map = list('nl' = 'Nightlights',
                                                'cap_dist' = 'Capital Distance'),
                         caption = 'Logit analysis of separatist conflict onset',
                         custom.model.names = 'Model K.1',
                         float.pos = 'ht!', label = 'tab:nl_onset') 

## add in fixed effects check marks
tabstr <- sub('\\\\hline\\nAIC',
              paste0('\\\\hline\nCountry Fixed Effects & ',
                     paste(rep('\\\\checkmark',
                               times = 1),
                           collapse = ' & '),
                     ' \\\\\\\\',
                     '\nPolynomial Time & ',
                     paste(rep('\\\\checkmark',
                               times = 1),
                           collapse = ' & '),
                     ' \\\\\\\\\n\\\\hline\nAIC'),
              tabstr)

fileConn <- file(here::here('Tables/pd_onset_fixed_cy.tex'))
writeLines(tabstr, fileConn)
close(fileConn)



## quit script
quit(save = 'no')

###################
## end of script ##
###################