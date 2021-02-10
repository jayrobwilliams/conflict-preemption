###############################
## author: Rob Williams      ##
## project: dissertation     ##
## created: October 10, 2019 ##
## updated: October 10, 2019 ##
###############################

## this script produces Figure 1 and the figure of percentage of ethnic groups
## excluded from political power on p. 3 (accompanied by footnote 1)

## load packages
library(tidyverse)
library(lubridate)
library(RWmisc)
library(wesanderson)

## load data
acd <- read.csv(here::here('Datasets/UCDP/ucdp-prio-acd-181.csv'))
forge <- rio::import(here::here('Datasets/FORGE/forge_v1.0_public.xlsx'))
sdm <- rio::import(here::here('Datasets/SRDP/SRDP_Org_2019_release.dta'))
epr <- read.csv(here::here('Datasets/EPR/EPR-2018.csv'))

## merge ACD and FORGE and prepare for merging
dat_for <- left_join(acd, forge, by = 'conflict_id') %>%
  filter(goalindep == 1) %>%
  group_by(year) %>% 
  summarize(ct = length(unique(conflict_id))) %>% 
  mutate(dat = 'forge')

## prepare SDM for merging
dat_sdm <- sdm %>% filter(independencedummy == 1) %>%
  group_by(year) %>% 
  summarize(ct = length(unique(group))) %>% 
  mutate(dat = 'sdm')

pdf(here::here('Figures/sd-secession-trend.pdf'), width = 8, height = 4)
bind_rows(dat_for, dat_sdm) %>% 
  filter(year >= 1960, year <= 2005) %>% 
  ggplot(aes(x = year, y = ct, color = dat)) + 
  geom_line() +
  scale_color_manual(name = '',
                     breaks = c('forge', 'sdm'),
                     labels = c('Secessionist Conflicts',
                                'Separatist Movements'),
                     values = rev(wes_palette('GrandBudapest1', 2))) +
  labs(x = '', y = 'Annual Count') +
  ylim(0, NA) +
  theme_rw() +
  theme(legend.position = 'bottom')
dev.off()

## politicial exclusion 1946-
epr %>% 
  summarize(excl = sum(!status %in% c('DOMINANT', 'SENIOR PARTNER',
                                      'MONOPOLY', 'JUNIOR PARTNER')) / n())

## politicial exclusion 1960-
epr %>% 
  filter(to >= 1960) %>% 
  summarize(excl = sum(!status %in% c('DOMINANT', 'SENIOR PARTNER',
                                      'MONOPOLY', 'JUNIOR PARTNER')) / n())

## politicial exclusion 1992-
epr %>% 
  filter(to >= 1992) %>% 
  summarize(excl = sum(!status %in% c('DOMINANT', 'SENIOR PARTNER',
                                      'MONOPOLY', 'JUNIOR PARTNER')) / n())



## quit script
quit(save = 'no')

###################
## end of script ##
###################