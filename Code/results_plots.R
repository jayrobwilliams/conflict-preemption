###############################
## author: Rob Williams      ##
## project: dissertation     ##
## created: October 10, 2019 ##
## updated: October 10, 2019 ##
###############################

## this script executes 

## load packages
library(tidyverse)
library(wesanderson)
library(RWmisc)

## load goals object for results
groups <- readRDS(here::here('Input Data/groups_nightlights.RDS'))

## create logged and lagged variables for models
groups_log <- groups %>% select(nl, pop_tot, cap_dist, area, gdp) %>% 
  mutate_all(log) %>%
  cbind(groups %>% select(gwgroupid, year, state_ind, state_year_ind), .,
        groups %>% select(v2x_polyarchy, excluded, family_downgraded_regaut5,
                          dom_overlap, border, oil)) %>% 
  group_by(gwgroupid) %>% 
  mutate_at(vars(pop_tot:oil), ~lag(., order_by = year)) %>% 
  filter(year >= 1992, !is.na(pop_tot)) %>% # drop NAs from lagging
  data.frame()

##
desc_labs <- as_labeller(c('nl' = 'ln Nightlights',
                           'pop_tot' = 'ln Population',
                           'border' = 'Border',
                           'cap_dist' = 'ln Capital Distance',
                           'area' = 'ln Area',
                           'excluded' = 'Excluded',
                           'dom_overlap' = 'Dominant Group Presence',
                           'oil' = 'Oil',
                           'family_downgraded_regaut5' = 'Lost Autonomy',
                           'gdp' = 'ln GDP per capita',
                           'v2x_polyarchy' = 'Polyarchy',
                           '(Intercept)' = '(Constant)'))


## capital distance and political status descriptive statistics
pdf(here::here('Figures/dist_hist_excl_log.pdf'), width = 8, height = 4)
groups %>% mutate(excluded = as.numeric(status) <= 4,
                  cap_dist = log(cap_dist)) %>%
  select(excluded, cap_dist) %>% 
  ggplot(aes(x = cap_dist)) +
  geom_histogram(fill = wes_palette('GrandBudapest1', 1)) +
  labs(x = expression(italic(ln)~' Capital Distance'), y = 'Frequency') +
  facet_wrap(~ excluded, ncol = 1,
             labeller = as_labeller(c('FALSE' = 'Included',
                                      'TRUE' = 'Excluded'))) +
  theme_rw()
dev.off()

pdf(here::here('Figures/descriptives.pdf'), width = 8, height = 5)
groups_log %>% select(nl, cap_dist, pop_tot, border, area, excluded,
                      dom_overlap, family_downgraded_regaut5, oil, gdp,
                      v2x_polyarchy) %>% 
  gather() %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ key, scales = 'free', nrow = 3, labeller = desc_labs) +
  theme_rw()
dev.off()



## paper plots ####

## main text model 2 and 3 marginal effects plot
load(here::here('Figure Data/marg_eff_pop_df_cy.RData'))

pdf(here::here('Figures/pd_margs_pop_cy.pdf'), width = 8, height = 4)
margs_gg_pop %>%
  ggplot(aes(x = mod, y = pe, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  geom_rug(data = groups_log, aes(x = cap_dist), inherit.aes = F,
           alpha = .01) +
  labs(x = expression(italic(ln)~' Capital Distance'),
       y = expression('Marginal Effect of '~italic(ln)~' Population')) +
  facet_wrap(~ model, labeller = as_labeller(c('1' = 'Model 2',
                                               '2' = 'Model 3'))) +
  theme_rw()
dev.off()

## main text model 2 and 3 marginal effects plot
load(here:::here('Figure Data/marg_eff_bord_df_cy.RData'))

pdf(here::here('Figures/pd_margs_bord_cy.pdf'), width = 8, height = 4)
margs_gg_bord %>%
  ggplot(aes(x = mod, y = pe, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  geom_rug(data = groups_log, aes(x = cap_dist), inherit.aes = F,
           alpha = .01) +
  labs(x = expression(italic(ln)~' Capital Distance'),
       y = 'Marginal Effect of Border') +
  facet_wrap(~ model, labeller = as_labeller(c('1' = 'Model 5',
                                               '2' = 'Model 6'))) +
  theme_rw()
dev.off()


## appendix population and border prior sensitivity plots ####

load(here::here('Figure Data/marg_eff_pop_prior_df_cy.RData'))

pdf(here::here('Figures/pd_margs_pop_prior_cy.pdf'), width = 8, height = 4)
margs_gg_pop_prior %>%
  ggplot(aes(x = mod, y = pe, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  geom_rug(data = groups_log, aes(x = cap_dist), inherit.aes = F,
           alpha = .01) +
  labs(x = expression(italic(ln)~' Capital Distance'),
       y = expression('Marginal Effect of '~italic(ln)~' Population')) +
  facet_wrap(~ model, labeller = as_labeller(c('1' = 'Narrow priors',
                                               '2' = 'Wide priors'))) +
  theme_rw()
dev.off()

load(here::here('Figure Data/marg_eff_bord_prior_df_cy.RData'))

pdf(here::here('Figures/pd_margs_bord_prior_cy.pdf'), width = 8, height = 4)
margs_gg_bord_prior %>%
  ggplot(aes(x = mod, y = pe, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  geom_rug(data = groups_log, aes(x = cap_dist), inherit.aes = F,
           alpha = .01) +
  labs(expression(italic(ln)~' Capital Distance'),
       y = 'Marginal Effect of Border') +
  facet_wrap(~ model, labeller = as_labeller(c('1' = 'Narrow priors',
                                               '2' = 'Wide priors'))) +
  theme_rw()
dev.off()



## appendix population gini marginal effect plots ####

load(here::here('Figure Data/marg_eff_pop_gini_df_cy.RData'))

pdf(here::here('Figures/pd_margs_pop_gini_cy.pdf'), width = 8, height = 4)
margs_gg %>%
  ggplot(aes(x = mod, y = pe, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  geom_rug(data = groups_log, aes(x = cap_dist), inherit.aes = F,
           alpha = .01) +
  labs(x = expression(italic(ln)~' Capital Distance'),
       y = 'Marginal Effect of Population Gini') +
  facet_wrap(~ model, labeller = as_labeller(c('1' = 'Model G.2',
                                               '2' = 'Model G.3'))) +
  theme_rw()
dev.off()

rm(margs_gg)



## appendix excluded groups marginal effect plots ####

## create logged and lagged variables for models
groups_log_excl <- groups %>% select(nl, pop_tot, cap_dist, area, gdp, status) %>% 
  mutate_at(vars(-status), log) %>%
  cbind(groups %>% select(gwgroupid, year, state_ind, year_ind, state_year_ind), .,
        groups %>% select(v2x_polyarchy, excluded, family_downgraded_regaut5,
                          dom_overlap, border)) %>% 
  group_by(gwgroupid) %>% 
  mutate_at(vars(pop_tot:dom_overlap), ~lag(., order_by = year)) %>% 
  filter(year >= 1992, !is.na(pop_tot), # drop NAs from lagging
         !status %in% c('IRRELEVANT', 'DOMINANT', 'MONOPOLY')) %>% # drop excluded
  data.frame()

load(here::here('Figure Data/marg_eff_pop_df_excl_cy.RData'))

pdf(here::here('Figures/pd_margs_pop_excl_cy.pdf'), width = 8, height = 4)
margs_gg_pop %>%
  ggplot(aes(x = mod, y = pe, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  geom_rug(data = groups_log_excl, aes(x = cap_dist), inherit.aes = F,
           alpha = .01) +
  labs(x = expression(italic(ln)~' Capital Distance'),
       y = expression('Marginal Effect of '~italic(ln)~' Population')) +
  facet_wrap(~ model, labeller = as_labeller(c('1' = 'Model G.2',
                                               '2' = 'Model G.3'))) +
  theme_rw()
dev.off()

load(here::here('Figure Data/marg_eff_bord_df_excl_cy.RData'))

pdf(here::here('Figures/pd_margs_bord_excl_cy.pdf'), width = 8, height = 4)
margs_gg_bord %>%
  ggplot(aes(x = mod, y = pe, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  geom_rug(data = groups_log_excl, aes(x = cap_dist), inherit.aes = F,
           alpha = .01) +
  labs(expression(italic(ln)~' Capital Distance'),
       y = 'Marginal Effect of Border') +
  facet_wrap(~ model, labeller = as_labeller(c('1' = 'Model G.5',
                                               '2' = 'Model G.6'))) +
  theme_rw()
dev.off()



## appendix marginalized groups marginal effect plots ####

## create logged and lagged variables for models
groups_log_excl2 <- groups %>% select(nl, pop_tot, cap_dist, area, gdp, status) %>% 
  mutate_at(vars(-status), log) %>%
  cbind(groups %>% select(gwgroupid, year, state_ind, year_ind, state_year_ind), .,
        groups %>% select(v2x_polyarchy, excluded, family_downgraded_regaut5,
                          dom_overlap, border)) %>% 
  group_by(gwgroupid) %>% 
  mutate_at(vars(pop_tot:dom_overlap), ~lag(., order_by = year)) %>% 
  filter(year >= 1992, !is.na(pop_tot), # drop NAs from lagging
         !status %in% c('IRRELEVANT', 'DOMINANT', 'MONOPOLY',
                        'SENIOR PARTNER', 'JUNIOR PARTNER')) %>% # drop really excluded
  data.frame()

load(here::here('Figure Data/marg_eff_pop_df_excl2_cy.RData'))

pdf(here::here('Figures/pd_margs_pop_excl2_cy.pdf'), width = 8, height = 4)
margs_gg_pop %>%
  ggplot(aes(x = mod, y = pe, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  geom_rug(data = groups_log_excl2, aes(x = cap_dist), inherit.aes = F,
           alpha = .01) +
  labs(x = expression(italic(ln)~' Capital Distance'),
       y = expression('Marginal Effect of '~italic(ln)~' Population')) +
  facet_wrap(~ model, labeller = as_labeller(c('1' = 'Model G.8',
                                               '2' = 'Model G.9'))) +
  theme_rw()
dev.off()

## main text model 2 and 3 marginal effects plot
load(here::here('Figure Data/marg_eff_bord_df_excl2_cy.RData'))

pdf(here::here('Figures/pd_margs_bord_excl2_cy.pdf'), width = 8, height = 4)
margs_gg_bord %>%
  ggplot(aes(x = mod, y = pe, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  geom_rug(data = groups_log_excl2, aes(x = cap_dist), inherit.aes = F,
           alpha = .01) +
  labs(expression(italic(ln)~' Capital Distance'),
       y = 'Marginal Effect of Border') +
  facet_wrap(~ model, labeller = as_labeller(c('1' = 'Model G.11',
                                               '2' = 'Model G.12'))) +
  theme_rw()
dev.off()



## placebo plots ####

## load marginal effects dataframes for main, GADM, and grid population models
load(here::here('Figure Data/marg_eff_pop_df_cy.RData'))
margs_gg_pop <- margs_gg_pop %>%
  filter(model == 1) %>%
  select(-model) %>% 
  rename(mean = pe) %>% 
  mutate(model = 'main')

load(here::here('Figure Data/marg_eff_pop_df_gadm_cy.RData'))
margs_interactive_pop_gadm <- margs_interactive_pop_gadm %>% mutate(model = 'gadm')

load(here::here('Figure Data/marg_eff_pop_df_grid_cy.RData'))
margs_interactive_pop_grid <- margs_interactive_pop_grid %>% mutate(model = 'grid')


pdf(here::here('Figures/pd_margs_placebo_pop_cy.pdf'), width = 8, height = 3)
mget(ls(pattern = 'margs_interactive_pop|margs_gg_pop$')) %>%
  bind_rows() %>% 
  mutate(model = factor(model, levels = c('main', 'gadm', 'grid'))) %>% 
  ggplot(aes(x = mod, y = mean, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  labs(x = expression(italic(ln)~' Capital Distance'),
       y = expression('Marginal Effect of '~italic(ln)~' Population')) +
  facet_wrap(~ model, labeller = as_labeller(c('main' = 'Ethnic Groups',
                                               'gadm' = 'Administrative Units',
                                               'grid' = 'PRIO GRID')),
             scales = 'free_x') +
  theme_rw()
dev.off()

## load marginal effects dataframes for main, GADM, and grid border models
load(here::here('Figure Data/marg_eff_bord_df_cy.RData'))
margs_gg_bord <- margs_gg_bord %>%
  filter(model == 1) %>%
  select(-model) %>% 
  rename(mean = pe) %>% 
  mutate(model = 'main')

load(here::here('Figure Data/marg_eff_bord_df_gadm_cy.RData'))
margs_interactive_bord_gadm <- margs_interactive_bord_gadm %>% mutate(model = 'gadm')

load(here::here('Figure Data/marg_eff_bord_df_grid_cy.RData'))
margs_interactive_bord_grid <- margs_interactive_bord_grid %>% mutate(model = 'grid')


pdf(here::here('Figures/pd_margs_placebo_bord_cy.pdf'), width = 8, height = 3)
mget(ls(pattern = 'margs_interactive_bord|margs_gg_bord$')) %>%
  bind_rows() %>%
  mutate(model = factor(model, levels = c('main', 'gadm', 'grid'))) %>% 
  ggplot(aes(x = mod, y = mean, ymin = lo, ymax = hi)) +
  geom_ribbon(fill = wes_palette('GrandBudapest1', 1), alpha = 0.25) + 
  geom_line(color = wes_palette('GrandBudapest1', 1)) +
  labs(expression(italic(ln)~' Capital Distance'),
       y = 'Marginal Effect of Border') +
  facet_wrap(~ model, labeller = as_labeller(c('main' = 'Ethnic Groups',
                                               'gadm' = 'Administrative Units',
                                               'grid' = 'PRIO GRID')),
             scales = 'free_x') +
  theme_rw()
dev.off()




## quit R
quit(save = 'no')

###################
## end of script ##
###################