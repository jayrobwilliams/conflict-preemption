###############################
## author: Rob Williams      ##
## project: dissertation     ##
## created: February 4, 2018 ##
## updated: Janurary 2, 2019 ##
###############################

## this script executes panel data bayesian linear regressions models of
## nightlights in ethnic group territories with country and year random effects

## print script to identify in log
print(paste('Nightlights Border Grid Analysis Started', Sys.time()))

## load packages
library(tidyverse)
library(brms)
library(BayesPostEst)
library(future)
plan(multicore(workers = max(4, as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')),
                             na.rm = T)))


## read in grid data
grid <- readRDS(here::here('Input Data/grid data.RDS'))

## create time state indicator vector for random intercepts
grid <- grid %>% mutate(state_ind = as.numeric(as.factor(gwid)),
                        state_year_ind = as.numeric(as.factor(state_ind * 1e5 + year)),
                        year_ind = year - 1989)

## create logged and lagged variables for models
grid_log <- grid %>%
  mutate_at(vars(nl, cap_dist, area), log) %>%
  group_by(id) %>% 
  mutate_at(vars(border, cap_dist, area), ~lag(., order_by = year)) %>% 
  filter(year >= 1992, !is.na(border)) %>% # drop NAs from lagging
  data.frame()



## define model priors and hyperpriors
mod_priors <- set_prior('normal(mu_beta, sigma_beta)', class = 'b') +
  set_prior('normal(0, 5)', class = 'Intercept') +
  set_prior('target += normal_lpdf(mu_beta | 0, 5)', check = F) +
  set_prior('target += cauchy_lpdf(sigma_beta | 0, 2.5)', check = F) +
  set_prior('cauchy(0, 2.5)', class = 'sd', group = 'state_ind') +
  set_prior('cauchy(0, 2.5)', class = 'sd', group = 'year_ind')

## add hyperpriors to stan model code
mod_stanvars <- stanvar(scode = '  real mu_beta; // mean of regression coefficients',
                        block = 'parameters') +
  stanvar(scode = '  real<lower=0.001> sigma_beta; // std of regression coefficients',
          block = 'parameters')



## interactive models ####

## interactive border model
mod_int_bord <- brm(brmsformula(nl ~ cap_dist + border + cap_dist:border +
                                  area + (1 | state_ind) + (1 | year_ind),
                                center = T),
                    data = grid_log, family = gaussian(), prior = mod_priors,
                    stanvars = mod_stanvars, iter = 4000, chains = 4,
                    save_dso = T, control = list(adapt_delta = .95),
                    seed = 1234, future = T,
                    file = here::here('Stanfits/pd_lm_int_bord_grid_cy'))



## marginal effects plots ####

## get marginal effects for interactive models
margs_interactive_bord_grid <- mcmcMargEff(mod_int_bord, 'b_border',
                                           'b_cap_dist:border',
                                           scale(grid_log$cap_dist,
                                                 center = T, scale = F),
                                           plot = F)

## write marginal effects plot dataframe to disk
save(margs_interactive_bord_grid, file = here::here('Figure Data/marg_eff_bord_df_grid_cy.RData'))



## print script to verify successful execution in log
print(paste('Nightlights Border Grid Analysis Completed', Sys.time()))

## quit R
quit(save = 'no')

###################
## end of script ##
###################