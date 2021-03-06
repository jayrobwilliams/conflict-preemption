## this script executes panel data bayesian linear regressions models of
## nightlights in ethnic group territories with country and year random effects
## using grid cells as the unit of analysis

## print script to identify in log
print(paste('Nightlights Population Grid Analysis Started', Sys.time()))

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
  mutate(pop_tot = log1p(pop)) %>% 
  group_by(id) %>% 
  mutate_at(vars(pop_tot, cap_dist, area), ~lag(., order_by = year)) %>% 
  filter(year >= 1992, !is.na(pop_tot)) %>% # drop NAs from lagging
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

## interactive population model
mod_int_pop <- brm(brmsformula(nl ~ cap_dist + pop_tot + cap_dist:pop_tot +
                                 area + (1 | state_ind) + (1 | year_ind),
                               center = F),
                   data = grid_log, family = gaussian(), prior = mod_priors,
                   stanvars = mod_stanvars, iter = 4000, chains = 4,
                   save_dso = T, control = list(adapt_delta = .95),
                   seed = 1234, future = T,
                   file = here::here('Stanfits/pd_lm_int_pop_grid_cy'))



## marginal effects plots ####

## get marginal effects for interactive models
margs_interactive_pop_grid <- mcmcMargEff(mod_int_pop, 'b_pop_tot',
                                          'b_cap_dist:pop_tot',
                                          scale(grid_log$cap_dist,
                                                center = T, scale = F),
                                          plot = F)

## write marginal effects plot dataframe to disk
save(margs_interactive_pop_grid,
     file = here::here('Figure Data/marg_eff_pop_df_grid_cy.RData'))



## print script to verify successful execution in log
print(paste('Nightlights Population Grid Analysis Completed', Sys.time()))

## quit R
quit(save = 'no')

###################
## end of script ##
###################