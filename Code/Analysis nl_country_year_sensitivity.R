###############################
## author: Rob Williams      ##
## project: dissertation     ##
## created: February 4, 2018 ##
## updated: Janurary 2, 2019 ##
###############################

## this script executes panel data bayesian linear regressions models of
## nightlights in ethnic group territories with country and year random effects

## print script to identify in log
print(paste('Nightlights Sensitivity Analysis Started', Sys.time()))

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
library(BayesPostEst)
library(future)
plan(multicore(workers = max(4, as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')),
                             na.rm = T)))
kfold_rmse <- function(kf) {
  
  kfp <- kfold_predict(kf)
  pr <- kf$fits[, 'predicted']
  sapply(pr, function(x) sqrt(mean((colMeans(kfp$yrep[, x]) - kfp$y[x])^2)))
  
}

## load goals object
groups <- readRDS(here::here('Input Data/groups_nightlights.RDS'))

## create logged and scaled variables for models
groups_log <- groups %>% select(nl, pop_tot, cap_dist, area, gdp) %>% 
  mutate_all(log) %>%
  cbind(groups %>% select(gwgroupid, year, state_ind, year_ind, state_year_ind), .,
        groups %>% select(v2x_polyarchy, excluded, family_downgraded_regaut5,
                          dom_overlap, border, oil)) %>% 
  group_by(gwgroupid) %>% 
  mutate_at(vars(pop_tot:oil), ~lag(., order_by = year)) %>% 
  filter(year >= 1992, !is.na(pop_tot)) %>% # drop NAs from lagging
  data.frame()

## full controls model
groups_mi <- mice(groups_log, pred = quickpred(data = groups_log,
                                               exclude = c('gwgroupid',
                                                           'year',
                                                           'state_ind',
                                                           'state_year_ind',
                                                           'time')))

## extract each imputed dataset from MIDS object
groups_list <- complete(groups_mi, action = 'long')

## split imputed datasets into list
groups_list <- split(groups_list, rep(1:5, each = nrow(groups_log)))

## register parallel backend
library(doParallel)
registerDoParallel(max(2, as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')) %/% 4, na.rm = T))

## add hyperpriors to stan model code
mod_stanvars <- stanvar(scode = '  real mu_beta; // mean of regression coefficients',
                        block = 'parameters') +
  stanvar(scode = '  real<lower=0.001> sigma_beta; // std of regression coefficients',
          block = 'parameters')



## narrower priors ####

## define model priors and hyperpriors
mod_priors <- set_prior('normal(mu_beta, sigma_beta)', class = 'b') +
  set_prior('target += normal_lpdf(mu_beta | 0, 2.5)', check = F) +
  set_prior('target += cauchy_lpdf(sigma_beta | 0, 1)', check = F) +
  set_prior('cauchy(0, 1)', class = 'sd', group = 'state_ind') +
  set_prior('cauchy(0, 1)', class = 'sd', group = 'year_ind')

## full controls population model
mod_int_pop_lo_controls_list <- foreach(i = 1:length(groups_list), .packages = 'brms') %dopar% {
  
  brm(brmsformula(nl ~ cap_dist + pop_tot + cap_dist:pop_tot + area + excluded +
                    family_downgraded_regaut5 + dom_overlap + gdp + oil +
                    v2x_polyarchy + (1 | state_ind) + (1 | year_ind),
                  center = F),
      data = groups_list[[i]], family = gaussian(), prior = mod_priors,
      stanvars = mod_stanvars,
      iter = 4000, chains = 2, save_dso = T, save_ranef = T, cores = 2,
      control = list(adapt_delta = .95), seed = 1234,
      file = here::here('Stanfits', paste0('pd_lm_int_pop_controls_cy_prior_lo_', i)))
  
}

## combine list for tables and figures
mod_int_pop_lo_controls <- combine_models(mlist = mod_int_pop_lo_controls_list, check_data = F)

## full controls border model
mod_int_bord_lo_controls_list <- foreach(i = 1:length(groups_list), .packages = 'brms') %dopar% {
  
  brm(brmsformula(nl ~ cap_dist + border + cap_dist:border + area + excluded +
                    family_downgraded_regaut5 + dom_overlap + gdp + oil +
                    v2x_polyarchy + (1 | state_ind) + (1 | year_ind),
                  center = F),
      data = groups_list[[i]], family = gaussian(), prior = mod_priors,
      stanvars = mod_stanvars,
      iter = 4000, chains = 2, save_dso = T, save_ranef = T, cores = 2,
      control = list(adapt_delta = .95), seed = 1234,
      file = here::here('Stanfits', paste0('pd_lm_int_bord_controls_cy_prior_lo_', i)))
  
}

## combine list for tables and figures
mod_int_bord_lo_controls <- combine_models(mlist = mod_int_bord_lo_controls_list, check_data = F)




## wider priors ####

## define model priors and hyperpriors
mod_priors <- set_prior('normal(mu_beta, sigma_beta)', class = 'b') +
  set_prior('target += normal_lpdf(mu_beta | 0, 10)', check = F) +
  set_prior('target += cauchy_lpdf(sigma_beta | 0, 4)', check = F) +
  set_prior('cauchy(0, 5)', class = 'sd', group = 'state_ind') +
  set_prior('cauchy(0, 5)', class = 'sd', group = 'year_ind')

## full controls population model
mod_int_pop_hi_controls_list <- foreach(i = 1:length(groups_list), .packages = 'brms') %dopar% {
  
  brm(brmsformula(nl ~ cap_dist + pop_tot + cap_dist:pop_tot + area + excluded +
                    family_downgraded_regaut5 + dom_overlap + gdp + oil +
                    v2x_polyarchy + (1 | state_ind) + (1 | year_ind),
                  center = F),
      data = groups_list[[i]], family = gaussian(), prior = mod_priors,
      stanvars = mod_stanvars,
      iter = 4000, chains = 2, save_dso = T, save_ranef = T, cores = 2,
      control = list(adapt_delta = .95), seed = 1234,
      file = here::here('Stanfits', paste0('pd_lm_int_pop_controls_cy_prior_hi_', i)))
  
}

## combine list for tables and figures
mod_int_pop_hi_controls <- combine_models(mlist = mod_int_pop_hi_controls_list, check_data = F)

## full controls population model
mod_int_bord_hi_controls_list <- foreach(i = 1:length(groups_list), .packages = 'brms') %dopar% {
  
  brm(brmsformula(nl ~ cap_dist + border + cap_dist:border + area + excluded + 
                    family_downgraded_regaut5 + dom_overlap + gdp + oil +
                    v2x_polyarchy + (1 | state_ind) + (1 | year_ind),
                  center = F),
      data = groups_list[[i]], family = gaussian(), prior = mod_priors,
      stanvars = mod_stanvars,
      iter = 4000, chains = 2, save_dso = T, save_ranef = T, cores = 2,
      control = list(adapt_delta = .95), seed = 1234,
      file = here::here('Stanfits', paste0('pd_lm_int_bord_controls_cy_prior_hi_', i)))
  
}

## combine list for tables and figures
mod_int_bord_hi_controls <- combine_models(mlist = mod_int_bord_hi_controls_list, check_data = F)




## get marginal effects for low prior population model
margs_controls_pop_lo <- mcmcMargEff(mod_int_pop_lo_controls, 'b_pop_tot', 'b_cap_dist:pop_tot',
                                     scale(groups_log$cap_dist), plot = F)

## get marginal effects for low prior border model
margs_controls_bord_lo <- mcmcMargEff(mod_int_bord_lo_controls, 'b_borderTRUE', 'b_cap_dist:borderTRUE',
                                      scale(groups_log$cap_dist), plot = F)

## get marginal effects for high prior population model
margs_controls_pop_hi <- mcmcMargEff(mod_int_pop_hi_controls, 'b_pop_tot', 'b_cap_dist:pop_tot',
                                     scale(groups_log$cap_dist), plot = F)

## get marginal effects for high prior border model
margs_controls_bord_hi <- mcmcMargEff(mod_int_bord_hi_controls, 'b_borderTRUE', 'b_cap_dist:borderTRUE',
                                      scale(groups_log$cap_dist), plot = F)


## combine marginal effects from narrow and wide prior population models
margs_gg_pop_prior <- rbind(data.frame(margs_controls_pop_lo, model = 1),
                            data.frame(margs_controls_pop_hi, model = 2))

save(margs_gg_pop_prior,
     file = here::here('Figure Data/marg_eff_pop_prior_df_cy.RData'))

## combine marginal effects from narrow and wide prior population models
margs_gg_bord_prior <- rbind(data.frame(margs_controls_bord_lo, model = 1),
                             data.frame(margs_controls_bord_hi, model = 2))

save(margs_gg_bord_prior,
     file = here::here('Figure Data/marg_eff_bord_prior_df_cy.RData'))



## print script to verify successful execution in log
print(paste('Nightlights Sensitivity Analysis Completed', Sys.time()))

## quit R
quit(save = 'no')

###################
## end of script ##
###################