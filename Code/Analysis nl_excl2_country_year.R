## this script executes panel data bayesian linear regressions models of
## nightlights in ethnic group territories with country and year random effects
## limited to politically marginalized groups

## print script to identify in log
print(paste('Nightlights Analysis Started', Sys.time()))

## load packages
library(tidyverse)
library(mice)
library(brms)
library(loo)
library(xtable)
library(BayesPostEst)
library(future)
plan(multicore(workers = max(4, as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')),
                             na.rm = T)))
kfold_rmse <- function(kf) {
  
  kfp <- kfold_predict(kf)
  pr <- kf$fits[, 'predicted']
  sapply(pr, function(x) sqrt(mean((colMeans(kfp$yrep[, x]) - kfp$y[x])^2)))
  
}
slurm_cores <- max(4, as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')), na.rm = T)

## load group data object
groups <- readRDS(here::here('Input Data/groups_nightlights.RDS'))

## create logged and lagged variables for models
groups_log <- groups %>% select(nl, pop_tot, cap_dist, area, gdp, status) %>% 
  mutate_at(vars(-status), log) %>%
  cbind(groups %>% select(gwgroupid, year, state_ind, year_ind, state_year_ind), .,
        groups %>% select(v2x_polyarchy, family_downgraded_regaut5,
                          dom_overlap, border, oil)) %>% 
  group_by(gwgroupid) %>% 
  mutate_at(vars(pop_tot:oil), ~lag(., order_by = year)) %>% 
  filter(year >= 1992, !is.na(pop_tot), # drop NAs from lagging
         !status %in% c('IRRELEVANT', 'DOMINANT', 'MONOPOLY',
                        'SENIOR PARTNER', 'JUNIOR PARTNER')) %>% # drop really excluded
  data.frame()

## coefficient map for regression tables
tab_map <- list('b_pop_tot' = '\\emph{ln} Population',
                'b_borderTRUE' = 'Border',
                'b_cap_dist' = '\\emph{ln} Capital Distance',
                'b_cap_dist:pop_tot' = '\\emph{ln} Population $\\times$ \\emph{ln} Capital Distance',
                'b_cap_dist:borderTRUE' = 'Border $\\times$ \\emph{ln} Capital Distance',
                'b_area' = '\\emph{ln} Area',
                'b_excludedTRUE' = 'Excluded',
                'b_dom_overlap' = 'Dominant Group Presence',
                'b_family_downgraded_regaut5' = 'Lost Autonomy',
                'b_oil' = 'Oil',
                'b_gdp' = '\\emph{ln} GDP$_\\text{PC}$',
                'b_v2x_polyarchy' = 'Polyarchy',
                'b_Intercept' = '(Constant)',
                'sd_state_ind__Intercept' = '$\\sigma_\\alpha$',
                'sd_year_ind__Intercept' = '$\\sigma_\\gamma$')

## define model priors and hyperpriors
mod_priors <- set_prior('normal(mu_beta, sigma_beta)', class = 'b') +
  set_prior('target += normal_lpdf(mu_beta | 0, 5)', check = F) +
  set_prior('target += cauchy_lpdf(sigma_beta | 0, 2.5)', check = F) +
  set_prior('cauchy(0, 2.5)', class = 'sd', group = 'state_ind') +
  set_prior('cauchy(0, 2.5)', class = 'sd', group = 'year_ind')

## add hyperpriors to stan model code
mod_stanvars <- stanvar(scode = '  real mu_beta; // mean of regression coefficients',
                        block = 'parameters') +
  stanvar(scode = '  real<lower=0.001> sigma_beta; // std of regression coefficients',
          block = 'parameters')



## bivariate models ####

## bivariate Population
mod_bivar_pop <- brm(brmsformula(nl ~ pop_tot + (1 | state_ind) +
                                   (1 | year_ind), center = F),
                     data = groups_log, family = gaussian(), prior = mod_priors,
                     stanvars = mod_stanvars,
                     iter = 4000, chains = 4, save_dso = T, save_ranef = T,
                     control = list(adapt_delta = .95), seed = 1234,
                     future = T, file = here::here('Stanfits/pd_lm_pop_excl2_cy'))

## calculate WAIC
mod_bivar_pop_waic <- waic(mod_bivar_pop, cores = slurm_cores)

## calculate k fold crossvalidation information criterion
mod_bivar_pop_kfold <- kfold(mod_bivar_pop, K = 5, folds = 'stratified',
                             group = 'state_ind', chains = 4, iter = 2000,
                             seed = 1234, save_fits = T)

## calculate RMSE for each fold
mod_bivar_pop_rmse <- kfold_rmse(mod_bivar_pop_kfold)

## bivariate border total
mod_bivar_bord <- brm(brmsformula(nl ~ border + (1 | state_ind) +
                                    (1 | year_ind), center = F),
                      data = groups_log, family = gaussian(), prior = mod_priors,
                      stanvars = mod_stanvars,
                      iter = 4000, chains = 4, save_dso = T, save_ranef = T,
                      control = list(adapt_delta = .95), seed = 1234,
                      future = T, file = here::here('Stanfits/pd_lm_bord_excl2_cy'))

## calculate WAIC
mod_bivar_bord_waic <- waic(mod_bivar_bord, cores = slurm_cores)

## calculate k fold crossvalidation information criterion
mod_bivar_bord_kfold <- kfold(mod_bivar_bord, K = 5, folds = 'stratified',
                              group = 'state_ind', chains = 4, iter = 2000,
                              seed = 1234, save_fits = T)

## calculate RMSE for each fold
mod_bivar_bord_rmse <- kfold_rmse(mod_bivar_bord_kfold)



## interactive models ####

## interactive population model
mod_int_pop <- brm(brmsformula(nl ~ cap_dist + pop_tot + cap_dist:pop_tot +
                                 area + (1 | state_ind) + (1 | year_ind),
                               center = F),
                   data = groups_log, family = gaussian(), prior = mod_priors,
                   stanvars = mod_stanvars,
                   iter = 4000, chains = 4, save_dso = T, save_ranef = T,
                   control = list(adapt_delta = .95), seed = 1234,
                   future = T, file = here::here('Stanfits/pd_lm_int_pop_excl2_cy'))

## calculate WAIC
mod_int_pop_waic <- waic(mod_int_pop, cores = slurm_cores)

## calculate k fold crossvalidation information criterion
mod_int_pop_kfold <- kfold(mod_int_pop, K = 5, folds = 'stratified',
                           group = 'state_ind', chains = 4, iter = 2000,
                           seed = 1234, save_fits = T)

## calculate RMSE for each fold
mod_int_pop_rmse <- kfold_rmse(mod_int_pop_kfold)

## interactive border model
mod_int_bord <- brm(brmsformula(nl ~ cap_dist + border + cap_dist:border +
                                  area + (1 | state_ind) + (1 | year_ind),
                                center = F),
                    data = groups_log, family = gaussian(), prior = mod_priors,
                    stanvars = mod_stanvars,
                    iter = 4000, chains = 4, save_dso = T, save_ranef = T,
                    control = list(adapt_delta = .95), seed = 1234,
                    future = T, file = here::here('Stanfits/pd_lm_int_bord_excl2_cy'))

## calculate WAIC
mod_int_bord_waic <- waic(mod_int_bord, cores = slurm_cores)

## calculate k fold crossvalidation information criterion
mod_int_bord_kfold <- kfold(mod_int_bord, K = 5, folds = 'stratified',
                            group = 'state_ind', chains = 4, iter = 2000,
                            seed = 1234, save_fits = T)

## calculate RMSE for each fold
mod_int_bord_rmse <- kfold_rmse(mod_int_bord_kfold)

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

## full controls population model
mod_int_pop_controls_list <- foreach(i = 1:length(groups_list), .packages = 'brms') %dopar% {
  
  brm(brmsformula(nl ~ cap_dist + pop_tot + cap_dist:pop_tot + area +
                    family_downgraded_regaut5 + dom_overlap + gdp + oil +
                    v2x_polyarchy + (1 | state_ind) + (1 | year_ind),
                  center = F),
      data = groups_list[[i]], family = gaussian(), prior = mod_priors,
      stanvars = mod_stanvars,
      iter = 4000, chains = 2, save_dso = T, save_ranef = T, cores = 2,
      control = list(adapt_delta = .95), seed = 1234,
      file = here::here('Stanfits', paste0('pd_lm_int_pop_controls_excl2_cy_', i)))
  
}

## combine list for tables and figures
mod_int_pop_controls <- combine_models(mlist = mod_int_pop_controls_list, check_data = F)

## save combined brmsfit object
saveRDS(mod_int_pop_controls,
        here::here('Stanfits/pd_lm_int_pop_controls_excl2_cy.rds'))

## save list of brmsfits for debugging
saveRDS(mod_int_pop_controls_list,
        here::here('Stanfits/pd_lm_int_pop_controls_list_excl2_cy.rds'))

## calculate WAIC
mod_int_pop_controls_waic <- waic(mod_int_pop_controls, cores = slurm_cores)

## calculate RMSE for each fold for each imputed dataset
mod_int_pop_controls_rmse <- foreach(i = mod_int_pop_controls_list, .packages = 'brms') %dopar% {
  
  kf <- kfold(i, K = 5, folds = 'stratified',
              group = 'state_ind', chains = 4, iter = 2000, seed = 1234,
              save_fits = T)
  kfold_rmse(kf)
  
}

## full controls border model
mod_int_bord_controls_list <- foreach(i = 1:length(groups_list), .packages = 'brms') %dopar% {
  
  brm(brmsformula(nl ~ cap_dist + border + cap_dist:border + area +
                    family_downgraded_regaut5 + dom_overlap + gdp + oil +
                    v2x_polyarchy + (1 | state_ind) + (1 | year_ind),
                  center = F),
      data = groups_list[[i]], family = gaussian(), prior = mod_priors,
      stanvars = mod_stanvars,
      iter = 4000, chains = 2, save_dso = T, save_ranef = T, cores = 2,
      control = list(adapt_delta = .95), seed = 1234,
      file = here::here('Stanfits', paste0('pd_lm_int_bord_controls_excl2_cy_', i)))
  
}

## combine list for tables and figures
mod_int_bord_controls <- combine_models(mlist = mod_int_bord_controls_list, check_data = F)

## save combined brmsfit object
saveRDS(mod_int_bord_controls,
        here::here('Stanfits/pd_lm_int_bord_controls_excl2_cy.rds'))

## save list of brmsfits for debugging
saveRDS(mod_int_bord_controls_list,
        here::here('Stanfits/pd_lm_int_bord_controls_list_excl2_cy.rds'))

## calculate WAIC
mod_int_bord_controls_waic <- waic(mod_int_bord_controls, cores = slurm_cores)

## calculate RMSE for each fold for each imputed dataset
mod_int_bord_controls_rmse <- foreach(i = mod_int_bord_controls_list, .packages = 'brms') %dopar% {
  
  kf <- kfold(i, K = 5, folds = 'stratified',
              group = 'state_ind', chains = 4, iter = 2000, seed = 1234,
              save_fits = T)
  kfold_rmse(kf)
  
}



## tables ####

## population models table 
tabstr <- mcmcReg(list(mod_bivar_pop, mod_int_pop, mod_int_pop_controls),
                  custom.coef.map = tab_map,
                  custom.model.names = paste0('Model G.', 7:9),
                  gof = list(c(mod_bivar_pop_waic$estimates['waic', 'Estimate'],
                               mean(mod_bivar_pop_rmse)),
                             c(mod_int_pop_waic$estimates['waic', 'Estimate'],
                               mean(mod_int_pop_rmse)),
                             c(mod_int_pop_controls_waic$estimates['waic', 'Estimate'],
                               mean(unlist(mod_int_pop_controls_rmse)))),
                  gofnames = list(c('WAIC', '5-fold RMSE'),
                                  c('WAIC', '5-fold RMSE'),
                                  c('WAIC', '5-fold RMSE')),
                  caption = 'Linear models explaining nightlights as a function of marginalized ethnic group population and capital distance. The standard deviation of the country and year random intercepts are represented by $\\sigma_\\alpha$ and $\\sigma_\\gamma$, respectively. Continuous variables logged and standarized.',
                  label = 'tab:nl_pop_excl2', float.pos = 'ht!')

## extract LaTeX tabular command and use to count columns in table
tab <- regexpr('\\\\begin\\{tabular\\}.* \\}', tabstr)
tab <- substr(tabstr, tab, tab + attr(tab, 'match.length'))
tab <- lengths(regmatches(tab, gregexpr(" ", tab)))

## add horizonal line separating random effect standard deviations
tabstr <- sub('\\$\\\\sigma', '\\\\hline\n\\$\\\\sigma', tabstr)

## add in number of observations
tabstr <- sub('\\\\hline\\n\\\\multicolumn',
              paste0('Observations & ',
                     paste(rep(nrow(groups_log),
                               times = tab - 1),
                           collapse = ' & '),
                     ' \\\\\\\\\n\\\\hline\n\\\\multicolumn'),
              tabstr)

fileConn <- file(here::here('Tables/pd_pop_excl2_cy.tex'))
writeLines(tabstr, fileConn)
close(fileConn)

## border models table 
tabstr <- mcmcReg(list(mod_bivar_bord, mod_int_bord, mod_int_bord_controls),
                  custom.coef.map = tab_map,
                  custom.model.names = paste0('Model G.', 10:12),
                  gof = list(c(mod_bivar_bord_waic$estimates['waic', 'Estimate'],
                               mean(mod_bivar_bord_rmse)),
                             c(mod_int_bord_waic$estimates['waic', 'Estimate'],
                               mean(mod_int_bord_rmse)),
                             c(mod_int_bord_controls_waic$estimates['waic', 'Estimate'],
                               mean(unlist(mod_int_bord_controls_rmse)))),
                  gofnames = list(c('WAIC', '5-fold RMSE'),
                                  c('WAIC', '5-fold RMSE'),
                                  c('WAIC', '5-fold RMSE')),
                  caption = 'Linear models explaining nightlights as a function of marginalized ethnic group border and capital distance. The standard deviation of the country and year random intercepts are represented by $\\sigma_\\alpha$ and $\\sigma_\\gamma$, respectively. Continuous variables logged and standarized.',
                  label = 'tab:nl_bord_excl2', float.pos = 'ht!')

## extract LaTeX tabular command and use to count columns in table
tab <- regexpr('\\\\begin\\{tabular\\}.* \\}', tabstr)
tab <- substr(tabstr, tab, tab + attr(tab, 'match.length'))
tab <- lengths(regmatches(tab, gregexpr(" ", tab)))

## add horizonal line separating random effect standard deviations
tabstr <- sub('\\$\\\\sigma', '\\\\hline\n\\$\\\\sigma', tabstr)

## add in number of observations
tabstr <- sub('\\\\hline\\n\\\\multicolumn',
              paste0('Observations & ',
                     paste(rep(nrow(groups_log),
                               times = tab - 1),
                           collapse = ' & '),
                     ' \\\\\\\\\n\\\\hline\n\\\\multicolumn'),
              tabstr)

fileConn <- file(here::here('Tables/pd_bord_excl2_cy.tex'))
writeLines(tabstr, fileConn)
close(fileConn)

## coefficients for independent and interactive effect in full model
samps <- rstan::extract(mod_int_pop_controls$fit, pars = c('b_pop_tot', 'b_cap_dist:pop_tot'))

## get .025 and .975 quantile of capital distance
cap_quant <- quantile(groups_log$cap_dist, probs = c(.025, .975))

## compute marginal effect for each sample
marg <- rep(samps[[1]], 3) + samps[[2]] %o% c(cap_quant[1], 0, cap_quant[2])

## take column means to get estimated marginal effects
marg_eff <- colMeans(marg)

## divide marginal effects by range of nighlights values to get pct change
marg_pct <- marg_eff / diff(range(groups_log$nl)) * 100

## write marginal effect differences
fileConn <- file(here::here('Tables/marg_eff_excl2_cy.txt'))
writeLines(paste0('The marginal effect of population on nightlights is ',
                  marg_eff[1],
                  ' (', marg_pct[1],
                  '%) at two standard deviations below the mean of capital distance',
                  '\nThe marginal effect of population on nightlights is ',
                  marg_eff[2],
                  ' (', marg_pct[2],
                  '%) at the mean of capital distance',
                  '\nThe marginal effect of population on nightlights is ',
                  marg_eff[3],
                  ' (', marg_pct[3],
                  '%) at two standard deviations above the mean of capital distance',
                  '\nThe shift in the marginal effect of population on nightlights in moving from two standard deviations below the mean of capital distance to two standard deviations above is ',
                  marg_eff[3] - marg_eff[1], ' (', marg_pct[3] - marg_pct[1], '%)'),
           fileConn)
close(fileConn)

## get marginal effects for interactive population models
margs_interactive_pop <- mcmcMargEff(mod_int_pop, 'b_pop_tot',
                                     'b_cap_dist:pop_tot',
                                     groups_log$cap_dist, plot = F)
margs_controls_pop <- mcmcMargEff(mod_int_pop_controls, 'b_pop_tot',
                                  'b_cap_dist:pop_tot',
                                  groups_log$cap_dist, plot = F)
margs_gg_pop <- rbind(data.frame(margs_interactive_pop, model = 1),
                      data.frame(margs_controls_pop, model = 2))

## write marginal effects plot dataframe to disk
save(margs_gg_pop,
     file = here::here('Figure Data/marg_eff_pop_df_excl2_cy.RData'))

## get marginal effects for interactive border models
margs_interactive_bord <- mcmcMargEff(mod_int_bord, 'b_borderTRUE',
                                      'b_cap_dist:borderTRUE',
                                      groups_log$cap_dist, plot = F)
margs_controls_bord <- mcmcMargEff(mod_int_bord_controls, 'b_borderTRUE',
                                   'b_cap_dist:borderTRUE',
                                   groups_log$cap_dist, plot = F)
margs_gg_bord <- rbind(data.frame(margs_interactive_bord, model = 1),
                       data.frame(margs_controls_bord, model = 2))

## write marginal effects plot dataframe to disk
save(margs_gg_bord,
     file = here::here('Figure Data/marg_eff_bord_df_excl2_cy.RData'))



## print script to verify successful execution in log
print(paste('Nightlights Analysis Completed', Sys.time()))

## quit R
quit(save = 'no')

###################
## end of script ##
###################