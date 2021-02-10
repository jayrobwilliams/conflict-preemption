###############################
## author: Rob Williams      ##
## project: dissertation     ##
## created: February 4, 2018 ##
## updated: June 9, 2019     ##
###############################

## this script executes panel data bayesian linear regressions models of
## nightlights in ethnic group territories with country and year random effects

## print script to identify in log
print(paste('Nightlights Analysis Using Population Gini Started', Sys.time()))

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
slurm_cores <- max(4, as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')), na.rm = T)

## load group data object
groups <- readRDS(here::here('Input Data/groups_nightlights.RDS'))

## create logged and lagged variables for models
groups_log <- groups %>% select(nl, pop_gini, cap_dist, area, gdp) %>% 
  mutate_all(log) %>%
  cbind(groups %>% select(gwgroupid, year, state_ind, year_ind, state_year_ind), .,
        groups %>% select(v2x_polyarchy, excluded, family_downgraded_regaut5,
                          dom_overlap, oil)) %>% 
  group_by(gwgroupid) %>% 
  mutate_at(vars(pop_gini:oil), ~lag(., order_by = year)) %>% 
  filter(year >= 1992, !is.na(pop_gini)) %>% # drop NAs from lagging
  data.frame()

## coefficient map for regression tables
tab_map <- list('b_pop_gini' = 'Population Gini',
                'b_borderTRUE' = 'Border',
                'b_cap_dist' = '\\emph{ln} Capital Distance',
                'b_cap_dist:pop_gini' = 'Population Gini $\\times$ \\emph{ln} Capital Distance',
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

# add positive prior on correlation later

## add hyperpriors to stan model code
mod_stanvars <- stanvar(scode = '  real mu_beta; // mean of regression coefficients',
                        block = 'parameters') +
  stanvar(scode = '  real<lower=0.001> sigma_beta; // std of regression coefficients',
          block = 'parameters')



## bivariate population gini model ####

## bivariate gini total
mod_bivar_gini <- brm(brmsformula(nl ~ pop_gini + (1 | state_ind) +
                                    (1 | year_ind), center = F),
                      data = groups_log, family = gaussian(), prior = mod_priors,
                      stanvars = mod_stanvars,
                      iter = 4000, chains = 4, save_dso = T, save_ranef = T,
                      control = list(adapt_delta = .95), seed = 1234,
                      future = T, file = here::here('Stanfits/pd_lm_gini_cy'))

## calculate WAIC
mod_bivar_gini_waic <- waic(mod_bivar_gini, cores = slurm_cores)

## calculate k fold crossvalidation information criterion
mod_bivar_gini_kfold <- kfold(mod_bivar_gini, K = 5, folds = 'stratified',
                              group = 'state_ind', chains = 4, iter = 2000,
                              seed = 1234, save_fits = T)

## calculate RMSE for each fold
mod_bivar_gini_rmse <- kfold_rmse(mod_bivar_gini_kfold)



## interactive population models ####

## population total interactive model
mod_int <- brm(brmsformula(nl ~ cap_dist + pop_gini + cap_dist:pop_gini + area +
                             (1 | state_ind) + (1 | year_ind),
                           center = F),
               data = groups_log, family = gaussian(), prior = mod_priors,
               stanvars = mod_stanvars,
               iter = 4000, chains = 4, save_dso = T, save_ranef = T,
               control = list(adapt_delta = .95), seed = 1234,
               future = T, file = here::here('Stanfits/pd_lm_gini_int_cy'))

## calculate WAIC
mod_int_waic <- waic(mod_int, cores = slurm_cores)

## calculate k fold crossvalidation information criterion
mod_int_kfold <- kfold(mod_int, K = 5, folds = 'stratified',
                       group = 'state_ind', chains = 4, iter = 2000,
                       seed = 1234, save_fits = T)

## calculate RMSE for each fold
mod_int_rmse <- kfold_rmse(mod_int_kfold)



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

## full controls model -- manually run in parallel to enable k-fold CV after
mod_int_controls_list <- foreach(i = 1:length(groups_list), .packages = 'brms') %dopar% {
  
  brm(brmsformula(nl ~ cap_dist + pop_gini + cap_dist:pop_gini + area +
                    excluded + family_downgraded_regaut5 + dom_overlap + gdp +
                    oil + v2x_polyarchy + (1 | state_ind) + (1 | year_ind),
                  center = F),
      data = groups_list[[i]], family = gaussian(), prior = mod_priors,
      stanvars = mod_stanvars,
      iter = 4000, chains = 2, save_dso = T, save_ranef = T, cores = 2,
      control = list(adapt_delta = .95), seed = 1234,
      file = here::here('Stanfits', paste0('pd_lm_gini_int_controls_cy_', i)))
  
}

## combine list for tables and figures
mod_int_controls <- combine_models(mlist = mod_int_controls_list, check_data = F)

## save combined brmsfit object
saveRDS(mod_int_controls, here::here('Stanfits/pd_lm_gini_int_controls_cy.rds'))

## save list of brmsfits for debugging
saveRDS(mod_int_controls_list, here::here('Stanfits/pd_lm_gini_int_controls_list_cy.rds'))

## calculate WAIC
mod_int_controls_waic <- waic(mod_int_controls, cores = slurm_cores)

## calculate RMSE for each fold for each imputed dataset
mod_int_controls_rmse <- foreach(i = mod_int_controls_list, .packages = 'brms') %dopar% {
  
  kf <- brms::kfold(i, K = 5, folds = 'stratified',
                    group = 'state_ind', chains = 4, iter = 2000,
                    seed = 1234, save_fits = T)
  kfold_rmse(kf)
  
}

## interactive population models table 
tabstr <- mcmcReg(list(mod_bivar_gini, mod_int, mod_int_controls),
                  custom.coef.map = tab_map,
                  custom.model.names = paste('Model', 6:8),
                  gof = list(c(mod_bivar_gini_waic$estimates['waic', 'Estimate'],
                               mean(mod_bivar_gini_rmse)),
                             c(mod_int_waic$estimates['waic', 'Estimate'],
                               mean(mod_int_rmse)),
                             c(mod_int_controls_waic$estimates['waic', 'Estimate'],
                               mean(unlist(mod_int_controls_rmse)))),
                  gofnames = list(c('WAIC', '5-fold RMSE'),
                                  c('WAIC', '5-fold RMSE'),
                                  c('WAIC', '5-fold RMSE')),
                  caption = 'Linear models explaining nightlights as a function of ethnic group population Gini and capital distance. The standard deviation of the country and year random intercepts are represented by $\\sigma_\\alpha$ and $\\sigma_\\gamma$, respectively. Continuous variables logged and standarized.',
                  label = 'tab:nl_pop_gini', float.pos = 'ht!')

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

fileConn <- file(here::here('Tables/pd_interactive_cy_gini.tex'))
writeLines(tabstr, fileConn)
close(fileConn)

## get marginal effects for interactive models
margs_interactive <- mcmcMargEff(mod_int, 'b_pop_gini', 'b_cap_dist:pop_gini',
                                 groups_log$cap_dist, plot = F)
margs_controls <- mcmcMargEff(mod_int_controls, 'b_pop_gini', 'b_cap_dist:pop_gini',
                              groups_log$cap_dist, plot = F)
margs_gg <- rbind(data.frame(margs_interactive, model = 1),
                  data.frame(margs_controls, model = 2))

## write marginal effects plot dataframe to disk
save(margs_gg, file = here::here('Figure Data/marg_eff_pop_gini_df_cy.RData'))



## print script to verify successful execution in log
print(paste('Nightlights Analysis Completed', Sys.time()))

## quit R
quit(save = 'no')

###################
## end of script ##
###################