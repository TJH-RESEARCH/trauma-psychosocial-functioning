library(tidyverse)
library(rstan)
library(tidybayes)
library(bayesplot)
library(brms)

# DRAW DAGs ---------------------------------------------------------------
source(here::here('src/dags.R'))


# IMPORT DATA -------------------------------------------------------------
data <- read_csv(here::here('data/data_clean.csv'))


# PREPARE OUTCOME DATA ------------------------------------------------------------
source(here::here('src/score-bipf.R'))


# SCALE PREDICTOR DATA ----------------------------------------------------------
source(here::here('src/scale-predictors.R'))


# PREPARE DATA ------------------------------------------------------------
source(here::here('src/prepare-data.R'))


# PLOT PRIORS -------------------------------------------------------------priors <-
tibble(
    beta_prior = rcauchy(n = 1e5, location = 0, scale = 2.5),
    alpha_prior = rt(n = 1e5, df = 3, ncp = 2.5),
    id = rep(1, 1e5)
  )  %>% 
  pivot_longer(-id) %>% 
  ggplot(aes(value)) +
  geom_density(aes(fill = name), alpha = .25) +
  lims(x = c(-15, 15))
    


# PRIOR PREDICTIVE --------------------------------------------------------


# FIT & SAMPLE POSTERIOR --------------------------------------------------

cores <- ifelse(parallel::detectCores() < 4, parallel::detectCores(), 4)## Ensure computer has enough cores

fit_ord_log_multivariate <- 
  stan(file = here::here('src/bipf-ordered-logit-multivariate.stan'),
       data = data_list, 
       chains = 4,
       warmup = 2000, 
       iter = 4000, 
       cores = cores,
       seed = 4020)


# EXTRACT DRAWS -----------------------------------------------------------
draws_ord_log_multivariate <-
  fit_ord_log_multivariate %>%
  tidybayes::recover_types(data_list) %>%
  tidybayes::spread_draws(beta[1:7], cutpoints[1:5]) %>% 
  pivot_wider(names_from = `1:5`, values_from = cutpoints, names_prefix = 'alpha_cut_') %>% 
  pivot_wider(names_from = `1:7`, values_from = beta, names_prefix = 'beta_')



# DIAGNOSE SAMPLING -------------------------------------------------------
fit_ord_log_multivariate

draws_ord_log_multivariate %>% 
  select(!contains('alpha')) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')

draws_ord_log_multivariate %>% 
  select(!contains('beta')) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')

bayesplot::mcmc_rank_overlay(fit_ord_log_multivariate, 
                             pars = c('beta[1]', 
                                      'beta[2]',
                                      'beta[3]',
                                      'beta[4]',
                                      'beta[5]',
                                      'beta[6]',
                                      'beta[7]',
                                      'cutpoints[1]', 
                                      'cutpoints[2]',
                                      'cutpoints[3]',
                                      'cutpoints[4]'))




# POSTERIOR PREDICTIVE CHECK ----------------------------------------------
## Extract the predicted values of y
y_predictions <- extract(fit_ord_log_multivariate)[["y_pred"]]

## Plot the predictions vs the actual data
bayesplot::ppc_bars(y = as.numeric(data$bipf_category), yrep = y_predictions[1:50, ])
bayesplot::ppc_hist(y = as.numeric(data$bipf_category), yrep = y_predictions[51:100, ])
bayesplot::ppc_dens_overlay(y = as.numeric(data$bipf_category), yrep = y_predictions[151:200, ])

## Histogram of errors 
bayesplot::ppc_error_scatter_avg(y = as.numeric(data$bipf_category), yrep = y_predictions[101:150, ])


  


# EXAMINE POSTERIOR DISTRIBUTIONS ----------------------------------------------------
draws_ord_log_multivariate %>%
  select(!contains('alpha')) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name)) +
  tidybayes::stat_halfeye()

draws_ord_log_multivariate %>%
  select(!contains('beta')) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .2)

draws_ord_log_multivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('beta_1'),
    prob = .95,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area') +
  labs(x = 'Log Odds')

draws_ord_log_multivariate %>% 
  mutate(beta_1_p = plogis(beta_1)) %>% 
  bayesplot::mcmc_areas(
    pars = c('beta_1_p'),
    prob = .95,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area') +
  labs(x = 'Probability',
       title = 'Posterior Distribution',
       subtitle = 'Coefficient on Moral Injury Symptoms')

# SUMMARIZE POSTERIOR -----------------------------------------------------
draws_ord_log_multivariate %>% tidybayes::summarise_draws()
draws_ord_log_multivariate %>% tidybayes::mode_hdi(beta_1, .width = .95) %>% 
  mutate(across(where(is.numeric), ~ plogis(.x), .names = "p_{.col}")) %>% 
  select(-p_.width)




# PREDICTIONS -------------------------------------------------------------
draws_ord_log_multivariate %>% 
  transmute(
    y_5 = beta_1 * 5,
    y_40 = beta_1 * 40,
    id = c(1:nrow(.))
  )

