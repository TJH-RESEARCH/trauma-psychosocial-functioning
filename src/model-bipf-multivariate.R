

library(tidyverse)
library(rstan)
library(tidybayes)
library(bayesplot)


# PREPARE THE DATA --------------------------------------------------------
data_prepared <-
  list(
    y = data$bipf_mean,
    x = 
      with(data,
           model.matrix(
             bipf_mean ~ 
               mios_total + 
               military_exp_combat +
               service_era_post_911 + 
               service_era_persian_gulf +
               sex_female +
               race_black +
               race_white)[,2:8]
      ),
    k = 7,
    n = nrow(data)
  )



# PLOT PRIORS

priors <-
  tibble(
    alpha_prior = rnorm(100000, mean = 0, sd = 5),
    beta_mios_prior = rnorm(100000, mean = 0, sd = 5),
    sigma_prior = rexp(100000, rate = .5),
    id = c(1:100000)
  ) 


priors %>% 
  pivot_longer(-id) %>% 
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = .5) +
  facet_grid(~name,scales = 'free_x') +
  ggsci::scale_fill_bmj()


# MODEL -------------------------------------------------------
model_bipf_multi <-
  '
  data {
    int < lower = 1 > n;         // Sample size
    int < lower = 0 > k;         // Number of predictors
    matrix[n, k] x;              // Predictor matrix
    vector[n] y;                 // Outcome
  }
  
  transformed data {
    real<lower=0> mean_y = mean(to_vector(y));
    real<lower=0> sd_y = sd(to_vector(y));
  }

  parameters {
    real alpha;                 // Alpha
    vector[k] beta;             // Coefficients
    real < lower = 0 > sigma;   // Error SD
  }
  
  model {
  
    // Priors
    alpha ~ normal(0, 5);
    beta ~ normal(0, 5);
    sigma ~ exponential(.5);
    
    // Linear Model
    y ~ normal(alpha + x * beta, sigma);
  }
  
  generated quantities {
    real y_rep[n] = normal_rng(alpha + x * beta, sigma);
    real mean_y_rep = mean(to_vector(y_rep));
    real<lower=0> sd_y_rep = sd(to_vector(y_rep));
    int<lower=0, upper=1> mean_gte = (mean_y_rep >= mean_y);
    int<lower=0, upper=1> sd_gte = (sd_y_rep >= sd_y);
  }

'




# FIT ---------------------------------------------------------------------
## Ensure computer has enough cores
cores <- ifelse(parallel::detectCores() < 4, 1, 4)

## Sample the posterior
fit_multivariate <- 
  stan(model_code = model_bipf_multi, 
       data = data_prepared, 
       cores = cores,
       seed = 4020)



# PROCESS MCMC DRAWS -----------------------------------------------------------
fit_multivariate %>% tidybayes::summarise_draws() %>% print(n = 300)

draws_multivariate <-
  fit_multivariate %>%
  recover_types(data_prepared) %>%
  spread_draws(alpha, beta[1:8], sigma) %>% 
  pivot_wider(names_from = `1:8`, values_from = beta, names_prefix = 'beta') %>% 
  rename(
    beta_mios_total = beta1,
    beta_military_exp_combat = beta2,
    beta_service_era_post_911 = beta3,
    beta_service_era_persian_gulf = beta4,
    beta_sex_female = beta5,
    beta_race_black = beta6,
    beta_race_white = beta7
  )

draws_multivariate




# CHECK DIAGNOSTICS ON THE MCMC SAMPLING -------------------------------------------------------------

## Trace plots
draws_multivariate %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')


# Rank Plots
bayesplot::mcmc_rank_hist(fit_multivariate, 
                          pars = c('alpha', 'beta[1]', 'beta[2]', 
                                   'beta[3]', 'beta[4]', 'beta[5]', 
                                   'beta[6]', 'beta[7]', 'sigma'))

bayesplot::mcmc_rank_overlay(fit_multivariate, 
                             pars = c('alpha', 'beta[1]', 'beta[2]', 
                                      'beta[3]', 'beta[4]', 'beta[5]', 
                                      'beta[6]', 'beta[7]', 'sigma'))









# Visualize the posterior distribution ------------------------------------
draws_multivariate %>%
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name)) +
  stat_halfeye()

draws_multivariate %>% 
  ggplot(aes(alpha)) +
  geom_density(alpha = .2, fill = 'green')

draws_multivariate %>% 
  ggplot(aes(beta_mios_total)) +
  geom_density(alpha = .2, fill = 'blue')

draws_multivariate %>% 
  ggplot(aes(sigma)) +
  geom_density(alpha = .2, fill = 'red')

draws_multivariate %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .4) +
  ggsci::scale_fill_aaas()




# Summarize the posterior -------------------------------------------------

draws_multivariate %>% tidybayes::summarise_draws()

draws_multivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('intercept'),
    prob = .9,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_multivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('beta_mios_total'),
    prob = .9,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_multivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('sigma'),
    prob = .9,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_multivariate %>% 
  tidybayes::mean_hdci(beta, .width = .90)





# POSTERIOR PREDICTIVE MODEL ----------------------------------------------


# POSTERIOR PREDICTIVE CHECK ----------------------------------------------
## Are the predicted values similar to the real values?

y_rep <- extract(fit_multivariate)[["y_rep"]]
bayesplot::ppc_dens_overlay(y = data$bipf_mean, yrep = y_rep[1:50, ])
bayesplot::ppc_hist(y = data$bipf_mean, yrep = y_rep[1:50, ])
