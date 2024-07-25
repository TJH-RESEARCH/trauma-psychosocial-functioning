

library(tidyverse)
library(rstan)
library(tidybayes)
library(bayesplot)



# PREPARE DATA ------------------------------------------------------------
y_cens_temp <- data %>% filter(bipf_zero == 1) %>% select(bipf_score)
y_obs_temp <- data %>% filter(bipf_zero == 0) %>% select(bipf_score)

k_cens_temp <- data %>% filter(bipf_zero == 1) %>% select(mios_total)
k_obs_temp <- data %>% filter(bipf_zero == 0) %>% select(mios_total)

n_cens_temp <-  nrow(y_cens_temp)
n_obs_temp <-  nrow(y_obs_temp)

data_list <- 
  list(
    y_cens = y_cens_temp$bipf_score,
    k_cens = k_cens_temp$mios_total,
    n_cens = n_cens_temp,
    
    y_obs  =  y_obs_temp$bipf_score,
    k_obs  = k_obs_temp$mios_total,
    n_obs  = n_obs_temp, 
    
    L = 0
  )


# PLOT PRIORS
## Consider the scale of the data. 
## The outcome bipf_score can range from 0 to 7. 
## The predictor mios_total can range from 0 to 56. 

priors <-
  tibble(
    alpha_prior = rnorm(100000, mean = 0, sd = .05),
    beta_prior = rnorm(100000, mean = 0, sd = .25),
    sigma_prior = rexp(100000, rate = 1.5),
    id = c(1:100000)
  ) 


priors %>% 
  pivot_longer(-id) %>% 
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = .5) +
  facet_grid(~name,scales = 'free_x') +
  ggsci::scale_fill_bmj()





# MODEL -------------------------------------------------------
model_censored_bivariate <-
  '
 data {
  int<lower=0> n_obs;
  vector[n_obs] y_obs;
  vector[n_obs] k_obs;
  
  int<lower=0> n_cens;
  vector[n_cens] k_cens;
  
  real < upper = min(y_obs) > L;
  
 }

transformed data {
  real<lower=0> mean_y_obs = mean(to_vector(y_obs));
  real<lower=0> sd_y_obs = sd(to_vector(y_obs));
}
  
parameters {
  real alpha; // Intercept
  real beta; // Slope (regression coefficients)
  real<lower=0> sigma;
  
  array[n_cens] real < upper = L > y_cens;
}
model {
  
  // Priors
  alpha ~ normal(0, .05);
  beta ~ normal(0, .25);
  sigma ~ exponential(1.5);
  
  y_obs ~ normal(alpha + beta * k_obs, sigma);
  y_cens ~ normal(alpha + beta * k_cens, sigma);
  
}

generated quantities {
    real y_rep[n_obs] = normal_rng(alpha + k_obs * beta, sigma);
    real mean_y_rep = mean(to_vector(y_rep));
    real<lower=0> sd_y_rep = sd(to_vector(y_rep));
    int<lower=0, upper=1> mean_gte = (mean_y_rep >= mean_y_obs);
    int<lower=0, upper=1> sd_gte = (sd_y_rep >= sd_y_obs);
  }

'


# SAMPLE POSTERIOR ------------------------------------------------------------------

## Ensure computer has enough cores
cores <- ifelse(parallel::detectCores() < 4, 1, 4)

## Sample the posterior
fit_censored_bivariate <- 
  stan(model_code = model_censored_bivariate, 
       data = data_list, 
       chains = 4,
       warmup = 2000, 
       iter = 4000, 
       cores = cores,
       seed = 4020)

fit_censored_bivariate %>% tidybayes::summarise_draws() %>% print(n = 300)


# When mean_gte or sd_gte are close to 0 or 1, that means the simulated 
# data is greater than or less than the real data most of the time, indicative
# of bad fit. In other words, a good fitting model has values closer to .5 

# RECOVER DATA TYPES
fit_censored_bivariate <- fit_censored_bivariate %>% recover_types(data_list)

# EXTRACT DRAWS
draws_censored_bivariate <-
  fit_censored_bivariate %>%
  recover_types(data_list) %>%
  spread_draws(alpha, beta, sigma)

#tidybayes::add_predicted_draws(object = draws_censored_bivariate, newdata = 50)


# DIAGNOSTICS -------------------------------------------------------------

## Trace plots
draws_censored_bivariate %>% 
  ggplot(aes(.iteration, alpha)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Alpha')

draws_censored_bivariate %>% 
  ggplot(aes(.iteration, beta)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Beta')

draws_censored_bivariate %>% 
  ggplot(aes(.iteration, sigma)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Sigma')

draws_censored_bivariate %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')


# Rank Plots
bayesplot::mcmc_rank_hist(fit_censored_bivariate, 
                          pars = c('alpha', 'beta', 'sigma'))

bayesplot::mcmc_rank_ecdf(fit_censored_bivariate, 
                          pars = c('alpha', 'beta', 'sigma'))

bayesplot::mcmc_rank_overlay(fit_censored_bivariate, 
                             pars = c('alpha', 'beta', 'sigma'))


# Visualize the posterior distribution ------------------------------------
draws_censored_bivariate %>%
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name)) +
  stat_halfeye()

draws_censored_bivariate %>% 
  ggplot(aes(alpha)) +
  geom_density(alpha = .2, fill = 'green')

draws_censored_bivariate %>% 
  ggplot(aes(beta)) +
  geom_density(alpha = .2, fill = 'blue')

draws_censored_bivariate %>% 
  ggplot(aes(sigma)) +
  geom_density(alpha = .2, fill = 'red')

draws_censored_bivariate %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .4) +
  ggsci::scale_fill_aaas()




# Summarize the posterior -------------------------------------------------

draws_censored_bivariate %>% tidybayes::summarise_draws()

draws_censored_bivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('alpha'),
    prob = .9,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_censored_bivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('beta'),
    prob = .9,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_censored_bivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('sigma'),
    prob = .9,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_censored_bivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('alpha', 'beta', 'sigma'),
    prob = .9,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_censored_bivariate %>% 
  tidybayes::mean_hdci(beta, .width = .90)





# POSTERIOR PREDICTIVE MODEL ----------------------------------------------

predict_bipf_bivariate <- 
  draws_censored_bivariate %>% 
  transmute(
    predictedMean_0 = alpha + beta * 0,
    predictedDist_0 = 
      rnorm(nrow(draws_censored_bivariate), 
            mean = predictedMean_0, 
            sd = sigma),
    predictedMean_25 = alpha + beta * 25,
    predictedDist_25 = 
      rnorm(nrow(draws_censored_bivariate), 
            mean = predictedMean_25, 
            sd = sigma),
    predictedMean_50 = alpha + beta * 50,
    predictedDist_50 = 
      rnorm(nrow(draws_censored_bivariate), 
            mean = predictedMean_50, 
            sd = sigma),
    diffDist50to0 = abs(predictedDist_50 - predictedDist_0),
    id = c(1:nrow(draws_censored_bivariate)),
  )

predict_bipf_bivariate %>% 
  pivot_longer(-id) %>%
  mutate(
    param = str_split_fixed(name, n = 2, pattern = '_')[,1],
    x = str_split_fixed(name, n = 2, pattern = '_')[,2]
  ) %>% 
  ggplot(aes(value, fill = x)) +
  geom_density(alpha = .3) +
  facet_grid(vars(param), scales = 'free_y') +
  ggsci::scale_fill_aaas()

## Here, mu is the predicted average bifp_mean given the
## value of mios_total. 
## y on the other hand is the predicted distribution of 
## bifp_mean given the value of mios_total. 


# POSTERIOR PREDICTIVE CHECK ----------------------------------------------
## Are the predicted values similar to the real values?

## Extract the predicted values of y
y_rep <- extract(fit_censored_bivariate)[["y_rep"]]


bayesplot::ppc_dens_overlay(y = data$bipf_score[data$bipf_gt_zero == 1], yrep = y_rep[1:50, ])
bayesplot::ppc_hist(y = data$bipf_score[data$bipf_gt_zero == 1], yrep = y_rep[1:50, ])





## 
y_cens <- extract(fit_censored_bivariate)[["y_cens"]]


bayesplot::ppc_dens_overlay(y = data$bipf_score[data$bipf_gt_zero == 0], yrep = y_cens[1:50, ])
bayesplot::ppc_hist(y = data$bipf_score[data$bipf_gt_zero == 0], yrep = y_cens[1:50, ])


