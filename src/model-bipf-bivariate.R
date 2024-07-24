

library(tidyverse)
library(rstan)
library(tidybayes)
library(bayesplot)



# PREPARE DATA ------------------------------------------------------------

data_list <- 
  data %>% 
  #filter(bipf_gt_zero == 1) %>% 
  select(mios_total, 
         bipf_mean) %>% 
  tidybayes::compose_data()


# PLOT PRIORS
## Consider the scale of the data. 
## The outcome bipf_mean can range from 0 to 7. 
## The predictor mios_total can range from 0 to 56. 

priors <-
  tibble(
    alpha_prior = rnorm(100000, mean = 0, sd = .25),
    beta_prior = rnorm(100000, mean = 0, sd = .125),
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
model_bipf_bivariate <-
  '
  data {
    int < lower = 1 > n; // Sample size
    vector[n] mios_total; // Predictor
    vector[n] bipf_mean; // Outcome
  }
  
  transformed data {
  real<lower=0> mean_bipf_mean = mean(to_vector(bipf_mean));
  real<lower=0> sd_bipf_mean = sd(to_vector(bipf_mean));
  }

  parameters {
    real alpha; // Intercept
    real beta; // Slope (regression coefficients)
    real < lower = 0 > sigma; // Error SD
  }
  
  model {
  
    // Priors
    alpha ~ normal(0, 5);
    beta ~ normal(0, 5);
    sigma ~ exponential(1);
    
    // Linear Model
    bipf_mean ~ normal(alpha + mios_total * beta, sigma);
  }
  
  generated quantities {
    real y_rep[n] = normal_rng(alpha + mios_total * beta, sigma);
    real mean_y_rep = mean(to_vector(y_rep));
    real<lower=0> sd_y_rep = sd(to_vector(y_rep));
    int<lower=0, upper=1> mean_gte = (mean_y_rep >= mean_bipf_mean);
    int<lower=0, upper=1> sd_gte = (sd_y_rep >= sd_bipf_mean);
  }

'



# SAMPLE POSTERIOR ------------------------------------------------------------------

## Ensure computer has enough cores
cores <- ifelse(parallel::detectCores() < 4, 1, 4)

## Sample the posterior
sample_bipf_bivariate <- 
  stan(model_code = model_bipf_bivariate, 
       data = data_list, 
       chains = 4,
       warmup = 2000, 
       iter = 4000, 
       cores = cores,
       seed = 4020)

sample_bipf_bivariate %>% tidybayes::summarise_draws() %>% print(n = 300)


# When mean_gte or sd_gte are close to 0 or 1, that means the simulated 
# data is greater than or less than the real data most of the time, indicative
# of bad fit. In other words, a good fitting model has values closer to .5 

# RECOVER DATA TYPES
sample_bipf_bivariate <- sample_bipf_bivariate %>% recover_types(data_list)

# EXTRACT DRAWS
draws_bipf_bivariate <-
  sample_bipf_bivariate %>%
  recover_types(data_list) %>%
  spread_draws(alpha, beta, sigma)

#tidybayes::add_predicted_draws(object = draws_bipf_bivariate, newdata = 50)


# DIAGNOSTICS -------------------------------------------------------------

## Trace plots
draws_bipf_bivariate %>% 
  ggplot(aes(.iteration, alpha)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Alpha')

draws_bipf_bivariate %>% 
  ggplot(aes(.iteration, beta)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Beta')

draws_bipf_bivariate %>% 
  ggplot(aes(.iteration, sigma)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Sigma')

draws_bipf_bivariate %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')


# Rank Plots
bayesplot::mcmc_rank_hist(sample_bipf_bivariate, 
                          pars = c('alpha', 'beta', 'sigma'))

bayesplot::mcmc_rank_ecdf(sample_bipf_bivariate, 
                          pars = c('alpha', 'beta', 'sigma'))

bayesplot::mcmc_rank_overlay(sample_bipf_bivariate, 
                             pars = c('alpha', 'beta', 'sigma'))


# Visualize the posterior distribution ------------------------------------
draws_bipf_bivariate %>%
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name)) +
  stat_halfeye()

draws_bipf_bivariate %>% 
  ggplot(aes(alpha)) +
  geom_density(alpha = .2, fill = 'green')

draws_bipf_bivariate %>% 
  ggplot(aes(beta)) +
  geom_density(alpha = .2, fill = 'blue')

draws_bipf_bivariate %>% 
  ggplot(aes(sigma)) +
  geom_density(alpha = .2, fill = 'red')

draws_bipf_bivariate %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .4) +
  ggsci::scale_fill_aaas()




# Summarize the posterior -------------------------------------------------

draws_bipf_bivariate %>% tidybayes::summarise_draws()

draws_bipf_bivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('alpha'),
    prob = .9,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_bipf_bivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('beta'),
    prob = .9,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_bipf_bivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('sigma'),
    prob = .9,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_bipf_bivariate %>% 
  tidybayes::mean_hdci(beta, .width = .90)





# POSTERIOR PREDICTIVE MODEL ----------------------------------------------

predict_bipf_bivariate <- 
  draws_bipf_bivariate %>% 
    transmute(
      predictedMean_0 = alpha + beta * 0,
      predictedDist_0 = 
        rnorm(nrow(draws_bipf_bivariate), 
              mean = predictedMean_0, 
              sd = sigma),
      predictedMean_25 = alpha + beta * 25,
      predictedDist_25 = 
        rnorm(nrow(draws_bipf_bivariate), 
              mean = predictedMean_25, 
              sd = sigma),
      predictedMean_50 = alpha + beta * 50,
      predictedDist_50 = 
        rnorm(nrow(draws_bipf_bivariate), 
        mean = predictedMean_50, 
        sd = sigma),
      id = c(1:nrow(draws_bipf_bivariate))
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
y_rep <- extract(sample_bipf_bivariate)[["y_rep"]]


bayesplot::ppc_dens_overlay(y = data$bipf_mean, yrep = y_rep[1:50, ])
bayesplot::ppc_hist(y = data$bipf_mean, yrep = y_rep[1:50, ])

bayesplot::ppd_hist(y_rep[1:50, ])
y_rep %>% broom::tidy()

