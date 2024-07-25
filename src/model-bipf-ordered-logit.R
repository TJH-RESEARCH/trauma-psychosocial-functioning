

library(tidyverse)
library(rstan)
library(tidybayes)
library(bayesplot)



# PREPARE DATA ------------------------------------------------------------

data_list <- 
  data %>% 
  select(mios_total, 
         bipf_category) %>% 
  tidybayes::compose_data()


# PLOT PRIORS ------------------------------------------------------------
priors <-
  tibble(
    alpha_prior = rnorm(100000, mean = 0, sd = .25),
    beta_prior = rnorm(100000, mean = 0, sd = .125),
    sigma_prior = rexp(100000, rate = 1),
    id = c(1:100000)
) 


priors %>% 
  pivot_longer(-id) %>% 
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = .5) +
  facet_grid(~name,scales = 'free_x') +
  ggsci::scale_fill_bmj()




# MODEL -------------------------------------------------------
model_ord_log_bivariate <-
  '
  data {
    int < lower = 2 > n_bipf_category;                                  // Factor levels in outcome variable 
    int < lower = 1 > n;                                                // Sample size
    array[n] int < lower = 1, upper = n_bipf_category > bipf_category;  // Outcome
    vector[n] mios_total;                                               // Predictor
  }

  parameters {
    real beta;   // Regression coefficient
    ordered[n_bipf_category - 1] cutpoints; 
  }
  
  transformed parameters{
  vector[n] log_prob = mios_total * beta;
}

  model {
  
    // Priors
    beta ~ normal(0, 5);
    
    // Logit Model
      bipf_category ~ ordered_logistic(log_prob, cutpoints);
  
  }
 
 
  generated quantities{
    vector[n] log_lik;        
    real y_pred[n];
    
    for (i in 1:n){
  
      //log-likelihood
      log_lik[i] = ordered_logistic_lpmf(bipf_category[i] | log_prob[i], cutpoints);
    
       //predictions
       y_pred[i] = ordered_logistic_rng(log_prob[i], cutpoints);
    }
}


'


# SAMPLE POSTERIOR ------------------------------------------------------------------

## Ensure computer has enough cores
cores <- ifelse(parallel::detectCores() < 4, 1, 4)

## Sample the posterior
fit_ord_log_bivariate <- 
  stan(model_code = model_ord_log_bivariate, 
       data = data_list, 
       chains = 4,
       warmup = 2000, 
       iter = 4000, 
       cores = cores,
       seed = 4020)

fit_ord_log_bivariate %>% tidybayes::summarise_draws() %>% print(n = 700)


# When mean_gte or sd_gte are close to 0 or 1, that means the simulated 
# data is greater than or less than the real data most of the time, indicative
# of bad fit. In other words, a good fitting model has values closer to .5 

# RECOVER DATA TYPES
fit_ord_log_bivariate <- fit_ord_log_bivariate %>% recover_types(data_list)

# EXTRACT DRAWS
draws_ord_log_bivariate <-
  fit_ord_log_bivariate %>%
  recover_types(data_list) %>%
  spread_draws(beta, cutpoints[1:5]) %>% 
  pivot_wider(names_from = `1:5`, values_from = cutpoints, names_prefix = 'alpha_cut_')

#tidybayes::add_predicted_draws(object = draws_ord_log_bivariate, newdata = 50)


# DIAGNOSTICS -------------------------------------------------------------

## Trace plots
draws_ord_log_bivariate %>% 
  ggplot(aes(.iteration, alpha_cut_1)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Alpha Cutpoint 1')

draws_ord_log_bivariate %>% 
  ggplot(aes(.iteration, alpha_cut_2)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Alpha Cutpoint 2')

draws_ord_log_bivariate %>% 
  ggplot(aes(.iteration, alpha_cut_3)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Alpha Cutpoint 3')

draws_ord_log_bivariate %>% 
  ggplot(aes(.iteration, alpha_cut_4)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Alpha Cutpoint 4')

draws_ord_log_bivariate %>% 
  ggplot(aes(.iteration, beta)) + 
  geom_line(aes(color = factor(.chain)), alpha = .85) +
  facet_wrap(vars(.chain)) +
  ggsci::scale_color_aaas() + 
  labs(title = 'Beta')

draws_ord_log_bivariate %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')


# Rank Plots
bayesplot::mcmc_rank_hist(fit_ord_log_bivariate, 
                          pars = c('beta', 
                                   'cutpoints[1]', 
                                   'cutpoints[2]',
                                   'cutpoints[3]',
                                   'cutpoints[4]'))

bayesplot::mcmc_rank_overlay(fit_ord_log_bivariate, 
                             pars = c('beta', 
                                      'cutpoints[1]', 
                                      'cutpoints[2]',
                                      'cutpoints[3]',
                                      'cutpoints[4]'))


# Visualize the posterior distribution ------------------------------------
draws_ord_log_bivariate %>%
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name)) +
  stat_halfeye()

draws_ord_log_bivariate %>%
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  filter(name != 'beta') %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .2) +
  facet_wrap(~name)

draws_ord_log_bivariate %>% 
  ggplot(aes(alpha_cut_1)) +
  geom_density(alpha = .2, fill = 'green')

draws_ord_log_bivariate %>% 
  ggplot(aes(alpha_cut_2)) +
  geom_density(alpha = .2, fill = 'green')

draws_ord_log_bivariate %>% 
  ggplot(aes(alpha_cut_3)) +
  geom_density(alpha = .2, fill = 'green')

draws_ord_log_bivariate %>% 
  ggplot(aes(alpha_cut_4)) +
  geom_density(alpha = .2, fill = 'green')

draws_ord_log_bivariate %>% 
  ggplot(aes(beta)) +
  geom_density(alpha = .2, fill = 'blue')


draws_ord_log_bivariate %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .4) +
  ggsci::scale_fill_aaas()




# Summarize the posterior -------------------------------------------------
draws_ord_log_bivariate %>% tidybayes::summarise_draws()

draws_ord_log_bivariate %>% 
  bayesplot::mcmc_areas(
    pars = c('beta'),
    prob = .95,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area')

draws_ord_log_bivariate %>% 
  tidybayes::mean_hdci(beta, .width = .95)






# POSTERIOR PREDICTIVE CHECK ----------------------------------------------
## Are the predicted values similar to the real values?

## Extract the predicted values of y
y_predictions <- extract(fit_ord_log_bivariate)[["y_pred"]]

## Plot the predictions vs the actual data
bayesplot::ppc_bars(y = as.numeric(data$bipf_category), yrep = y_predictions[1:50, ])
bayesplot::ppc_hist(y = as.numeric(data$bipf_category), yrep = y_predictions[51:100, ])
bayesplot::ppc_dens_overlay(y = as.numeric(data$bipf_category), yrep = y_predictions[151:200, ])

## Histogram of errors 
bayesplot::ppc_error_hist(y = as.numeric(data$bipf_category), yrep = y_predictions[101:150, ])




# POSTERIOR PREDICTIVE MODEL ----------------------------------------------
