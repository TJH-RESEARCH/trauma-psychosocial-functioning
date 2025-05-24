# PLOT POSTERIOR
library(metbrewer)

# Non-Zero Outcomes -------------------------------------------------------
draws_hurdle_1 %>%
  select(.chain, .iteration, .draw, b_pcl_total, b_Intercept) %>% 
  rename(Intercept = b_Intercept, PCL = b_pcl_total) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name, fill = name)) +
  tidybayes::stat_halfeye() +
  #scale_y_discrete(breaks = NULL) +
  labs(y = NULL, 
       x = 'Coefficient', 
       title = 'Non-Zero Model') + 
  theme_custom +
  theme(legend.position = 'none') +
  MetBrewer::scale_fill_met_d('VanGogh2')



# Hurdle: Log Odds --------------------------------------------------------
draws_hurdle_1 %>%
  select(.chain, .iteration, .draw, Intercept_hu, b_hu_pcl_total) %>% 
  # Rename Variables 
  rename(Intercept = Intercept_hu, PCL = b_hu_pcl_total) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name, fill = name)) +
  tidybayes::stat_halfeye() +
  #scale_y_discrete(breaks = NULL) +
  labs(y = NULL, 
       x = 'Coefficient (Log Odds)', 
       title = 'Hurdle Model',
       subtitle = 'Logs Odds of scoring zero on bIPF') + 
  theme_custom +
  theme(legend.position = 'none')


# Hurdle: Probability -------------------------------------------------------------
draws_hurdle_1 %>%
  select(.chain, .iteration, .draw, Intercept_hu, b_hu_pcl_total) %>% 
  # Rename Variables 
  rename(Intercept = Intercept_hu, PCL = b_hu_pcl_total) %>% 
  mutate(
    Intercept = exp(Intercept)/(1+exp(Intercept)),
    PCL = exp(PCL)/(1+exp(PCL)),
    ) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name, fill = name)) +
  tidybayes::stat_halfeye() +
  #scale_y_discrete(breaks = NULL) +
  labs(y = NULL, 
       x = 'Coefficient (Probability)', 
       title = 'Hurdle Model',
       subtitle = 'Probability of scoring zero on bIPF') + 
  theme_custom +
  theme(legend.position = 'none')


# Hurdle: Risk Ratio -------------------------------------------------------------
draws_hurdle_1 %>%
  select(.chain, .iteration, .draw, Intercept_hu, b_hu_pcl_total) %>% 
  # Rename Variables 
  rename(Intercept = Intercept_hu, PCL = b_hu_pcl_total) %>% 
  
  mutate(
    Intercept = effectsize::logoddsratio_to_riskratio(Intercept),
    PCL = effectsize::logoddsratio_to_riskratio(PCL),
  ) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name, fill = name)) +
  tidybayes::stat_halfeye() +
  #scale_y_discrete(breaks = NULL) +
  labs(y = NULL, 
       x = 'Risk Ratio', 
       title = 'Hurdle Model',
       subtitle = 'Risk Ration of scoring zero on bIPF') + 
  theme_custom +
  theme(legend.position = 'none')








draws_hurdle_1 %>%
  bayesplot::mcmc_areas(
    pars = c('b_pcl_total'),
    prob = .95,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area') +
  labs(x = 'PCL-5 Coefficient',
       title = 'Posterior Distribution')
?mcmc_areas

draws_hurdle_1 %>%
  ggplot(aes(b_pcl_total)) + geom_density()


    

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
