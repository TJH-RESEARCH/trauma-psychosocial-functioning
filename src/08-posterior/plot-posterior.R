## PLOT POSTERIOR ----------------------------------------------------------
draws_hurdle_1 %>%
  select(.chain, .iteration, .draw, b_PCL_TOT) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name)) +
  tidybayes::stat_halfeye()

    
?stat_halfeye
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
