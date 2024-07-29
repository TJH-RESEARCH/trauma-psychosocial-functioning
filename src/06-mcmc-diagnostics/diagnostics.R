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

