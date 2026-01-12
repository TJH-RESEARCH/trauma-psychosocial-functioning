

# Caterpillar Plots

## 35 variables, so batch these
draws_hurdle_1 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,4:10)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')

draws_hurdle_1 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,11:17)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')

draws_hurdle_1 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,18:23)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')




## save the names to aid in batching
b_names <- draws_hurdle_1 %>% names()


# Trace Rank Plots
bayesplot::mcmc_rank_overlay(model_1_hurdle, 
                             pars = c(b_names[5:13]))
bayesplot::mcmc_rank_overlay(model_1_hurdle, 
                             pars = c(b_names[14:23]))



# Any issues with Sampling?
draws_hurdle_1 %>% count(divergent__)
draws_hurdle_1 %>% count(n_leapfrog__)
draws_hurdle_1 %>% count(stepsize__)


# 

broom.mixed::tidy(model_1_hurdle) %>% select(term) %>% c()



# Check Rhat and Effective Sample Size -----------------------------------------


draws_hurdle_1 %>% ggplot(aes(accept_stat__)) + geom_density()
bayestestR::effective_sample(model_1_hurdle)
bayestestR::diagnostic_draws(model_1_hurdle)
