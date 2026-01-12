

# Caterpillar Plots

## Lots of variables, so batch these
draws_hurdle_3 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,4:10)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')

draws_hurdle_3 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,11:17)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')

draws_hurdle_3 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,18:25)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')


draws_hurdle_3 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,26:31)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')


draws_hurdle_3 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,32:38)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')


draws_hurdle_3 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,39:45)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')


draws_hurdle_3 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,46:52)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')

draws_hurdle_3 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  select(c(1,2,3,53:59)) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')




## save the names to aid in batching
b_names <- draws_hurdle_3 %>% names()


# Trace Rank Plots
bayesplot::mcmc_rank_overlay(model_3_hurdle, 
                             pars = c(b_names[5:13]))
bayesplot::mcmc_rank_overlay(model_3_hurdle, 
                             pars = c(b_names[14:22]))
bayesplot::mcmc_rank_overlay(model_3_hurdle, 
                             pars = c(b_names[23:31]))
bayesplot::mcmc_rank_overlay(model_3_hurdle, 
                             pars = c(b_names[32:40]))
bayesplot::mcmc_rank_overlay(model_3_hurdle, 
                             pars = c(b_names[41:50]))
bayesplot::mcmc_rank_overlay(model_3_hurdle, 
                             pars = c(b_names[51:56]))


