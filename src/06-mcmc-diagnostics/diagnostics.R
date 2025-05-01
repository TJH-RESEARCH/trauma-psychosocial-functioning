

# Caterpillar Plots
draws_hurdle_1 %>% 
  select(.chain, .iteration, .draw, contains('b_')) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>%
  ggplot(aes(.iteration, value)) + 
  geom_line(aes(color = factor(.chain), alpha = .5)) +
  ggsci::scale_color_aaas() +
  facet_grid(vars(name), scales = 'free_y')


# Trace Rank Plots
bayesplot::mcmc_rank_overlay(model_hurdle_1, 
                             pars = c(
                               'b_Intercept',
                               'b_PCL_TOT',
                               'b_deployed',
                               'b_gender_female',
                               'b_race_black'))

