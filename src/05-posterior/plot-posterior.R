# PLOT POSTERIOR
library(MetBrewer)

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


# Non-Zero Outcomes -------------------------------------------------------
draws_hurdle_1 %>%
  select(.chain, .iteration, .draw, b_pcl_total) %>% 
  rename(PCL = b_pcl_total) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name, 
             fill = after_stat(x > .2 & x < .4))) + #experiement with fill after_stat
  tidybayes::stat_halfeye() +
  #scale_y_discrete(breaks = NULL) +
  labs(y = NULL, 
       x = 'Coefficient', 
       title = 'Non-Zero Process') + 
  theme_custom +
  theme(legend.position = 'none') +
  MetBrewer::scale_fill_met_d('Austria')




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
       title = 'Zero Process',
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
       title = 'Zero Process',
       subtitle = 'Probability of scoring zero on bIPF') + 
  theme_custom +
  theme(legend.position = 'none')
