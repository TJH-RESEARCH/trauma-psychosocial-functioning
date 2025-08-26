

# Study 1 -----------------------------------------------------------------
plot_bipf_1 <-
  data_1 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram(bins = 20) + 
  scale_y_continuous(breaks = seq(0,40,5)) +
  labs(title = 'bIPF Distribution',
       subtitle = 'Study 1',
       x = 'Difficulty',
       y = 'Count') +
  theme()




# Study 2 -----------------------------------------------------------------
plot_bipf_2 <-
  data_2 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram(bins = 20) + 
  scale_y_continuous(breaks = seq(0,80,20)) +
  labs(title = 'bIPF Distribution',
       subtitle = 'Study 2',
       x = 'Difficulty',
       y = 'Count') +
  theme()


# Study 3 -----------------------------------------------------------------
plot_bipf_3 <-
  data_3 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram(bins = 20) + 
  scale_y_continuous(breaks = seq(0,40,5)) +
  labs(title = 'bIPF Distribution',
       subtitle = 'Study 3',
       x = 'Difficulty',
       y = 'Count') +
  theme()

library(patchwork)
plot_bipf_1 / plot_bipf_2 / plot_bipf_3 +
  plot_layout(axis_titles = "collect")
