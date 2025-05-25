

# Study 1 -----------------------------------------------------------------
data_1 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram(bins = 20) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15)) +
  theme_custom +
  labs(title = 'bIPF Distribution',
       subtitle = 'Study 1',
       x = 'Difficulty',
       y = 'Count')




# Study 2 -----------------------------------------------------------------
data_2 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram(bins = 20) + 
  theme_custom +
  labs(title = 'bIPF Distribution',
       subtitle = 'Study 2',
       x = 'Difficulty',
       y = 'Count')


# Study 3 -----------------------------------------------------------------
data_3 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram(bins = 20) + 
  theme_custom +
  labs(title = 'bIPF Distribution',
       subtitle = 'Study 3',
       x = 'Difficulty',
       y = 'Count')

