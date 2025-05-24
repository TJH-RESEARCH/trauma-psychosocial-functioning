

# Study 1 -----------------------------------------------------------------
data_1 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram() + 
  scale_y_continuous(breaks = c(0, 5, 10, 15)) +
  theme_custom +
  labs(title = 'bIPF Distribution',
       subtitle = 'Study 1',
       x = 'bIPF (Difficulty Functioning)',
       y = 'Count')

data_1 %>% 
  mutate(bipf_zero = ifelse(bipf_total == 0, 1, 0)) %>% 
  count(bipf_zero) %>% 
  mutate(perc = n / sum(n))


# Study 2 -----------------------------------------------------------------
data_2 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram() + 
  theme_custom +
  labs(title = 'bIPF Distribution',
       subtitle = 'Study 2',
       x = 'bIPF (Difficulty Functioning)',
       y = 'Count')

data_2 %>% 
  mutate(bipf_zero = ifelse(bipf_total == 0, 1, 0)) %>% 
  count(bipf_zero) %>% 
  mutate(perc = n / sum(n))

# Study 3 -----------------------------------------------------------------
data_3 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram() + 
  theme_custom +
  labs(title = 'bIPF Distribution',
       subtitle = 'Study 3',
       x = 'bIPF (Difficulty Functioning)',
       y = 'Count')

data_3 %>% 
  mutate(bipf_zero = ifelse(bipf_total == 0, 1, 0)) %>% 
  count(bipf_zero) %>% 
  mutate(perc = n / sum(n))

