data_1 %>% 
  mutate(bipf_zero = ifelse(bipf_total == 0, 1, 0)) %>% 
  count(bipf_zero) %>% 
  mutate(perc = n / sum(n))

data_2 %>% 
  mutate(bipf_zero = ifelse(bipf_total == 0, 1, 0)) %>% 
  count(bipf_zero) %>% 
  mutate(perc = n / sum(n))

data_3 %>% 
  mutate(bipf_zero = ifelse(bipf_total == 0, 1, 0)) %>% 
  count(bipf_zero) %>% 
  mutate(perc = n / sum(n))
