

data_1 %>% 
  ggplot(aes(pcl_total, bipf_score)) + 
  geom_point(size = 2, alpha = .8, position = 'jitter') +
  geom_smooth(se = FALSE, span = 1) +
  labs(x = "PCL (PTSD Symptoms)", y = "bIPF (Difficulty Functioning)") +
  theme_custom


data_2 %>% 
  ggplot(aes(mios_total, bipf_score)) + 
  geom_point(size = 2, alpha = .8, position = 'jitter') +
  geom_smooth(se = FALSE, span = 1) +
  labs(x = "MIOS (Moral Injury Symptoms)", y = "bIPF (Difficulty Functioning)") +
  theme_custom


# Study 3 -----------------------------------------------------------------
data_3 %>% 
  ggplot(aes(mios_total, bipf_score)) + 
  geom_point(size = 2, alpha = .8, position = 'jitter') +
  geom_smooth(se = FALSE, span = 1) +
  labs(x = "MIOS (Moral Injury Symptoms)", y = "bIPF (Difficulty Functioning)") +
  theme_custom

data_3 %>% 
  ggplot(aes(pcl_total, bipf_score)) + 
  geom_point(size = 2, alpha = .8, position = 'jitter') +
  geom_smooth(se = FALSE, span = 1) +
  labs(x = "PCL (PTSD Symptoms)", y = "bIPF (Difficulty Functioning)") +
  theme_custom

data_3 %>% 
  ggplot(aes(mios_total, bipf_score, color = pcl_total)) + 
  geom_point(size = 2, alpha = .8, position = 'jitter') +
  geom_smooth(se = FALSE, span = 1) +
  labs(x = "MIOS (Moral Injury Symptoms)", 
       y = "bIPF (Difficulty Functioning)",
       color = 'PCL (PTSD Symptoms)') +
  theme_custom
