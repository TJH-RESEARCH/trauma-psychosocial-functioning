data_1 %>% 
  ggplot(aes(pcl_total, bipf_score)) + geom_point()

data_1 %>% 
  ggplot(aes(pcl_total, bipf_total)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "PCL (Symptoms)", y = "bIPF (Difficulty Functioning)") + theme_light()

