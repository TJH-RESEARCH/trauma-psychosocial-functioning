draws_hurdle_1 %>% 
  tidybayes::summarise_draws() %>% 
  filter(variable == "b_pcl_total")


draws_hurdle_2 %>% 
  tidybayes::summarise_draws() %>% 
  filter(variable == "b_mios_total") %>% 
  mutate(across(where(is.numeric), exp)) # Exponentiate to transform from log odds to original scale (these are gamma regression coefficients which are multiplicative)

draws_hurdle_2 %>% tidybayes::mode_hdci(b_mios_total) %>% mutate(across(where(is.numeric), exp))
draws_hurdle_2 %>% tidybayes::mode_hdci(b_pc_ptsd_positive_screen)

data_baked_2 %>% 
  ggplot(aes(mios_total, bipf_score)) +
  geom_point()

