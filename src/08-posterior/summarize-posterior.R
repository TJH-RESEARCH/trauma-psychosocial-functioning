# Study 1 -----------------------------------------------------------------
draws_hurdle_1 %>% 
  tidybayes::summarise_draws() %>% 
  filter(variable == "b_pcl_total") %>% 
  mutate(across(where(is.numeric), exp))

## Mode
draws_hurdle_1 %>% tidybayes::mode_hdci(b_pcl_total) %>% mutate(across(where(is.numeric), exp))


# Study 2 -----------------------------------------------------------------
draws_hurdle_2 %>% 
  tidybayes::summarise_draws() %>% 
  filter(variable == "b_mios_total") %>% 
  mutate(across(where(is.numeric), exp)) # Exponentiate to transform from log odds to original scale (these are gamma regression coefficients which are multiplicative)

## Mode
draws_hurdle_2 %>% tidybayes::mode_hdci(b_mios_total) %>% mutate(across(where(is.numeric), exp))
draws_hurdle_2 %>% tidybayes::mode_hdci(b_pc_ptsd_positive_screen) %>% mutate(across(where(is.numeric), exp))


# Study 3 -----------------------------------------------------------------
draws_hurdle_3 %>% 
  tidybayes::summarise_draws() %>% 
  filter(variable == "b_mios_total" | variable == "b_pcl_total") %>% 
  mutate(across(where(is.numeric), exp)) 

## Mode
draws_hurdle_3 %>% tidybayes::mode_hdci(b_mios_total) %>% mutate(across(where(is.numeric), exp))
draws_hurdle_3 %>% tidybayes::mode_hdci(b_pcl_total) %>% mutate(across(where(is.numeric), exp))

## Interaction
draws_hurdle_3_interact %>% 
  tidybayes::summarise_draws() %>% 
  filter(variable == "b_pcl_total_x_mios_total") %>% 
  mutate(across(where(is.numeric), exp)) 
draws_hurdle_3_interact %>% tidybayes::mode_hdci(b_pcl_total_x_mios_total) %>% mutate(across(where(is.numeric), exp))
