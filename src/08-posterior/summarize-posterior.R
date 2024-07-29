draws_ord_log_multivariate %>% tidybayes::summarise_draws()
draws_ord_log_multivariate %>% tidybayes::mode_hdi(beta_1, .width = .95) %>% 
  mutate(across(where(is.numeric), ~ plogis(.x), .names = "p_{.col}")) %>% 
  select(-p_.width)
