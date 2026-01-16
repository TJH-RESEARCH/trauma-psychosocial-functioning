results_posterior_1 <-
  draws_hurdle_1 %>% 
  tidybayes::summarise_draws(default_summary_measures()) %>% 
  filter(str_detect(variable, "b_")) %>% 
  mutate(across(where(is.numeric), exp)) %>% 
  mutate(term = ifelse(str_detect(variable, "b_hu"), "Odds Ratio", "Multiplicative")) %>% 
  arrange(term, variable)
  
results_posterior_1 %>% print(n = 500)

# Write to file
results_posterior_1 %>% write_csv(here::here('output/results-posterior-1.csv'))

