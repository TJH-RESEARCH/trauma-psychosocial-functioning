results_posterior_3 <-
  draws_hurdle_3 %>% 
  tidybayes::summarise_draws(default_summary_measures()) %>% 
  filter(str_detect(variable, "b_")) %>% 
  mutate(across(where(is.numeric), exp)) %>% 
  mutate(term = ifelse(str_detect(variable, "b_hu"), "Odds Ratio", "Multiplicative")) %>% 
  arrange(term, variable)
  
# Print to console
results_posterior_3 %>% print(n = 500)

# Write to file
results_posterior_3 %>% write_csv(here::here('output/results-posterior-3.csv'))


