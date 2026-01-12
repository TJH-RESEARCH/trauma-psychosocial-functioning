

results_posterior_3_interact <-
  draws_hurdle_3_interact %>% 
  tidybayes::summarise_draws(default_summary_measures()) %>% 
  filter(str_detect(variable, "b_")) %>% 
  mutate(across(where(is.numeric), exp)) %>% 
  mutate(term = ifelse(str_detect(variable, "b_hu"), "Odds Ratio", "Multiplicative")) %>% 
  arrange(term, variable)

results_posterior_3_interact %>% print(n = 500)

# Write to file
results_posterior_3_interact %>% write_csv(here::here('output/results-posterior-3-interact.csv'))



