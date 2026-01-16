# Compare draws of the model to the observed data

# Posterior Predictive Check
pp_check(model_2_hurdle, ndraws = 100) +
  labs(title = "Posterior predictive check", 
       subtitle = "Survey 2",
       x = "Predicted Dysfunction (bIPF)",
       y = 'Density') +
  lims(x = c(0, 405)) +
  theme_ppc

## Save
ggsave(here::here("output/plot-ppc-2.jpg"), width = 6, height = 4)


pp_check(model_2_hurdle, type = "hist", ndraws = 11)
pp_check(model_2_hurdle, type = "error_hist", ndraws = 12)
pp_check(model_2_hurdle, type = "scatter_avg", ndraws = 100)

# Sample 2 model looks pretty good