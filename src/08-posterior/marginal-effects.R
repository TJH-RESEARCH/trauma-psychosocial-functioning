library(MetBrewer)
library(marginaleffects)
library(posterior)
library(patchwork)

# Average Marginal Effect
## reports the median of the posterior distribution as their main estimates.
options(marginaleffects_posterior_interval = "hdi")
options(marginaleffects_posterior_center = median)

## Non-zero process, in log units:
model_1_hurdle |> avg_slopes(dpar = 'mu', type = "link", variables = 'pcl_total')

## Non-zero process, in response units:
model_1_hurdle |> avg_slopes(dpar = 'mu', type = "response", variables = 'pcl_total')

## Non-zero process, all parameters:
model_1_hurdle %>% marginaleffects::avg_slopes(dpar = 'mu') |> tibble()

## Zero process (probability of a zero outcome)
model_1_hurdle |> avg_slopes(dpar = "hu", type = 'response', variables = 'pcl_total')
model_1_hurdle |> avg_slopes(dpar = "hu", type = "link", variables = 'pcl_total')

## Zero process, all parameters:
model_1_hurdle %>% marginaleffects::avg_slopes(dpar = 'hu') |> tibble()

## Plot Average Marginal Effects
plot_slopes(
  model_1_hurdle,
  variables = "pcl_total",
  condition = "pcl_total",
  rug = TRUE
  ) +
  labs(
    x = 'PCL',
    y = "AME", 
    title = 'Average Marginal Effects',
    subtitle = 'Non-Zero Process') +
  theme_custom
  
plot_slopes(
    model_1_hurdle,
    dpar = "hu",
    variables = "pcl_total",
    condition = "pcl_total",
    rug = TRUE,
    type = 'response'
    ) +
  labs(
    x = 'PCL',
    y = "Log Odds", 
    title = 'Average Marginal Effects',
    subtitle = 'Zero Process',
    caption = "At the lowest end of PCL scores, a 1-SD increase in \n PCL reduces the log-odds of having no difficulty by 0.2."
    ) +
  theme_custom
# The average marginal effects being around -.2 indicates that an increase in PCL at those levels are associated with about a decrease in the log-odds of being a zero. That is more PCL (PTSD symptoms) leads to less chance of having no difficulty functioning, by a log-odds of -.2. But as PTSD symptoms increases, this marginal effect tapers off. So that at higher levels, the impact of a change is less extreme. In other words, if having mild-to-moderate symptoms doesn't lead to some difficulty, then having extreme symptoms wont either. But the AME is never zero, it only approaches 0, so increases in PTSD symptoms still increase the chance of having some difficulty, only this increased chance is less at higher values of PCL. 


# Marginal Effects at the mean
model_1_hurdle %>% marginaleffects::avg_slopes(newdata = 'mean')







# Draws of average predictions
draws_avg_reds <- 
  model_1_hurdle %>% 
  marginaleffects::avg_predictions() %>% 
  get_draws()

draws_avg_reds %>% ggplot(aes(draw)) + geom_density()




# Plot adjusted predictions as a function of PCL
plot_predictions(
  model_1_hurdle,
  condition = "pcl_total") +
  labs(y = "mu") +
plot_predictions(
    model_1_hurdle,
    dpar = "hu",
    condition = "pcl_total") +
  labs(y = "hu")



