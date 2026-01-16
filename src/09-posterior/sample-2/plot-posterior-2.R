# PLOT POSTERIOR


# Non-Zero Outcomes -------------------------------------------------------
## For gamma models, coefficients are multiplicative, not additive as with standard OLS regression. 
## 1 coefficient of 1.5 unit, for example, means a one unit change in the predictor is associated with a 1.5x greater outcome
## also, the coefficients are in log. To transform them use: exp(coefficient)
## These means we are not (necessarily) concerned with the coefficient's relationship to 0, rather to 1. Because a exp(coefficient) of 1 would mean no change for an increase in the predictor 

plot_posterior_gamma_2 <-
  draws_hurdle_2 %>%
  select(.chain, .iteration, .draw, b_mios_total) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  # tranform from log scale to something easier to understand:
  mutate(value = exp(value)) %>% 
  ggplot(aes(x = value)) + 
  stat_slab(point_interval = mode_hdci, 
            aes(fill = after_stat(level)), color = "black", linewidth = .1, alpha = 1, .width = c(.95, 1)) +
  stat_spike(at = "Mode", linetype = "dotted", color =  "#0a3351", linewidth = .35) +
  stat_spike(
    at = function(x) hdci(x, .width = .95),
    size = 0, color =  "#0a3351", linewidth = 0.35, linetype = "dotted"
  ) +
  #tidybayes::stat_halfeye(fill = MetPalettes$Peru1[[1]][5]) +
  labs(y = "Posterior Density", 
       x = 'Gamma Coefficient\n(Moral Injury Symptoms)', 
       subtitle = "<span style = 'color:#0a3351'> **Posterior distribution of the multiplicative effect**</span> (Survey 2)",
       caption = "Note. Multiplicative effects indicate the factor by which the depdent variable is multiplied for each<br>1SD change in the independent variable."
       ) + 
  theme_custom +
  scale_fill_manual(values = c("#9ECAE1", "#6BAED6", "#3182BD", "#08519C", "#72aeb6", "#4692b0")) +
  #MetBrewer::scale_fill_met_d(name = "Hokusai2") +
  scale_x_continuous(breaks = seq(1, 2, .2), limits = c(1, 2)) +
  scale_thickness_shared()

## Print to console
plot_posterior_gamma_2 %>% print()

## Save to file
ggsave(plot = plot_posterior_gamma_2, file = here::here("output/plot-posterior-gamma-2.jpg"), width = 6, height = 4)




# Hurdle: Probability -------------------------------------------------------------
plot_posterior_logistic_2 <-
  draws_hurdle_2 %>%
  select(.chain, .iteration, .draw, b_hu_mios_total) %>% 
  # Rename Variables 
  rename(mios = b_hu_mios_total) %>% 
  mutate(
    # creates odds ratios from the log odds
    mios = exp(mios)/(1+exp(mios)),
    ) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value)) + 
  stat_slab(point_interval = mode_hdci, 
            aes(fill = after_stat(level)), color = "black", linewidth = .1, alpha = 1, .width = c(.95, 1)) +
  stat_spike(at = "Mode", linetype = "dotted", color =  "#0a3351", linewidth = .35) +
  stat_spike(
    at = function(x) hdci(x, .width = .95),
    size = 0, color =  "#0a3351", linewidth = 0.35, linetype = "dotted"
  ) +
  labs(y = NULL, 
       x = 'Logistic Coefficient\n(Odds Ratio)', 
       subtitle = "<span style = 'color:#0a3351'> **Posterior distribution of the odds ratios**</span> (Survey 2)",
       caption = "Note. Odds ratios indicate the change in probability associated with a 1 SD increase in the predictor") + 
  theme_custom +
  scale_fill_manual(values = c("#9ECAE1", "#6BAED6", "#3182BD", "#08519C", "#72aeb6", "#4692b0")) +
  #MetBrewer::scale_fill_met_d(name = "Hokusai2") +
  scale_x_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  scale_thickness_shared()

## Print to console
plot_posterior_logistic_2 %>% print()

## Save to file
ggsave(plot = plot_posterior_logistic_2, file = here::here("output/plot-posterior-logistic-2.jpg"), width = 6, height = 4)


