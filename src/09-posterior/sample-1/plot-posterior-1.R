# PLOT POSTERIOR


# Non-Zero Outcomes -------------------------------------------------------
## For gamma models, coefficients are multiplicative, not additive as with standard OLS regression. 
## 1 coefficient of 1.5 unit, for example, means a one unit change in the predictor is associated with a 1.5x greater outcome
## also, the coefficients are in log. To transform them use: exp(coefficient)
## These means we are not (necessarily) concerned with the coefficient's relationship to 0, rather to 1. Because a exp(coefficient) of 1 would mean no change for an increase in the predictor 

plot_posterior_gamma_1 <-
  draws_hurdle_1 %>%
  select(.chain, .iteration, .draw, b_pcl_total) %>% 
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
       x = 'Gamma Coefficient\n(Posttraumatic Symptoms)', 
       subtitle = "<span style = 'color:#0a3351'> **Posterior distribution of the multiplicative effect**</span> (Survey 1)",
       caption = "Note. Multiplicative effects indicate the factor by which the depdent variable is multiplied for each<br>1SD change in the independent variable."
       ) + 
  # I can't get stat_spike to add the dots to the end of the HDCI line segements.... maybe a way to do this programmatically instead of annotating, but for now: 
  # annotate(geom = "point", x = 1.18725, y = .145, color =  "#0a3351") + # left
  # annotate(geom = "point", x = 1.523, y = .133, color =  "#0a3351") + # right

  # Label the stat at the intervals
  annotate(geom = "text", label = "1.35", x = 1.3355, y = .955, color = "#134b73", fontface = "bold") +
  # annotate(geom = "text", label = "1.22", x = 1.19, y = .205, color = "#134b73", fontface = "bold") +
  # annotate(geom = "text", label = "1.68", x = 1.7, y = .205, color = "#134b73", fontface = "bold") +

  # Label the intervals
  annotate(geom = "text", label = "66%", x = 1.22, y = .035, color = "#134b73", fontface = "bold", size = 3) +
  annotate(geom = "text", label = "66%", x = 1.49, y = .035, color = "#134b73", fontface = "bold", size = 3) +
  annotate(geom = "text", label = "95%", x = 1.09175, y = .035, color = "#abc9c8", fontface = "bold", size = 3) +
  annotate(geom = "text", label = "95%", x = 1.69, y = .035, color = "#abc9c8", fontface = "bold", size = 3) +

  theme_custom +
  scale_fill_manual(values = c("#9ECAE1", "#6BAED6", "#3182BD", "#08519C", "#72aeb6", "#4692b0")) +
  #MetBrewer::scale_fill_met_d(name = "Hokusai2") +
  scale_x_continuous(breaks = seq(1, 2, .2), limits = c(1, 2)) +
  scale_thickness_shared()
ggsave(plot = plot_posterior_gamma_1, file = here::here("output/plot-posterior-gamma-1.jpg"), width = 6, height = 4)

# MetBrewer::colorblind_palettes
# MetBrewer::display_all()
# MetBrewer::MetPalettes$Hokusai2[[1]]
#c("#abc9c8", "#72aeb6", "#4692b0", "#2f70a1", "#134b73", "#0a3351")
# RColorBrewer::brewer.pal(6, name = 'Blues')
#"#EFF3FF" "#C6DBEF" "#9ECAE1" "#6BAED6" "#3182BD" "#08519C"



# Hurdle: Probability -------------------------------------------------------------
plot_posterior_logistic_1 <-
  draws_hurdle_1 %>%
  select(.chain, .iteration, .draw, b_hu_pcl_total) %>% 
  # Rename Variables 
  rename(PCL = b_hu_pcl_total) %>% 
  mutate(
    # creates odds ratios from the log odds
    PCL = exp(PCL)/(1+exp(PCL)),
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
       subtitle = "<span style = 'color:#0a3351'> **Posterior distribution of the odds ratios**</span> (Survey 1)",
       caption = "Note. Odds ratios indicate the change in probability associated with a 1 SD increase in the predictor") + 
  theme_custom +
  scale_fill_manual(values = c("#9ECAE1", "#6BAED6", "#3182BD", "#08519C", "#72aeb6", "#4692b0")) +
  #MetBrewer::scale_fill_met_d(name = "Hokusai2") +
  scale_x_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  scale_thickness_shared()

ggsave(plot = plot_posterior_logistic_1, file = here::here("output/plot-posterior-logistic-1.jpg"), width = 6, height = 4)


