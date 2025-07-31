# PLOT POSTERIOR
library(MetBrewer)
library(ggdist)


# Non-Zero Outcomes -------------------------------------------------------
## For gamma models, coefficients are multiplicative, not additive as with standard OLS regression. 
## 1 coefficient of 1.5 unit, for example, means a one unit change in the predictor is associated with a 1.5x greater outcome
## also, the coefficients are in log. To transform them use: exp(coefficient)
## These means we are not (necessarily) concerned with the coefficient's relationship to 0, rather to 1. Because a exp(coefficient) of 1 would mean no change for an increase in the predictor 

draws_hurdle_2 %>%
  select(.chain, .iteration, .draw, b_mios_total) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  # tranform from log scale to something easier to understand:
  mutate(value = exp(value)) %>% 
  ggplot(aes(x = value#, fill = after_stat(x > .2 & x < .5) #experiement with fill after_stat
             )
         ) + 
  stat_slab(point_interval = mode_hdci, 
            aes(fill = after_stat(level)), color = "black", linewidth = .1, alpha = 1, .width = c(.95, 1)) +
  stat_spike(at = "Mode", linetype = "dotted", color =  "#0a3351", linewidth = .35) +
  stat_spike(
    at = function(x) hdci(x, .width = .95),
    size = 0, color =  "#0a3351", linewidth = 0.35, linetype = "dotted"
  ) +
  #tidybayes::stat_halfeye(fill = MetPalettes$Peru1[[1]][5]) +
  labs(y = "Posterior Density", 
       x = 'Gamma Coefficient<br>(Moral Injury Symptoms)', 
       title = 'Veterans with worse Moral Injury had more dysfunction',
       subtitle = "<span style = 'color:#0a3351'> **Posterior distribution of regression coefficient**</span> (Study 2)",
       caption = "Note: Gamma coefficients are multiplicative, not additive. A coefficient of 1.44, the posterior mean,<br>means a one-unit increase in the predictor is associated with a 144% increase in the outcome.<br>The coefficients have been exponentiated from the log scale to the original outcome scale."
       ) + 
  # I can't get stat_spike to add the dots to the end of the HDCI line segements.... maybe a way to do this programmatically instead of annotating, but for now: 
  annotate(geom = "point", x = 1.21305, y = .12595, color =  "#0a3351") + # left
  annotate(geom = "point", x = 1.67175, y = .14625, color =  "#0a3351") + # right
 
   # Label the stat at the intervals
  annotate(geom = "text", label = "1.42", x = 1.415, y = .955, color = "#134b73", fontface = "bold") +
  annotate(geom = "text", label = "1.22", x = 1.19, y = .205, color = "#134b73", fontface = "bold") +
  annotate(geom = "text", label = "1.68", x = 1.7, y = .205, color = "#134b73", fontface = "bold") +
  
  # Label the intervals
  #annotate(geom = "text", label = "66%", x = 1.35, y = .035, color = "#134b73", fontface = "bold", size = 3) +
  #annotate(geom = "text", label = "66%", x = 1.5, y = .035, color = "#134b73", fontface = "bold", size = 3) +
  annotate(geom = "text", label = "95%", x = 1.2475, y = .035, color = "#abc9c8", fontface = "bold", size = 3) +
  annotate(geom = "text", label = "95%", x = 1.64, y = .035, color = "#abc9c8", fontface = "bold", size = 3) +
  
  theme(
    legend.position = 'none',
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#3e3e3e"),
    axis.title = element_markdown(size = 10, face = "bold"),
    axis.title.y = element_markdown(margin = margin(r = 20)),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e")
  ) + 
  scale_fill_manual(values = c("#9ECAE1", "#6BAED6", "#3182BD", "#08519C", "#72aeb6", "#4692b0")) +
  #MetBrewer::scale_fill_met_d(name = "Hokusai2") +
  scale_x_continuous(breaks = seq(1, 2, .5), limits = c(1, 2)) +
  scale_thickness_shared()
ggsave(here::here("output/plot-posterior-2.jpg"), width = 6, height = 4)

# MetBrewer::colorblind_palettes
# MetBrewer::display_all()
# MetBrewer::MetPalettes$Hokusai2[[1]]
#c("#abc9c8", "#72aeb6", "#4692b0", "#2f70a1", "#134b73", "#0a3351")
# RColorBrewer::brewer.pal(6, name = 'Blues')
#"#EFF3FF" "#C6DBEF" "#9ECAE1" "#6BAED6" "#3182BD" "#08519C"

# Hurdle: Log Odds --------------------------------------------------------
draws_hurdle_2 %>%
  select(.chain, .iteration, .draw, Intercept_hu, b_hu_mios_total) %>% 
  # Rename Variables 
  rename(Intercept = Intercept_hu, MIOS = b_hu_mios_total) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name, fill = name)) +
  tidybayes::stat_halfeye() +
  #scale_y_discrete(breaks = NULL) +
  labs(y = NULL, 
       x = 'Coefficient (Log Odds)', 
       title = 'Zero Process',
       subtitle = 'Logs Odds of scoring zero on bIPF') + 
  theme_custom +
  theme(legend.position = 'none')


# Hurdle: Probability -------------------------------------------------------------
draws_hurdle_2 %>%
  select(.chain, .iteration, .draw, Intercept_hu, b_hu_mios_total) %>% 
  # Rename Variables 
  rename(Intercept = Intercept_hu, MIOS = b_hu_mios_total) %>% 
  mutate(
    Intercept = exp(Intercept)/(1+exp(Intercept)),
    MIOS = exp(MIOS)/(1+exp(MIOS)),
    ) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name, fill = name)) +
  tidybayes::stat_halfeye() +
  #scale_y_discrete(breaks = NULL) +
  labs(y = NULL, 
       x = 'Coefficient (Probability)', 
       title = 'Zero Process',
       subtitle = 'Probability of scoring zero on bIPF') + 
  theme_custom +
  theme(legend.position = 'none')
