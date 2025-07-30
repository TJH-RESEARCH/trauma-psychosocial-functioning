# PLOT POSTERIOR
library(MetBrewer)
library(ggdist)

# Non-Zero Outcomes -------------------------------------------------------
draws_hurdle_3_interact %>%
  select(.chain, .iteration, .draw, b_pcl_total_x_mios_total) %>% 
  rename(MIOSxPCL = b_pcl_total_x_mios_total) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  mutate(value = exp(value)) %>%  # transform from log scale to something easier to understand:
  ggplot(aes(x = value)) + 
  stat_slab(point_interval = mode_hdci, aes(fill = after_stat(level)), .width = c(.66, .95, 1)) +
  stat_spike(at = "Mode", linetype = "dotted", color = "#ffd353") +
  #tidybayes::stat_halfeye(fill = MetPalettes$Peru1[[1]][5]) +
  labs(y = "Posterior Density", 
       x = 'Gamma Coefficient<br>(PTSD x Moral Injury Interaction)', 
       title = 'Interaction is not associated with intensified difficulty',
       subtitle = "<span style = 'color:#bb292c'> **Posterior distribution of interaction term coefficient**</span> (Study 3)",
       caption = "Note: Gamma coefficients are multiplicative, not additive. A coefficient less than 1 indicates an expected<br>decrease in difficulty for a one-unit increase in the predictor. The coefficients have been exponentiated<br>from the log scale to the original outcome scale."
       ) + 
 
  # geom_text(x = 3.5, y = 10, label = "66%") +
  theme(
    legend.position = 'none',
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#3e3e3e"),
    axis.title = element_markdown(size = 10, face = "bold"),
    #axis.title.y = element_markdown(margin = margin(r = 17)),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e")
  ) + 
  MetBrewer::scale_fill_met_d(name = "Tam") +
  scale_x_continuous(breaks = seq(.65, 1.15, .1), limits = c(.65, 1.15)) +
  scale_thickness_shared()
ggsave(here::here("output/plot-posterior-3-interact.jpg"), width = 6, height = 4)


# MetBrewer::MetPalettes$Tam[[1]]
#"#ffd353" "#ffb242" "#ef8737" "#de4f33" "#bb292c" "#9f2d55" "#62205f""#341648"



# Hurdle: Zero Process --------------------------------------------------------
draws_hurdle_3_interact %>%
  select(.chain, .iteration, .draw, b_hu_pcl_total_x_mios_total) %>% 
  rename(MIOSxPCL = b_hu_pcl_total_x_mios_total) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  mutate(value = exp(value)) %>% 
  ggplot(aes(x = value, y = name, fill = name)) +
  tidybayes::stat_halfeye() +
  labs(y = NULL, 
       x = 'Coefficient (Log Odds)', 
       title = 'Zero Process',
       subtitle = 'Logs Odds of scoring zero on bIPF') + 
  theme(legend.position = 'none')


# Hurdle: Probability -------------------------------------------------------------
draws_hurdle_3_interact %>%
  select(.chain, .iteration, .draw, b_hu_pcl_total_x_mios_total) %>% 
  rename(MIOSxPCL = b_hu_pcl_total_x_mios_total) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  mutate(value = exp(value)) %>% 
  ggplot(aes(x = value, y = name, fill = name)) +
  tidybayes::stat_halfeye() +
  #scale_y_discrete(breaks = NULL) +
  labs(y = NULL, 
       x = 'Coefficient (Probability)', 
       title = 'Zero Process',
       subtitle = 'Probability of scoring zero on bIPF') + 
  theme(legend.position = 'none')
