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
  stat_spike(at = "Mode", linetype = "dotted", color = "#6f9969") +
  stat_spike(
    at = function(x) hdci(x, .width = .95),
    size = 0, color = "black", linewidth = 0.5,
  ) +
  labs(y = "Posterior Density", 
       x = 'Gamma Coefficient<br>(PTSD x Moral Injury Interaction)', 
       title = 'PTSD and Moral Injury did not interact to intensify difficulty',
       subtitle = "<span style = 'color:#454a74'> **Posterior distribution of interaction coefficient**</span> (Study 3)",
       caption = "Note: Gamma coefficients are multiplicative, not additive. A coefficient less than 1 indicates an expected<br>decrease in difficulty for a one-unit increase in the predictor. A coefficient of 1 represents no change.<br>The coefficients have been exponentiated from the log scale to the original outcome scale."
       ) + 
  
  # I can't get stat_spike to add the dots to the end of the HDCI line segements.... maybe a way to do this programmatically instead of annotating, but for now: 
  annotate(geom = "point", x = .7684, y = .1256, color = "black") + # left
  annotate(geom = "point", x = 1.016, y = .143, color = "black") + # right
  
  # Label the stat at the intervals
  annotate(geom = "text", label = ".89", x = .89, y = .96, color = "#454a74", fontface = "bold") +
  annotate(geom = "text", label = ".78", x = .75, y = .175, color = "#5c66a8", fontface = "bold") +
  annotate(geom = "text", label = "1.03", x = 1.04, y = .175, color = "#5c66a8", fontface = "bold") +
  
  # Label the intervals
  annotate(geom = "text", label = "66%", x = .8425, y = .035, color = "black", fontface = "bold", size = 3) +
  annotate(geom = "text", label = "66%", x = .93, y = .035, color = "black", fontface = "bold", size = 3) +
  annotate(geom = "text", label = "95%", x = .785, y = .035, color = "black", fontface = "bold", size = 3) +
  annotate(geom = "text", label = "95%", x = 1, y = .035, color = "black", fontface = "bold", size = 3) +
  
  
  # geom_text(x = 3.5, y = 10, label = "66%") +
  theme(
    legend.position = 'none',
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#3e3e3e"),
    axis.title = element_markdown(size = 10, face = "bold"),
    axis.title.y = element_markdown(margin = margin(r = 17)),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e")
  ) + 
  scale_fill_manual(values = c("#808fe1", "#5c66a8", "#454a74")) +
  #MetBrewer::scale_fill_met_d(name = "Derain") +
  scale_x_continuous(breaks = seq(.65, 1.15, .25), limits = c(.65, 1.15)) +
  scale_thickness_shared()
ggsave(here::here("output/plot-posterior-3-interact.jpg"), width = 6, height = 4)

#MetBrewer::MetPalettes$Derain[[1]]
#c("#efc86e", "#97c684", "#6f9969", "#aab5d5", "#808fe1", "#5c66a8", "#454a74")

#MetBrewer::colorblind_palettes
#MetBrewer::display_all()

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
