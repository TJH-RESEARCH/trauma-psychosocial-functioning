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
  stat_slab(point_interval = mode_hdci, aes(fill = after_stat(level)), color = "black", linewidth = .1, .width = c(.95, 1)) +
  stat_spike(at = "Mode", linetype = "dotted", color = "black", linewidth = 0.35) +
  stat_spike(
    at = function(x) hdci(x, .width = .95),
    size = 0, color = "black", linewidth = 0.35, linetype = "dotted"
  ) +
  labs(y = "Posterior Density", 
       x = 'Gamma Coefficient<br>(PTSD x Moral Injury Interaction)', 
       title = 'PTSD & Moral Injury did not interact to worsen dysfunction',
       subtitle = "<span style = 'color:#238B45'> **Posterior distribution of interaction coefficient**</span> (Study 3)",
       caption = 
       "Density of posterior probability distribution of the interaction term coefficient (exponentiated).<br>
       Gamma Regression fit to the non-zero outcome cases. Gamma coefficients are multiplicative.<br>
       A coefficient of .89 indicates a decrease in the outcome for an increase in the predictor."
       ) + 
  
  # I can't get stat_spike to add the dots to the end of the HDCI line segements.... maybe a way to do this programmatically instead of annotating, but for now: 
  annotate(geom = "point", x = .7684, y = .1256, color = "black") + # left
  annotate(geom = "point", x = 1.016, y = .143, color = "black") + # right
  
  # Label the stat at the intervals
  annotate(geom = "text", label = ".89", x = .89, y = .96, color = "#238B45", fontface = "bold") +
  annotate(geom = "text", label = ".78", x = .75, y = .175, color = "#238B45", fontface = "bold") +
  annotate(geom = "text", label = "1.03", x = 1.04, y = .175, color = "#238B45", fontface = "bold") +
  
  # Label the intervals
  annotate(geom = "text", label = "95%", x = .785, y = .035, color = "#A1D99B", fontface = "bold", size = 3) +
  annotate(geom = "text", label = "95%", x = 1, y = .035, color = "#A1D99B", fontface = "bold", size = 3) +
  
  
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
  scale_fill_manual(values = c("#A1D99B", "#31A354")) +
  #MetBrewer::scale_fill_met_d(name = "Derain") +
  scale_x_continuous(breaks = seq(.65, 1.15, .25), limits = c(.65, 1.15)) +
  scale_thickness_shared()
ggsave(here::here("output/plot-posterior-3-interact.jpg"), width = 6, height = 4)

#MetBrewer::MetPalettes$Derain[[1]]
#MetBrewer::MetPalettes$Archambault[[1]]
c("#88a0dc", "#381a61", "#7c4b73", "#ed968c", "#ab3329", "#e78429", "#f9d14a")
c("#efc86e", "#97c684", "#6f9969", "#aab5d5", "#808fe1", "#5c66a8", "#454a74")

#MetBrewer::colorblind_palettes
#MetBrewer::display_all()
# RColorBrewer::display.brewer.all()
# RColorBrewer::brewer.pal(n = 9, name = "Greens")
"#E5F5E0" "#A1D99B" "#31A354" "#EDF8E9" "#BAE4B3" "#74C476" "#238B45"


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
