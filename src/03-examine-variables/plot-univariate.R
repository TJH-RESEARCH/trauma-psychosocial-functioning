library(patchwork)


# Create a unified theme for each of the three plots below ----------------


theme_hist <-
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "#e3e3e3"),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold", color = "#363636"),
    axis.title = element_text(face = "bold", color = "#363636")
  )


# Study 1 -----------------------------------------------------------------
(plot_bipf_1 <-
  data_1 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram(bins = 20, fill = "white", color = "black") + 
  scale_y_continuous(breaks = seq(0,40,5)) +
  labs(title = 'Distribution of Outcome Variable', # this serves as the title for the combined plots
       subtitle = 'Survey 1',
       x = 'Dysfunction',
       y = 'Count') +
   theme_hist
 
)

# Study 2 -----------------------------------------------------------------
(plot_bipf_2 <-
  data_2 %>% 
  ggplot(aes(bipf_score)) + 
   geom_histogram(bins = 20, fill = "white", color = "black") + 
  scale_y_continuous(breaks = seq(0,80,20)) +
  labs(subtitle = 'Survey 2',
       x = 'Dysfunction',
       y = 'Count') +
  theme_hist
 
)


# Study 3 -----------------------------------------------------------------
(plot_bipf_3 <-
  data_3 %>% 
  ggplot(aes(bipf_score)) + 
   geom_histogram(bins = 20, fill = "white", color = "black") + 
  scale_y_continuous(breaks = seq(0,40,5)) +
  labs(subtitle = 'Survey 3',
       x = 'Dysfunction',
       y = 'Count') +
   theme_hist
 
)


# Combine the plots
plot_histogram <-
  plot_bipf_1 / plot_bipf_2 / plot_bipf_3 +
  plot_layout(axis_titles = "collect")# +
  # patchwork::plot_annotation(title = "Distribution of Outcome Variable")

# Print to window
plot_histogram %>% print()

# Write to file
ggsave(here::here("output/plot-histogram.jpg"), width = 6, height = 6)
