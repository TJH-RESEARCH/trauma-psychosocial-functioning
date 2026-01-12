# Plot histograms of the outcome variable

# Sample 1 ---------------------------------------------------------------------
plot_bipf_1 <-
  data_1 %>% 
  ggplot(aes(bipf_score)) + 
  geom_histogram(bins = 20, fill = "white", color = "black", linewidth = .3) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0,40,5)) + # limits consistent with Sample 3
  labs(title = 'Distribution of Outcome Variable', # this serves as the title for the combined plots
       subtitle = 'Survey 1',
       x = 'Dysfunction (bIPF)',
       y = 'Count') +
   theme_hist

# Sample 2 ---------------------------------------------------------------------
plot_bipf_2 <-
  data_2 %>% 
  ggplot(aes(bipf_score)) + 
   geom_histogram(bins = 20, fill = "white", color = "black", linewidth = .3) + 
  scale_y_continuous(breaks = seq(0,80,20)) +
  labs(subtitle = 'Survey 2',
       x = 'Dysfunction (bIPF)',
       y = 'Count') +
  theme_hist

# Sample 3 ---------------------------------------------------------------------
plot_bipf_3 <-
  data_3 %>% 
  ggplot(aes(bipf_score)) + 
   geom_histogram(bins = 20, fill = "white", color = "black", linewidth = .3) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0,40,5)) + # limits consistent with Sample 1
  labs(subtitle = 'Survey 3',
       x = 'Dysfunction (bIPF)',
       y = 'Count') +
   theme_hist


# Combine the plots ------------------------------------------------------------
plot_histogram <-
  plot_bipf_1 / plot_bipf_2 / plot_bipf_3 +
  plot_layout(axis_titles = "collect")

# Print to window --------------------------------------------------------------
plot_histogram %>% print()

# Write to file ----------------------------------------------------------------
ggsave(plot = plot_histogram, file = here::here("output/plot-histogram.jpg"), width = 6, height = 6)
