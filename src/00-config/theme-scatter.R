
# Theme for scatter plots

theme_scatter <-
  theme(
    # Panel customization
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_line(color = "#e9e9e9", linewidth = .3), # the real difference here is to have both X and Y grid lines for a scatter plot
    
    # Axis customization
    axis.line = element_blank(),
    axis.title = element_markdown(size = 10, face = "bold"),
    axis.title.y = element_markdown(margin = margin(r = 20)),
    axis.ticks = element_blank(),
    
    # Title customization
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e"),
    
    # Strip customization
    strip.background = element_rect(fill = "white")
    
  )
