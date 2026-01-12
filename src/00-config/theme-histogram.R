
# Theme for histograms
theme_hist <-
  theme(
    
    # Panel customization
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid.major.y = element_line(color = "#e3e3e3"),
    panel.grid.major.x = element_blank(),
    
    # Axis customization
    axis.line = element_blank(),
    #axis.line = element_line(color = "#3e3e3e"),
    axis.title = element_markdown(size = 10, face = "bold"),
    axis.title.y = element_markdown(margin = margin(r = 20)),
    axis.ticks = element_blank(),
    
    # Title customization
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e")
  )
