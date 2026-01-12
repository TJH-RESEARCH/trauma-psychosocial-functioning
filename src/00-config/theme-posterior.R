theme_posterior <-
  theme(
  legend.position = 'none',
  panel.background = element_rect(fill = "white"),
  panel.grid.major.x = element_line(color = "#e9e9e9", linewidth = .3),
  panel.grid.minor.x = element_line(color = "#e9e9e9", linewidth = .1),
  panel.grid.major.y = element_blank(),
  #panel.grid.minor = element_blank(),
  axis.line = element_line(color = "#3e3e3e"),
  axis.title = element_markdown(size = 10, face = "bold"),
  axis.title.y = element_markdown(margin = margin(r = 20)),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_markdown(size = 14, face = "bold"),
  plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
  plot.caption = element_markdown(hjust = 0, color = "#7e7e7e")
)

