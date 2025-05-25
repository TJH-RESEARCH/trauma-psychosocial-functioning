library(ggplot2)

theme_custom <-
  
  theme(
    
    # Backgrounds
    panel.background = element_rect(fill = "ghostwhite", color = 'white'),
    plot.background = element_rect(fill = "ghostwhite", color = 'black'),
    legend.background = element_rect(fill = "ghostwhite"),
    
    # Grid
    panel.grid.major = element_line(color = 'grey85'),
    panel.grid.minor = element_line(color = 'grey85'),
    
    # Text
    plot.title = element_text(face = "bold", size = 16, family = 'Arial', hjust = .5),
    plot.subtitle = element_text(face = "plain", size = 16, family = 'Arial', hjust = .5),
    axis.title.x = element_text(face = "bold", size = 14, family = 'Arial'),
    axis.title.y = element_text(face = "bold", size = 14, family = 'Arial'),
    axis.text = element_text(face = "plain", size = 12, family = 'Arial'),
    
    # Axes
    axis.line = element_line(color = 'black'),
    axis.ticks = element_blank(),
    
    # Legend
    legend.position = 'bottom',
    legend.title = element_text(face = "bold", size = 12, family = 'Arial'),
    legend.text = element_text(face = "plain", size = 12, family = 'Arial')
  )
