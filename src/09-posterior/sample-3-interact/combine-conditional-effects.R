# Combine the conditional effects plots of the model components


# Logistic zero-process models: PCL Focus -------------------------------------------------

## Update labels
plot_conditional_effects_1_hu <- plot_conditional_effects_1_hu + labs(subtitle = "Sample 1")
plot_conditional_effects_3_hu_pcl <- plot_conditional_effects_3_hu_pcl + labs(subtitle = "Sample 3")
plot_conditional_effects_3_hu_interact <- plot_conditional_effects_3_hu_interact + labs(subtitle = "Sample 3 Interaction")

(plot_conditional_effects_1_hu /
plot_conditional_effects_3_hu_pcl / 
plot_conditional_effects_3_hu_interact) +  
  plot_layout(axes = "collect_y") +
  plot_annotation(title = "Conditional effects of posttraumatic symptoms on dysfunction",
                  subtitle = "Conditional effects at the mean of all covariates",
                  theme = theme(plot.title = element_text(face = "bold"),
                                plot.subtitle = element_text()))


# Logistic zero-process models: MIOS Focus -------------------------------------------------
plot_conditional_effects_2_hu <- plot_conditional_effects_2_hu + labs(subtitle = "Sample 2")
plot_conditional_effects_3_hu_mios <- plot_conditional_effects_3_hu_mios + labs(subtitle = "Sample 3")

(plot_conditional_effects_2_hu /
 plot_conditional_effects_3_hu_mios / 
 plot_conditional_effects_3_hu_interact) +  
  plot_layout(axes = "collect") +  
  plot_layout(axes = "collect_y") +
  plot_annotation(title = "Conditional effects of moral injury symptoms on dysfunction",
                  subtitle = "Conditional effects at the mean of all covariates",
                  theme = theme(plot.title = element_text(face = "bold"),
                                plot.subtitle = element_text()))



# Gamma non-zero models --------------------------------------------------------
(plot_conditional_effects_1_mu /
plot_conditional_effects_2_mu /
plot_conditional_effects_3_mu_mios) +
  plot_layout(axes = "collect") +
  plot_annotation(title = "Conditional effects of posttraumatic symptoms on dysfunction",
                  subtitle = "Conditional effects at the mean of all covariates (Sample 3)",
                  theme = theme(plot.title = element_text(face = "bold"),
                                plot.subtitle = element_text()))


# Combined Hurdle + Gamma Models -----------------------------------------------