
# Combined Model
plot_conditional_effects_3_interact <- 
  plot(conditional_effects(model_3_hurdle_interact), plot = FALSE)[[3]]$data %>%  
  # transform pcl from SD to original units
  #mutate(pcl_total = effect1__ * sd(data_3$pcl_total) + mean(data_3$pcl_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total_x_mios_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = colors_tam[1]) +
  #scale_x_continuous(limits = c(0,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,170), breaks = seq(0, 160, 40)) +
  labs(x = "Interaction Term (MIOS * PCL)", 
       y = "Predicted Dysfunction (bIPF)",
       subtitle = "Combined parts of the model (Hurdle + Gamma)") +
  theme_scatter


# Non-Zero Gamma Model
plot_conditional_effects_3_mu_interact <- 
  plot(conditional_effects(model_3_hurdle_interact, dpar = "mu"), plot = FALSE)[[3]]$data %>% 
  mutate(pcl_total = effect1__ * sd(data_3$pcl_total) + mean(data_3$pcl_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total_x_mios_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = colors_tam[3]) +
  #scale_x_continuous(limits = c(0,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,170), breaks = seq(0, 160, 40)) +
  labs(x = "Interaction Term (MIOS * PCL)", 
       y = "Predicted Dysfunction (bIPF)",
       subtitle = "Gamma part of the model") +
  theme_scatter


# Zero-Process Logistic Model
plot_conditional_effects_3_hu_interact <- 
  plot(conditional_effects(model_3_hurdle_interact, dpar = "hu"), plot = FALSE)[[3]]$data %>% 
  mutate(pcl_total = effect1__ * sd(data_3$pcl_total) + mean(data_3$pcl_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total_x_mios_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = colors_tam[5]) +
  #scale_x_continuous(limits = c(0,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,1), labels = label_percent()) +
  labs(x = "Interaction Term (MIOS * PCL)", 
       y = "Pr (Dysfunction = 0)",
       subtitle = "Hurdle part of the model") +
  theme_scatter


# Combine the plots
plot_conditional_effects_3_combo_interact <-
  (plot_conditional_effects_3_interact / plot_conditional_effects_3_mu_interact / plot_conditional_effects_3_hu_interact) +
  plot_layout(axes = "collect") +
  plot_annotation(title = "Conditional effects of MIOS x PCL interaction on dysfunction",
                  subtitle = "Conditional effects at the mean of all covariates (Sample 3)",
                  theme = theme(plot.title = element_text(face = "bold"),
                                plot.subtitle = element_text()))

# Save the combined plots
ggsave(plot = plot_conditional_effects_3_combo_interact, file = here::here("output/plot-conditional-effects-3-interact.jpg"), width = 6, height = 8)

