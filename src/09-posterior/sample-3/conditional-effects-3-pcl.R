
# Combined Model
plot_conditional_effects_3_pcl <- 
  plot(conditional_effects(model_3_hurdle), plot = FALSE)[[1]]$data %>%  
  # transform pcl from SD to original units
  mutate(pcl_total = effect1__ * sd(data_3$pcl_total, na.rm = TRUE) + mean(data_3$pcl_total, na.rm = TRUE)) %>% 
  rename(bipf_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total, y = bipf_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = colors_tam[1]) +
  scale_x_continuous(limits = c(-1,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,170), breaks = seq(0, 160, 40)) +
  labs(x = "Posttraumatic Symptoms (PCL)", 
       y = "Predicted Dysfunction (bIPF)",
       subtitle = "Combined parts of the model (Hurdle + Gamma)") +
  theme_scatter


# Non-Zero Gamma Model
plot_conditional_effects_3_mu_pcl <- 
  plot(conditional_effects(model_3_hurdle, dpar = "mu"), plot = FALSE)[[1]]$data %>% 
  mutate(pcl_total = effect1__ * sd(data_3$pcl_total, na.rm = TRUE) + mean(data_3$pcl_total, na.rm = TRUE)) %>% 
  rename(bipf_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total, y = bipf_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = colors_tam[3]) +
  scale_x_continuous(limits = c(-1,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,170), breaks = seq(0, 160, 40)) +
  labs(x = "Posttraumatic Symptoms (PCL)", 
       y = "Predicted Dysfunction (bIPF)",
       subtitle = "Gamma part of the model") +
  theme_scatter


# Zero-Process Logistic Model
plot_conditional_effects_3_hu_pcl <- 
  plot(conditional_effects(model_3_hurdle, dpar = "hu"), plot = FALSE)[[1]]$data %>% 
  mutate(pcl_total = effect1__ * sd(data_3$pcl_total, na.rm = TRUE) + mean(data_3$pcl_total, na.rm = TRUE)) %>% 
  rename(bipf_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total, y = bipf_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = colors_tam[5]) +
  scale_x_continuous(limits = c(-1,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,1), labels = label_percent()) +
  labs(x = "Posttraumatic Symptoms (PCL)", 
       y = "Pr (Dysfunction = 0)",
       subtitle = "Hurdle part of the model") +
  theme_scatter


# Combine the plots
plot_conditional_effects_3_combo_pcl <-
  (plot_conditional_effects_3_pcl / plot_conditional_effects_3_mu_pcl / plot_conditional_effects_3_hu_pcl) +
  plot_layout(axes = "collect") +
  plot_annotation(title = "Conditional effects of posttraumatic symptoms on dysfunction",
                  subtitle = "Conditional effects at the mean of all covariates (Sample 3)",
                  theme = theme(plot.title = element_text(face = "bold"),
                                plot.subtitle = element_text()))

# Save the combined plots
ggsave(plot = plot_conditional_effects_3_combo_pcl, file = here::here("output/plot-conditional-effects-3-pcl.jpg"), width = 6, height = 8)

