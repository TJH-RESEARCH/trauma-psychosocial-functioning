
# Combined Model
plot_conditional_effects_1 <- 
  plot(conditional_effects(model_1_hurdle), plot = FALSE)[[1]]$data %>%  
  # transform PCL from SD to original units
  mutate(pcl_total = effect1__ * sd(data_1$pcl_total) + mean(data_1$pcl_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = colors_tam[1]) +
  scale_x_continuous(limits = c(-1,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,115), breaks = seq(0, 100, 20)) +
  labs(x = "Posttraumatic Symptoms (PCL)", 
       y = "Predicted Dysfunction (bIPF)",
       subtitle = "Combined parts of the model (Hurdle + Gamma)") +
  theme_scatter

# Non-Zero Gamma Model
plot_conditional_effects_1_mu <- 
  plot(conditional_effects(model_1_hurdle, dpar = "mu"), plot = FALSE)[[1]]$data %>% 
  mutate(pcl_total = effect1__ * sd(data_1$pcl_total) + mean(data_1$pcl_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = colors_tam[3]) +
  scale_x_continuous(limits = c(-1,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,115), breaks = seq(0, 100, 20)) +
  labs(x = "Posttraumatic Symptoms (PCL)", 
       y = "Predicted Dysfunction (bIPF)",
       subtitle = "Gamma part of the model") +
  theme_scatter

# Zero-Process Logistic Model
plot_conditional_effects_1_hu <- 
  plot(conditional_effects(model_1_hurdle, dpar = "hu"), plot = FALSE)[[1]]$data %>% 
  mutate(pcl_total = effect1__ * sd(data_1$pcl_total) + mean(data_1$pcl_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = colors_tam[5]) +
  scale_x_continuous(limits = c(-1,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,1), labels = label_percent()) +
  labs(x = "Posttraumatic Symptoms (PCL)", 
       y = "Pr (Dysfunction = 0)",
       subtitle = "Hurdle part of the model") +
  theme_scatter


# Combine the different plots
(plot_conditional_effects_1 / plot_conditional_effects_1_mu / plot_conditional_effects_1_hu) +
  plot_layout(axes = "collect") +
  plot_annotation(title = "Conditional effects of posttraumatic symptoms on dysfunction",
                  subtitle = "Conditional effects at the mean of all covariates (Sample 1)",
                  theme = theme(plot.title = element_text(face = "bold"),
                                plot.subtitle = element_text()))

# Save the combined plots
ggsave(here::here("output/plot-conditional-effects-1.jpg"), width = 6, height = 8)

