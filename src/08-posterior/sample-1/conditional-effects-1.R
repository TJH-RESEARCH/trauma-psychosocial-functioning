
# Combined Model
plot_conditional_effects_1 <- 
  plot(conditional_effects(model_1_hurdle), plot = FALSE)[[1]]$data %>%  
  # transform PCL from SD to original units
  mutate(pcl_total = effect1__ * sd(data_1$pcl_total) + mean(data_1$pcl_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = clrs[1]) +
  scale_y_continuous(limits = c(0,115), breaks = seq(0, 100, 20)) +
  labs(x = "Posttraumatic Symptoms (PCL)", 
       y = "Predicted\nDysfunction (bIPF)",
       subtitle = "Combined parts of the model (\"mu\" and \"hu\")") +
  theme_marginal_fx

# Non-Zero Gamma Model
plot_conditional_effects_1_mu <- 
  plot(conditional_effects(model_1_hurdle, dpar = "mu"), plot = FALSE)[[1]]$data %>% 
  mutate(pcl_total = effect1__ * sd(data_1$pcl_total) + mean(data_1$pcl_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = clrs[3]) +
  scale_y_continuous(limits = c(0,115), breaks = seq(0, 100, 20)) +
  labs(x = "Posttraumatic Symptoms (PCL)", 
       y = "Predicted\nDysfunction (bIPF)",
       subtitle = "Gamma part of the model (dpar = \"mu\")") +
  theme_marginal_fx

# Zero-Process Logistic Model
plot_conditional_effects_1_hu <- 
  plot(conditional_effects(model_1_hurdle, dpar = "hu"), plot = FALSE)[[1]]$data %>% 
  mutate(pcl_total = effect1__ * sd(data_1$pcl_total) + mean(data_1$pcl_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = pcl_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = clrs[5]) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "Posttraumatic Symptoms (PCL)", 
       y = "Predicted probability\nof 0 dysfunction",
       subtitle = "Hurdle part of the model (dpar = \"hu\")") +
  theme_marginal_fx


# Combine the different plots
(plot_conditional_effects_1 / plot_conditional_effects_1_mu / plot_conditional_effects_1_hu) +
  plot_annotation(title = "Conditional effects of postraumatic symptoms on dysfunction",
                  subtitle = "Conditional effects at the mean of all covariates",
                  theme = theme(plot.title = element_text(face = "bold"),
                                plot.subtitle = element_text()))

# Save the combined plots
ggsave(here::here("output/plot-conditional-effects-1.jpg"), width = 6, height = 8)

