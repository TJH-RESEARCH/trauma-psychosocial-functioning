
# Combined Model
plot_conditional_effects_3 <- 
  plot(conditional_effects(model_3_hurdle), plot = FALSE)[[1]]$data %>%  
  # transform mios from SD to original units
  mutate(mios_total = effect1__ * sd(data_3$mios_total) + mean(data_3$mios_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = mios_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = clrs[1]) +
  #scale_x_continuous(limits = c(0,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,170), breaks = seq(0, 160, 40)) +
  labs(x = "Moral Injury Symptoms (MIOS)", 
       y = "Predicted\nDysfunction (bIPF)",
       subtitle = "Combined parts of the model (\"mu\" and \"hu\")") +
  theme_marginal_fx


# Non-Zero Gamma Model
plot_conditional_effects_3_mu <- 
  plot(conditional_effects(model_3_hurdle, dpar = "mu"), plot = FALSE)[[1]]$data %>% 
  mutate(mios_total = effect1__ * sd(data_3$mios_total) + mean(data_3$mios_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = mios_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = clrs[3]) +
  #scale_x_continuous(limits = c(0,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0,170), breaks = seq(0, 160, 40)) +
  labs(x = "Moral Injury Symptoms (MIOS)", 
       y = "Predicted\nDysfunction (bIPF)",
       subtitle = "Gamma part of the model (dpar = \"mu\")") +
  theme_marginal_fx


# Zero-Process Logistic Model
plot_conditional_effects_3_hu <- 
  plot(conditional_effects(model_3_hurdle, dpar = "hu"), plot = FALSE)[[1]]$data %>% 
  mutate(mios_total = effect1__ * sd(data_3$mios_total) + mean(data_3$mios_total)) %>% 
  rename(bifp_pred = estimate__) %>% 
  ggplot(aes(x = mios_total, y = bifp_pred)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
  geom_line(size = 1, color = clrs[5]) +
  #scale_x_continuous(limits = c(0,80), breaks = seq(0, 80, 20)) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "Moral Injury Symptoms (MIOS)", 
       y = "Predicted probability\nof 0 dysfunction",
       subtitle = "Hurdle part of the model (dpar = \"hu\")") +
  theme_marginal_fx


# Combine the plots
(plot_conditional_effects_3 / plot_conditional_effects_3_mu / plot_conditional_effects_3_hu) +
  plot_annotation(title = "Conditional effects of moral injury symptoms on dysfunction",
                  subtitle = "Conditional effects at the mean of all covariates",
                  theme = theme(plot.title = element_text(face = "bold"),
                                plot.subtitle = element_text()))


# Save the combined plots
ggsave(here::here("output/plot-conditional-effects-3.jpg"), width = 6, height = 8)

