
# Average Marginal Effect
## reports the median of the posterior distribution as their main estimates.
options(marginaleffects_posterior_interval = "hdi")
options(marginaleffects_posterior_center = median)

## Non-zero process, in log units:
model_1_hurdle |> avg_slopes(dpar = 'mu', type = "link", variables = 'pcl_total')

## Non-zero process, in response units:
model_1_hurdle |> avg_slopes(dpar = 'mu', type = "response", variables = 'pcl_total')

## Non-zero process, all parameters:
model_1_hurdle %>% marginaleffects::avg_slopes(dpar = 'mu') |> tibble()

## Zero process (probability of a zero outcome)
model_1_hurdle |> avg_slopes(dpar = "hu", type = 'response', variables = 'pcl_total')
model_1_hurdle |> avg_slopes(dpar = "hu", type = "link", variables = 'pcl_total')

## Zero process, all parameters:
model_1_hurdle %>% marginaleffects::avg_slopes(dpar = 'hu') |> tibble()




## Plot Average Marginal Effects


data_1 %>% summarise(mean_pcl = mean(pcl_total), sd = sd(pcl_total))
pcl_sd_minus_1 <- mean(data_1$pcl_total) - sd(data_1$pcl_total)
pcl_mean <- mean(data_1$pcl_total)
pcl_sd_plus_1 <- mean(data_1$pcl_total) + sd(data_1$pcl_total)
pcl_sd_plus_2 <- mean(data_1$pcl_total) + 2 * sd(data_1$pcl_total)

### Create the data set for use with ggplot
data_plot_ame_1 <-
  plot_slopes(
    model_1_hurdle,
    variables = "pcl_total",
    condition = "pcl_total",
    draw = FALSE # to get the data set
    ) %>% 
  tibble()


### Plot
plot_ame_1 <-
  data_plot_ame_1 %>% 
  mutate(estimate = estimate / sd(data_1$pcl_total),
         conf.low = conf.low / sd(data_1$pcl_total),
         conf.high = conf.high / sd(data_1$pcl_total),
         pcl_total = pcl_total * sd(data_1$pcl_total) + mean(data_1$pcl_total)) %>% 
  ggplot(aes(pcl_total, estimate)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              color = MetPalettes$Peru1[[1]][3],
              linewidth = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, color = NA) +
  #geom_point(aes(pcl_sd, ame_sd), size = 4, color = MetPalettes$Peru1[[1]][3]) +
  #geom_text(aes(pcl_sd, ame_sd, label = round(ame_sd, 1)), nudge_y = 2, color = MetPalettes$Peru1[[1]][3], fontface = "bold") +
  lims(y = c(0, 1.5)) +
  labs(
    x = 'Posttraumatic Symptoms (SD)',
    y = 'Change in Dysfunction',
    #title = "PTSD symptoms associated with difficulty functioning",
    subtitle = "<span style = 'color:#1c9d7c'> **Average marginal effects**</span> for non-zero gamma model (Survey 1)",
    # caption = "Average marginal effects represent the change in the outcome Functioning for a marginal change<br>in the predictor PTSD Symptoms at different levels of the predictor. The value of the average<br>marginal effect is shown at the mean level of PTSD Sypmtoms and at -1SD, +1SD, and +1.9SD.<br>The data are from Study 1. They represent only the responses with values of Functioning<br>greater than zero."
    ) + 
  theme_marginal_fx
plot_ame_1
ggsave(here::here("output/plot-ame-1.jpg"), width = 6, height = 4)




# Zero process ------------------------------------------------------------
data_plot_ame_1_zero <-
  plot_slopes(
      model_1_hurdle,
      dpar = "hu",
      variables = "pcl_total",
      condition = "pcl_total",
      type = 'response',
      draw = FALSE
      ) %>% 
  tibble()

plot_ame_1_zero <-
  data_plot_ame_1_zero %>% 
  mutate(
    # convert to original units
    estimate = estimate / sd(data_1$pcl_total),          # slope per 1 original unit
    conf.low = conf.low / sd(data_1$pcl_total),
    conf.high = conf.high / sd(data_1$pcl_total),
    # convert PCL to original units
    pcl_total = pcl_total * sd(data_1$pcl_total) + mean(data_1$pcl_total)
    ) %>% 
  ggplot(aes(pcl_total, estimate)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              color = MetPalettes$Peru1[[1]][2],
              linewidth = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, color = NA) +
  scale_y_continuous(limits = c(-.016, 0), labels = scales::label_percent()) + 
  #geom_point(aes(pcl_sd, ame_sd), size = 4, color = MetPalettes$Peru1[[1]][2]) +
  #geom_text(aes(pcl_sd, ame_sd, label = round(ame_sd, 2)), nudge_x = -.03, nudge_y = .04, color = MetPalettes$Peru1[[1]][2], fontface = "bold") +
  #lims(y = c(.45, .5)) +
  labs(
    x = 'Posttraumatic Symptoms',
    y = "Change in Probability\nof No Dysfunction", 
    #title = 'Greater symptoms are more impactful on dysfunction at lower levels',
    subtitle = "<span style = 'color:#e35e28'> **Average marginal effects**</span> for zero-process logistic model (Survey 1)",
    # caption = "At the lowest end of PCL scores, a marginal increase in Posttraumatic Symptoms reduces the log-odds<br>of having no difficulty by 0.26."
    ) + 
  theme_marginal_fx
plot_ame_1_zero

ggsave(here::here("output/plot-ame-1-zero.jpg"), width = 6, height = 4)


# The average marginal effects being around -.2 indicates that an increase in PCL at those levels are associated with about a decrease in the log-odds of being a zero. That is more PCL (PTSD symptoms) leads to less chance of having no difficulty functioning, by a log-odds of -.2. But as PTSD symptoms increases, this marginal effect tapers off. So that at higher levels, the impact of a change is less extreme. In other words, if having mild-to-moderate symptoms doesn't lead to some difficulty, then having extreme symptoms wont either. But the AME is never zero, it only approaches 0, so increases in PTSD symptoms still increase the chance of having some difficulty, only this increased chance is less at higher values of PCL. 


# Marginal Effects at the mean
model_1_hurdle %>% marginaleffects::avg_slopes(newdata = 'mean')




# Combine -----------------------------------------------------------------

plot_ame_1 / plot_ame_1_zero 
ggsave(here::here("output/plot-ame-1-combo.jpg"), width = 6, height = 6)


