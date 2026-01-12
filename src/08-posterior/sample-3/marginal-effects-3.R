
# Average Marginal Effect
## reports the median of the posterior distribution as their main estimates.
options(marginaleffects_posterior_interval = "hdi")
options(marginaleffects_posterior_center = median)

## Non-zero process, in log units:
model_3_hurdle |> avg_slopes(dpar = 'mu', type = "link", variables = 'mios_total')

## Non-zero process, in response units:
model_3_hurdle |> avg_slopes(dpar = 'mu', type = "response", variables = 'mios_total')

## Non-zero process, all parameters:
model_3_hurdle %>% marginaleffects::avg_slopes(dpar = 'mu') |> tibble()

## Zero process (probability of a zero outcome)
model_3_hurdle |> avg_slopes(dpar = "hu", type = 'response', variables = 'mios_total')
model_3_hurdle |> avg_slopes(dpar = "hu", type = "link", variables = 'mios_total')

## Zero process, all parameters:
model_3_hurdle %>% marginaleffects::avg_slopes(dpar = 'hu') |> tibble()




## Plot Average Marginal Effects


data_3 %>% summarise(mean_mios = mean(mios_total), sd = sd(mios_total))
mios_sd_minus_3 <- mean(data_3$mios_total) - sd(data_3$mios_total)
mios_mean <- mean(data_3$mios_total)
mios_sd_plus_3 <- mean(data_3$mios_total) + sd(data_3$mios_total)
mios_sd_plus_3 <- mean(data_3$mios_total) + 2 * sd(data_3$mios_total)

### Create the data set for use with ggplot
data_plot_ame_3 <-
  plot_slopes(
    model_3_hurdle,
    variables = "mios_total",
    condition = "mios_total",
    draw = FALSE # to get the data set
    ) %>% 
  tibble()


### Plot
plot_ame_3 <-
  data_plot_ame_3 %>% 
  mutate(estimate = estimate / sd(data_3$mios_total),
         conf.low = conf.low / sd(data_3$mios_total),
         conf.high = conf.high / sd(data_3$mios_total),
         mios_total = mios_total * sd(data_3$mios_total) + mean(data_3$mios_total)) %>% 
  ggplot(aes(mios_total, estimate)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              color = MetPalettes$Peru1[[1]][3],
              linewidth = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, color = NA) +
  lims(y = c(0, 1.5)) +
  labs(
    x = 'Moral Injury Symptoms (SD)',
    y = 'Change in Dysfunction',
    subtitle = "<span style = 'color:#1c9d7c'> **Average marginal effects**</span> for non-zero gamma model (Survey 1)",
    ) + 
  theme_marginal_fx
plot_ame_3
ggsave(here::here("output/plot-ame-3.jpg"), width = 6, height = 4)




# Zero process ------------------------------------------------------------
data_plot_ame_3_zero <-
  plot_slopes(
      model_3_hurdle,
      dpar = "hu",
      variables = "mios_total",
      condition = "mios_total",
      type = 'response',
      draw = FALSE
      ) %>% 
  tibble()

plot_ame_3_zero <-
  data_plot_ame_3_zero %>% 
  mutate(
    # convert to original units
    estimate = estimate / sd(data_3$mios_total),          # slope per 1 original unit
    conf.low = conf.low / sd(data_3$mios_total),
    conf.high = conf.high / sd(data_3$mios_total),
    # convert mios to original units
    mios_total = mios_total * sd(data_3$mios_total) + mean(data_3$mios_total)
    ) %>% 
  ggplot(aes(mios_total, estimate)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              color = MetPalettes$Peru1[[1]][2],
              linewidth = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, color = NA) +
  scale_y_continuous(limits = c(-.016, 0), labels = scales::label_percent()) + 
  #lims(y = c(.45, .5)) +
  labs(
    x = 'Moral Injury Symptoms',
    y = "Change in Probability\nof No Dysfunction", 
    subtitle = "<span style = 'color:#e35e28'> **Average marginal effects**</span> for zero-process logistic model (Survey 1)",
    ) + 
  theme_marginal_fx
plot_ame_3_zero

ggsave(here::here("output/plot-ame-3-zero.jpg"), width = 6, height = 4)


# The average marginal effects being around -.2 indicates that an increase in mios at those levels are associated with about a decrease in the log-odds of being a zero. That is more mios (PTSD symptoms) leads to less chance of having no difficulty functioning, by a log-odds of -.2. But as PTSD symptoms increases, this marginal effect tapers off. So that at higher levels, the impact of a change is less extreme. In other words, if having mild-to-moderate symptoms doesn't lead to some difficulty, then having extreme symptoms wont either. But the AME is never zero, it only approaches 0, so increases in PTSD symptoms still increase the chance of having some difficulty, only this increased chance is less at higher values of mios. 


# Marginal Effects at the mean
model_3_hurdle %>% marginaleffects::avg_slopes(newdata = 'mean')




# Combine -----------------------------------------------------------------

plot_ame_3 / plot_ame_3_zero 
ggsave(here::here("output/plot-ame-3-combo.jpg"), width = 6, height = 6)


