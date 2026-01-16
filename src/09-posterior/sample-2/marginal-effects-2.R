
# Average Marginal Effect
## reports the median of the posterior distribution as their main estimates.
options(marginaleffects_posterior_interval = "hdi")
options(marginaleffects_posterior_center = median)

## Non-zero process, in log units:
model_2_hurdle |> avg_slopes(dpar = 'mu', type = "link", variables = 'mios_total')

## Non-zero process, in response units:
model_2_hurdle |> avg_slopes(dpar = 'mu', type = "response", variables = 'mios_total')

## Non-zero process, all parameters:
model_2_hurdle %>% marginaleffects::avg_slopes(dpar = 'mu') |> tibble()

## Zero process (probability of a zero outcome)
model_2_hurdle |> avg_slopes(dpar = "hu", type = 'response', variables = 'mios_total')
model_2_hurdle |> avg_slopes(dpar = "hu", type = "link", variables = 'mios_total')

## Zero process, all parameters:
model_2_hurdle %>% marginaleffects::avg_slopes(dpar = 'hu') |> tibble()

# Marginal Effects at the mean
model_2_hurdle %>% marginaleffects::avg_slopes(newdata = 'mean')





## Plot Average Marginal Effects



mios_sd_minus_2 <- mean(data_2$mios_total) - sd(data_2$mios_total)
mios_mean <- mean(data_2$mios_total)
mios_sd_plus_2 <- mean(data_2$mios_total) + sd(data_2$mios_total)
mios_sd_plus_2 <- mean(data_2$mios_total) + 2 * sd(data_2$mios_total)

### Create the data set for use with ggplot
data_plot_ame_2 <-
  plot_slopes(
    model_2_hurdle,
    variables = "mios_total",
    condition = "mios_total",
    draw = FALSE # to get the data set
    ) %>% 
  tibble()


### Plot
plot_ame_2 <-
  data_plot_ame_2 %>% 
  mutate(estimate = estimate / sd(data_2$mios_total),
         conf.low = conf.low / sd(data_2$mios_total),
         conf.high = conf.high / sd(data_2$mios_total),
         mios_total = mios_total * sd(data_2$mios_total) + mean(data_2$mios_total)) %>% 
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
  theme_custom
plot_ame_2
ggsave(here::here("output/plot-ame-2.jpg"), width = 6, height = 4)




# Zero process ------------------------------------------------------------
data_plot_ame_2_zero <-
  plot_slopes(
      model_2_hurdle,
      dpar = "hu",
      variables = "mios_total",
      condition = "mios_total",
      type = 'response',
      draw = FALSE
      ) %>% 
  tibble()

plot_ame_2_zero <-
  data_plot_ame_2_zero %>% 
  mutate(
    # convert to original units
    estimate = estimate / sd(data_2$mios_total),          # slope per 1 original unit
    conf.low = conf.low / sd(data_2$mios_total),
    conf.high = conf.high / sd(data_2$mios_total),
    # convert mios to original units
    mios_total = mios_total * sd(data_2$mios_total) + mean(data_2$mios_total)
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
  theme_custom

## Print to Console
plot_ame_2_zero %>% print()

## Save to file
ggsave(here::here("output/plot-ame-2-zero.jpg"), width = 6, height = 4)




# Combine -----------------------------------------------------------------

plot_ame_2 / plot_ame_2_zero 
ggsave(here::here("output/plot-ame-2-combo.jpg"), width = 6, height = 6)


