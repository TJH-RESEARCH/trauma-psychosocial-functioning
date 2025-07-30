library(MetBrewer)
library(marginaleffects)
library(posterior)
library(patchwork)
library(ggtext)

# Average Marginal Effect
## reports the median of the posterior distribution as their main estimates.
options(marginaleffects_posterior_interval = "hdi")
options(marginaleffects_posterior_center = median)

## Non-zero process, in log units:
model_3_hurdle_interact %>% avg_slopes(dpar = 'mu', type = "response") %>% tibble()

## Zero process (probability of a zero outcome)
model_3_hurdle_interact %>% avg_slopes(dpar = "hu", type = 'response', variables = 'pcl_total')
model_3_hurdle_interact %>% avg_slopes(dpar = "hu", type = "link", variables = 'pcl_total')

## Zero process, all parameters:
model_3_hurdle_interact %>% marginaleffects::avg_slopes(dpar = 'hu') %>% tibble()




## Plot Average Marginal Effects

### Create the data set for use with ggplot
data_plot_ame_3_interact <-
  plot_slopes(
    model_3_hurdle_interact,
    variables = "pcl_total_x_mios_total",
    condition = c("pcl_total_x_mios_total"),
    draw = FALSE # to get the data set
    ) %>% 
  tibble() %>% 
  mutate(pcl_sd = 
           case_when(
             rowid == 200 ~ 1.9,
             rowid == 140 ~ 1,
             rowid == 75 ~ 0,
             rowid == 9 ~ -1,
             .default = NA
               ),
         # subtracting .1 makes the dots center on the loess line better
         ame_sd =
           case_when(
             pcl_sd == 1.9 ~ estimate - .15,
             pcl_sd == 1 ~ estimate - .15,
             pcl_sd == 0 ~ estimate - .15,
             pcl_sd == -1 ~ estimate - .15,
             .default = NA
           )
         )


### Plot
data_plot_ame_3_interact %>% 
  ggplot(aes(pcl_total_x_mios_total, estimate)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              color = MetPalettes$Austria[[1]][1],
              linewidth = 2) +
  #lims(y = c(10, 30)) +
  labs(
    x = 'Interaction',
    y = 'Average Marginal Effects',
    title = "Interaction associated with less difficulty",
    subtitle = "<span style = 'color:#a40000'> **Average marginal effects**</span> for the non-zero gamma model (Study 1)",
    caption = "Average marginal effects represent the change in the outcome Functioning for a marginal change<br>in the predictor PTSD Symptoms at different levels of the predictor. The value of the average<br>marginal effect is shown at the mean level of PTSD Sypmtoms and at -1SD, +1SD, and +1.9SD.<br>The data are from Study 1. They represent only the responses with values of Functioning<br>greater than zero."
    ) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "#3e3e3e"),
    axis.title = element_markdown(size = 10, face = "bold"),
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e")
  )
  
ggsave(here::here("output/plot-ame-3-interaction.jpg"), width = 6, height = 4)



# Zero process ------------------------------------------------------------
data_plot_ame_3_zero <-
  plot_slopes(
    model_3_hurdle_interact,
    variables = "pcl_total_x_mios_total",
    condition = c("pcl_total_x_mios_total"),
    draw = FALSE # to get the data set
  ) %>% 
  tibble() %>% 
  mutate(pcl_sd = 
           case_when(
             rowid == 50 ~ 1.9,
             rowid == 39 ~ 1,
             rowid == 19 ~ 0,
             rowid == 3 ~ -1,
             .default = NA
           ),
         # subtracting .1 makes the dots center on the loess line better
         ame_sd =
           case_when(
             pcl_sd == 1.9 ~ estimate,
             pcl_sd == 1 ~ estimate  - .005,
             pcl_sd == 0 ~ estimate,
             pcl_sd == -1 ~ estimate - .005,
             .default = NA
           )
  )


data_plot_ame_3_zero %>% 
  ggplot(aes(pcl_total, estimate)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              color = MetPalettes$Peru1[[1]][2],
              linewidth = 2) +
  geom_point(aes(pcl_sd, ame_sd), size = 4, color = MetPalettes$Peru1[[1]][2]) +
  geom_text(aes(pcl_sd, ame_sd, label = round(ame_sd, 2)), nudge_x = -.03, nudge_y = .04, color = MetPalettes$Peru1[[1]][2], fontface = "bold") +
  lims(y = c(-.3, .05)) +
  labs(
    x = 'Posttraumatic Symptoms',
    y = "Log Odds of Zero Difficulty", 
    title = 'PTSD associated with greater odds of difficulty functioning',
    subtitle = "<span style = 'color:#e35e28'> **Average marginal effects**</span> for the zero-process hurdle model (Study 1)",
    caption = "At the lowest end of PCL scores, a marginal increase in Posttraumatic Symptoms reduces the log-odds<br>of having no difficulty by 0.26."
    ) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "#3e3e3e"),
    axis.title = element_markdown(size = 10, face = "bold"),
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e")
  )

ggsave(here::here("output/plot-ame-1-zero.jpg"), width = 6, height = 4)


# The average marginal effects being around -.2 indicates that an increase in PCL at those levels are associated with about a decrease in the log-odds of being a zero. That is more PCL (PTSD symptoms) leads to less chance of having no difficulty functioning, by a log-odds of -.2. But as PTSD symptoms increases, this marginal effect tapers off. So that at higher levels, the impact of a change is less extreme. In other words, if having mild-to-moderate symptoms doesn't lead to some difficulty, then having extreme symptoms wont either. But the AME is never zero, it only approaches 0, so increases in PTSD symptoms still increase the chance of having some difficulty, only this increased chance is less at higher values of PCL. 


# Marginal Effects at the mean
model_1_hurdle %>% marginaleffects::avg_slopes(newdata = 'mean')







# Draws of average predictions
draws_avg_reds <- 
  model_1_hurdle %>% 
  marginaleffects::avg_predictions() %>% 
  get_draws()

draws_avg_reds %>% ggplot(aes(draw)) + geom_density()




# Plot adjusted predictions as a function of PCL
plot_predictions(
  model_1_hurdle,
  condition = "pcl_total") +
  labs(y = "mu") +
plot_predictions(
    model_1_hurdle,
    dpar = "hu",
    condition = "pcl_total") +
  labs(y = "hu")
### The prediction plot for "hu" is basically the same as the log odds plot, just inverted, and a little more intuitive. Actually both are a little more intuitive. 

data_plot_preds <-
  plot_predictions(
  model_1_hurdle,
  condition = "pcl_total", 
  draw = FALSE) %>% 
  tibble()

data_plot_preds_zero <-
  plot_predictions(
    model_1_hurdle,
    dpar = "hu",
    condition = "pcl_total", 
    draw = FALSE) %>% 
  tibble()%>% 
  mutate(pcl_sd = 
           case_when(
             rowid == 50 ~ 1.9,
             rowid == 35 ~ 1,
             rowid == 19 ~ 0,
             rowid == 3 ~ -1,
             .default = NA
           ),
         # subtracting .1 makes the dots center on the loess line better
         ame_sd =
           case_when(
             pcl_sd == 1.9 ~ estimate,
             pcl_sd == 1 ~ estimate,
             pcl_sd == 0 ~ estimate,
             pcl_sd == -1 ~ estimate + .005,
             .default = NA
           )
  )




data_plot_preds_zero %>% 
  ggplot(aes(pcl_total, estimate)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              color = MetPalettes$Peru1[[1]][2],
              linewidth = 2) +
  geom_point(aes(pcl_sd, ame_sd), size = 4, color = MetPalettes$Peru1[[1]][2]) +
  geom_text(aes(pcl_sd, ame_sd, label = round(ame_sd, 2)), nudge_x = 0, nudge_y = .04, color = MetPalettes$Peru1[[1]][2], fontface = "bold") +
  lims(y = c(0, .3)) +
  labs(
    x = 'Posttraumatic Symptoms (SD)',
    y = "Log Odds of Zero Difficulty", 
    title = 'PTSD associated with greater odds of difficulty functioning',
    subtitle = "<span style = 'color:#e35e28'> **Predictions**</span> for the zero-process hurdle model (Study 1)",
    caption = "At the lowest end of PCL scores, a marginal increase in Posttraumatic Symptoms reduces the log-odds<br>of having no difficulty by 0.26."
  ) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "#3e3e3e"),
    axis.title = element_markdown(size = 10, face = "bold"),
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e")
  )

ggsave(here::here("output/plot-preds-1-zero.jpg"), width = 6, height = 4)

