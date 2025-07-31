
library(marginaleffects)
library(ggtext)

# Non-Zero Process Conditional Predictions --------------------------------


## Create the new data on which to make preductions
## This data is based on the observed data. There are several ways to do this. 
## The covariates in this case are representative, i think
## The condition variable is a range from the observed min to observed max, with values evenly spaced. It is not the observed data, but it is in the observed ranged.. 

data_plot_preds <-
  plot_predictions(
    model_1_hurdle,
    condition = "pcl_total", 
    draw = FALSE) %>% 
  tibble() %>% 
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
             pcl_sd == 1 ~ estimate  - .005,
             pcl_sd == 0 ~ estimate,
             pcl_sd == -1 ~ estimate - .05,
             .default = NA
           )
  )


data_plot_preds %>% 
  ggplot(aes(pcl_total, estimate)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              color = MetPalettes$Peru1[[1]][1],
              linewidth = 2) +
  geom_point(aes(pcl_sd, ame_sd), size = 4, color = MetPalettes$Peru1[[1]][1]) +
  geom_text(aes(pcl_sd, ame_sd, label = round(ame_sd, 1)), nudge_x = -0.025, nudge_y = 6.25, color = MetPalettes$Peru1[[1]][1], fontface = "bold") +
  lims(y = c(0, 100)) +
  labs(
    x = 'Posttraumatic Symptoms (SD)',
    y = "Difficulty Functioning", 
    title = 'Veterans with worse PTSD had more difficulty functioning',
    subtitle = "<span style = 'color:#b5361c'> **Predicted level of difficulty conditional on PTSD symptoms**</span> (Study 1)",
    caption = 
    'Model predictions or "fitted values." Predictions were caluculated for a different values of PTSD<br>
     Symptoms ranging from the observed minimum to the observed maximum. Averaged over a <br> 
     representative grid of covariates. Covariates were gender, age, prior trauma, and military status.<br>
     Study 1 included active duty service members, veteran, and non-military civilians. <br>
     Gamma Regression fit to the non-zero outcome cases only. N = 222.'
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

ggsave(here::here("output/plot-preds-1.jpg"), width = 6, height = 4)






# Zero Process ------------------------------------------------------------


data_plot_preds_zero <-
  plot_predictions(
    model_1_hurdle,
    dpar = "hu",
    condition = "pcl_total", 
    draw = FALSE) %>% 
  tibble() %>% 
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
             pcl_sd == 1.9 ~ (1 - estimate),
             pcl_sd == 1 ~ (1 - estimate), # reverse the probability up here too
             pcl_sd == 0 ~ (1 - estimate),
             pcl_sd == -1 ~ (1 - estimate - .005), # to make the plot not give a prob = 1 after rounding and instead .99
             .default = NA
           ),
         ame_sd_label = 
           str_replace(as.character(round(ame_sd, 2)), 
                       pattern = "0.", replacement = " ."),
         ame_sd_label = ifelse(pcl_sd == 1.9, ".99", ame_sd_label)
  )




data_plot_preds_zero %>% 
  mutate(estimate = 1 - estimate) %>% # flip the probability to be the chance of having >0 difficulty
  ggplot(aes(pcl_total, estimate)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              color = MetPalettes$Peru1[[1]][2],
              linewidth = 2) +
  geom_point(aes(pcl_sd, ame_sd), size = 4, color = MetPalettes$Peru1[[1]][2]) +
  geom_text(aes(pcl_sd, ame_sd, label = ame_sd_label), nudge_x = 0.1, nudge_y = -0.015, color = MetPalettes$Peru1[[1]][2], fontface = "bold") +
  lims(y = c(.7, 1)) +
  labs(
    x = 'Posttraumatic Symptoms (SD)',
    y = "Probability", 
    title = '**Veterans with PTSD had a lower chance of functioning well**',
    subtitle = "<span style = 'color:#e35e28'> **Predicted probability of difficulty conditional on PTSD**</span> (Study 1)",
    caption = "Predicted values conditional on PTSD Symptoms from the logistic regression model.<br>Fit to the zero outcome cases only. Calculated using a representative combination of<br>covariates using marginaleffects::plot_predictions() in R. Values are shown at<br>the mean (0) and -1SD, +1SD, and +1.9SD"
  ) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "#3e3e3e"),
    axis.title = element_markdown(size = 10, face = "bold"),
    plot.title = element_markdown(size = 14),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e")
  )

ggsave(here::here("output/plot-preds-1-zero.jpg"), width = 6, height = 4)



# different way of coding it?

predictions(model_1_hurdle, dpar = 'hu') %>% 
  ggplot(aes(estimate)) +
  geom_histogram()