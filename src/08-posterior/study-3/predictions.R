

## Im not sure the predictions plot for the interaction is very intuitive

# Non-Zero Process Conditional Predictions --------------------------------


## Create the data
data_plot_preds_3 <-
  plot_predictions(
    model_3_hurdle_interact,
    condition = c("pcl_total", "mios_total"), 
    draw = FALSE) %>% 
  tibble() %>%
  mutate(pcl_sd = 
           case_when(
             rowid == 226 ~ 2,
             rowid == 165 ~ 1,
             rowid == 101 ~ 0,
             rowid == 40 ~ -1,
             .default = NA
           ),
         # subtracting .1 makes the dots center on the loess line better
         ame_sd =
           case_when(
             pcl_sd == 2 ~ estimate,
             pcl_sd == 1 ~ estimate,
             pcl_sd == 0 ~ estimate,
             pcl_sd == -1 ~ estimate,
             .default = NA
           )
  )


data_plot_preds_3 %>% count(pcl_total)
data_plot_preds_3 %>% count(mios_total)


data_plot_preds_3 %>% 
  mutate(mios_total = 
           case_when(
               mios_total == "-2.060" ~ "-2",
               #mios_total == "-0.801" ~ -.8,
               mios_total == " 0.099" ~ "0",
               #mios_total == " 0.728" ~ .7,
               mios_total == " 1.987" ~ "2",
             .default = NA
           )
           ) %>%
  filter(!is.na(mios_total)) %>% 
  #select(rowid, estimate, pcl_total_x_mios_total, pcl_total, mios_total) %>% 
  ggplot(aes(pcl_total, estimate, color = mios_total)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              linewidth = 2,
              se = FALSE) +
  MetBrewer::scale_color_met_d("Derain") +
  geom_text(label = "MI +2SD", x = 1, y = 90, color = "#454a74") +
  geom_text(label = "MI Mean", x = 2.25, y = 85, color = "#808fe1") +
  geom_text(label = "MI -2SD", x = 2, y = 50, color = "#6f9969") +
  #geom_point(aes(pcl_total_x_mios_total, ame_sd), size = 4, color = MetPalettes$Peru1[[1]][1]) +
  #geom_text(aes(pcl_total_x_mios_total, ame_sd, label = round(ame_sd, 1)), nudge_x = -0.025, nudge_y = 6.25, color = MetPalettes$Peru1[[1]][1], fontface = "bold") +
  lims(y = c(0, 100)) +
  labs(
    x = 'PTSD',
    y = "Difficulty Functioning", 
    title = 'Veterans with PTSD & Moral Injury had worse dysfunction',
    subtitle = "<span style = 'color:#454a74'> **Predicted difficulty conditional on PTSD and Moral Injury**</span> (Study 3)",
    caption = "Predicted values conditional on PTSD Symptoms, stratified by Moral Injury symptoms, from the<br>gamma regression model. Fit to the non-zero outcome cases only. Calculated using a representative<br>combination of covariates with marginaleffects::plot_predictions() in R."
  ) + 
  theme(
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "#3e3e3e"),
    axis.title = element_markdown(size = 10, face = "bold"),
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e")
  )

ggsave(here::here("output/plot-preds-3.jpg"), width = 6, height = 4)

MetBrewer::colorblind_palettes
MetBrewer::display_all()
MetBrewer::MetPalettes$Derain[[1]]
c("#efc86e", "#97c684", "#6f9969", "#aab5d5", "#808fe1", "#5c66a8", "#454a74")


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
    title = '**Veterans with PTSD have lower chance of functioning well**',
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