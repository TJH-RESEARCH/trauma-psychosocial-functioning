library(MetBrewer)
library(marginaleffects)
library(posterior)
library(patchwork)
library(ggtext)
library(showtext)

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

### Create the data set for use with ggplot
data_plot_ame <-
  plot_slopes(
    model_1_hurdle,
    variables = "pcl_total",
    condition = c("pcl_total", "veteran", "civilian"),
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

### Font
font_add_google("Arimo", "arimo")
font_add_google("Oswald", "oswald")
font_add_google("Acme", "acme")

# Automatically use {showtext} for plots
showtext_auto(enable = FALSE)

### Plot
data_plot_ame %>% 
  ggplot(aes(pcl_total, estimate)) +
  geom_smooth(method = "loess", 
              formula = "y ~ x",
              color = MetPalettes$Peru1[[1]][3],
              linewidth = 2) +
  geom_point(aes(pcl_sd, ame_sd), size = 4, color = MetPalettes$Peru1[[1]][3]) +
  geom_text(aes(pcl_sd, ame_sd, label = round(ame_sd, 1)), nudge_y = 2, color = MetPalettes$Peru1[[1]][3], fontface = "bold") +
  #facet_grid(vars(civilian)) +
  lims(y = c(10, 30)) +
  labs(
    x = 'Posttraumatic Symptoms (SD)',
    y = 'Average Marginal Effects',
    title = "PTSD symptoms associated with difficulty functioning",
    subtitle = "<span style = 'color:#1c9d7c'> **Average marginal effects**</span> for the non-zero model (Study 1)",
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
  
ggsave(here::here("output/plot-ame.jpg"), width = 6, height = 4)

  
  plot_slopes(
    model_1_hurdle,
    dpar = "hu",
    variables = "pcl_total",
    condition = "pcl_total",
    rug = TRUE,
    type = 'response'
    ) +
  labs(
    x = 'PCL',
    y = "Log Odds", 
    title = 'Average Marginal Effects',
    subtitle = 'Zero Process',
    caption = "At the lowest end of PCL scores, a 1-SD increase in \n PCL reduces the log-odds of having no difficulty by 0.275."
    ) +
  theme_custom
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



