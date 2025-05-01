
library(brms)
library(modelr)
library(patchwork)
library(gganimate)
library(gifski)

# Hypothetical Outcomes Plot ----------------------------------------------
#http://mjskay.github.io/tidybayes/articles/tidy-brms.html

# NOTE: using a small number of draws to keep this example
# small, but in practice you probably want 50 or 100
ndraws = 50


fit_plot <-
  data_scaled %>%
  data_grid(mios_scaled = seq_range(mios_scaled, n = 101)) %>%
  add_epred_draws(model_ologit, 
                  value = "P(bipf_category | mios_total)", 
                  category = "bipf_category") %>%
  ggplot(aes(x = mios_scaled, 
             y = `P(bipf_category | mios_scaled)`, 
             color = bipf_category)) +
  stat_lineribbon(aes(fill = bipf_category), alpha = 1/5) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")
fit_plot


# Hypothetical Outcomes Plot ----------------------------------------------
p <-
  data_scaled %>%
  data_grid(mios_scaled = seq_range(mios_scaled, n = 101)) %>%
  add_epred_draws(model_ologit, 
                  value = "P(bipf_category | mios_scaled)", 
                  category = "bipf_category") %>%
  ggplot(aes(x = mios_scaled, 
             y = `P(bipf_category | mios_scaled)`, 
             color = bipf_category)) +
  # we remove the `.draw` column from the data for stat_lineribbon so that the same ribbons
  # are drawn on every frame (since we use .draw to determine the transitions below)
  stat_lineribbon(aes(fill = bipf_category), 
                  alpha = 1/5, 
                  color = NA, 
                  data = . %>% select(-.draw)
                  ) +
  # we use sample_draws to subsample at the level of geom_line (rather than for the full dataset
  # as in previous HOPs examples) because we need the full set of draws for stat_lineribbon above
  geom_line(aes(group = paste(.draw, bipf_category)), 
            linewidth = 1, 
            data = . %>% sample_draws(ndraws)
            ) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")


p + gganimate::transition_manual(.draw)

gganimate::animate(p, 
                   nframes = ndraws, 
                   fps = 2.5, 
                   width = 576, 
                   height = 192, 
                   res = 96, 
                   dev = "png", 
                   type = "cairo")

