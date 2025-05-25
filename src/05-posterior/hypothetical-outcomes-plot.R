
library(modelr)
library(patchwork)
library(gganimate)
library(gifski)
library(MetBrewer)

# Hypothetical Outcomes Plot ----------------------------------------------
#http://mjskay.github.io/tidybayes/articles/tidy-brms.html

# NOTE: using a small number of draws to keep this example
# small, but in practice you probably want 50 or 100
ndraws <- 50


#fit_plot <-
  data_baked_1 %>%   
  data_grid(data_baked_1, .model = model_1_hurdle) %>% 
  add_epred_draws(model_1_hurdle, 
                  value = "P(bipf_score | pcl_total)") %>%
  ggplot(aes(x = pcl_total, 
             y = `P(bipf_score | pcl_total)`)) +
  stat_lineribbon(aes(y = `P(bipf_score | pcl_total)`), 
                  .width = c(.99, .95, .8, .5), 
                  color = met.brewer(name = 'Austria', n = 1)) +
    theme_custom

# Hypothetical Outcomes Plot ----------------------------------------------
  data_baked_1 %>%   
    data_grid(data_baked_1, .model = model_1_hurdle, n = 51) %>% 
    add_epred_draws(model_1_hurdle) %>%
    ggplot(aes(x = pcl_total, 
               y = .epred)) +
    stat_lineribbon(aes(y = .epred), 
                    .width = c(.99, .95, .8, .5), 
                    alpha = .8, 
                    color = met.brewer(name = 'VanGogh3', n = 1)) +
    labs(x = 'PCL',
         y = 'bIPF',
         title = 'Posterior Predictions') +
    scale_fill_met_d(name = 'VanGogh3') +
    theme_custom
  
  
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

