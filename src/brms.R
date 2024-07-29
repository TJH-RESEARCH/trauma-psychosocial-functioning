
library(brms)
library(modelr)
library(patchwork)
library(gganimate)
library(gifski)




# MAKE PREDICTIONS --------------------------------------------------------
tibble(mios_scaled = c(-.5, 0, .5),
       pc_ptsd_positive_screen_scaled = c(0,0,0), 
       service_era_post_911_scaled  = c(0,0,0), 
       service_era_persian_gulf_scaled  = c(0,0,0), 
       sex_female_scaled  = c(0,0,0), 
       race_black_scaled  = c(0,0,0), 
       race_white_scaled = c(0,0,0)
       ) %>% 
  add_epred_draws(model_multi) %>%
  median_qi(.epred) %>% 
  select(1, 8, 9, 10, 11, 12, 13, 14, 15)


################################################################
################################################################
################################################################
################################################################

data_plot = data_scaled %>%
  ggplot(aes(x = mios_scaled, y = bipf_category, color = bipf_category)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2", name = "bipf_category")



fit_plot = data_scaled %>%
  data_grid(mios_scaled = seq_range(mios_scaled, n = 101)) %>%
  add_epred_draws(model_bi, value = "P(bipf_category | mios_scaled)", category = "bipf_category") %>%
  ggplot(aes(x = mios_scaled, y = `P(bipf_category | mios_scaled)`, color = bipf_category)) +
  stat_lineribbon(aes(fill = bipf_category), alpha = 1/5) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")


data_plot /
fit_plot




# -------------------------------------------------------------------------
