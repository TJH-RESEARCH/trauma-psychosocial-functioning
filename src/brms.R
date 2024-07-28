
library(brms)

m_intercept<- brm(bipf_category ~ 
                    1,
                  data = data_scaled,
                  family = cumulative(threshold = "flexible"),
                  cores = 4)

summary(m_intercept)
get_prior(m_intercept)

################################################################
################################################################
################################################################
################################################################

# BIVARIATE - PRIOR

model_bi_prior <- 
  brm(bipf_category ~ 
        mios_scaled,
          data = data_scaled,
          prior(cauchy(0, 2.5), class = 'b'),
          family = cumulative(threshold = "flexible"),
          sample_prior = "only",
          cores = 4)

brms::get_prior(model_bi_prior)
pp_check(model_bi_prior, type = 'bars')
#summary(model_bi_prior)





################################################################
################################################################
################################################################
################################################################

# BIVARIATE

model_bi <- 
  brm(bipf_category ~ 
        mios_scaled,
      data = data_scaled,
      prior(cauchy(0, 2.5), class = 'b'),
      family = cumulative(threshold = "flexible"),
      cores = 4)

brms::get_prior(model_bi)
pp_check(model_bi, type = 'bars')
#summary(model_bi)





################################################################
################################################################
################################################################
################################################################

# MULTIVARIATE - PRIOR
model_multi_prior <- brm(bipf_category ~ 
                     mios_scaled +
                     pc_ptsd_positive_screen_scaled + 
                     service_era_post_911_scaled + 
                     service_era_persian_gulf_scaled + 
                     sex_female_scaled +
                     race_black_scaled + 
                     race_white_scaled,
                   data = data_scaled,
                   prior(cauchy(0, 2.5), class = 'b'),
                   family = cumulative(threshold = "flexible"),
                   sample_prior = "only",
                   cores = 4)

# cauchy(0, 1) not good
# cauchy(0, 2.5) not as good as # cauchy(0, 2.5)

brms::get_prior(model_multi_prior)
pp_check(model_multi_prior, type = 'bars')
pp_check(model_multi_prior, type = 'dens')
pp_check(model_multi_prior, type = 'hist')
#summary(model_multi_prior)



################################################################
################################################################
################################################################
################################################################

# MULTIVARIATE - PRIOR
model_multi <- brm(bipf_category ~ 
                     mios_scaled +
                     pc_ptsd_positive_screen_scaled + 
                     service_era_post_911_scaled + 
                     service_era_persian_gulf_scaled + 
                     sex_female_scaled +
                     race_black_scaled + 
                     race_white_scaled,
                   data = data_scaled,
                   prior(cauchy(0, 2.5), class = 'b'),
                   family = cumulative(threshold = "flexible"),
                   cores = 4)

brms::get_prior(model_multi)
pp_check(model_multi, type = 'bars')
pp_check(model_multi, type = 'dens')
pp_check(model_multi, type = 'hist')

summary(model_multi)

get_variables(model_multi)
brms::stancode(model_multi)

draws_multi <-
  model_multi %>% 
  spread_draws(
      b_Intercept[1:4], 
      b_mios_scaled, 
      b_pc_ptsd_positive_screen_scaled, 
      b_service_era_post_911_scaled, 
      b_service_era_persian_gulf_scaled,
      b_sex_female_scaled, 
      b_race_black_scaled,
      b_race_white_scaled) %>% 
  pivot_wider(values_from = b_Intercept, names_from = `1:4`, names_prefix = 'intercept_')

draws_multi %>% tidybayes::summarise_draws()



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


library(modelr)
fit_plot = data_scaled %>%
  data_grid(mios_scaled = seq_range(mios_scaled, n = 101)) %>%
  add_epred_draws(model_bi, value = "P(bipf_category | mios_scaled)", category = "bipf_category") %>%
  ggplot(aes(x = mios_scaled, y = `P(bipf_category | mios_scaled)`, color = bipf_category)) +
  stat_lineribbon(aes(fill = bipf_category), alpha = 1/5) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

library(patchwork)
data_plot /
fit_plot



# Hypothetical Outcomes Plot ----------------------------------------------
#http://mjskay.github.io/tidybayes/articles/tidy-brms.html

# NOTE: using a small number of draws to keep this example
# small, but in practice you probably want 50 or 100
ndraws = 50

p = data_scaled %>%
  data_grid(mios_scaled = seq_range(mios_scaled, n = 101)) %>%
  add_epred_draws(model_bi, value = "P(bipf_category | mios_scaled)", category = "bipf_category") %>%
  ggplot(aes(x = mios_scaled, y = `P(bipf_category | mios_scaled)`, color = bipf_category)) +
  # we remove the `.draw` column from the data for stat_lineribbon so that the same ribbons
  # are drawn on every frame (since we use .draw to determine the transitions below)
  stat_lineribbon(aes(fill = bipf_category), alpha = 1/5, color = NA, data = . %>% select(-.draw)) +
  # we use sample_draws to subsample at the level of geom_line (rather than for the full dataset
  # as in previous HOPs examples) because we need the full set of draws for stat_lineribbon above
  geom_line(aes(group = paste(.draw, bipf_category)), linewidth = 1, data = . %>% sample_draws(ndraws)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  gganimate::transition_manual(.draw)

gganimate::animate(p, 
                   nframes = ndraws, 
                   fps = 2.5, 
                   width = 576, 
                   height = 192, 
                   res = 96, 
                   dev = "png", 
                   type = "cairo")


# -------------------------------------------------------------------------
#"We could take a slice through these lines at an x position in the above chart (say, mios_scale = 0) and look at the correlation between them using a scatterplot matrix:"
tibble(mios_scaled = 0) %>%
  add_epred_draws(model_bi, value = "P(bipf_category | mios_scaled)", category = "bipf_category") %>%
  ungroup() %>%
  select(.draw, bipf_category, `P(bipf_category | mios_scaled)`) %>%
  gather_pairs(bipf_category, `P(bipf_category | mios_scaled)`, triangle = "both") %>%
  filter(.row != .col) %>%
  ggplot(aes(.x, .y)) +
  geom_point(alpha = 1/50) +
  facet_grid(.row ~ .col) +
  ylab("P(bipf_category = row | mios_scaled = 0)") +
  xlab("P(bipf_category = col | mios_scaled = 0)")



# -------------------------------------------------------------------------
