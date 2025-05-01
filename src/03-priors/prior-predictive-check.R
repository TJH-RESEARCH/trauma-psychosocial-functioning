
# Set global Stan options
CHAINS <- 4
ITER <- 2000
WARMUP <- 1000
SEED <- 999888777


# Hurdle gamma model: Priors  --------------------------------------------------
model_hurdle_prior <- 
  brm(
    bf(bipf_score ~ PCL_TOT + deployed + gender_female + race_black + race_white),
    data = data_baked_1,
    family = hurdle_gamma(),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 0, 1.75), class = Intercept),
      prior(beta(1, 1.25), class = hu, lb = 0, ub = 1),
      prior(gamma(2, 1), class = shape, lb = 0) 
    ),
    sample_prior = 'only',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )
pairs(model_hurdle_prior)
model_hurdle_prior$prior
model_hurdle_prior$model

# Prior predictive check for regression lines
model_hurdle_prior %>% 
  tidybayes::gather_draws(c(Intercept, b_PCL_TOT)) %>% 
  pivot_wider(names_from = .variable, values_from = .value) %>% 
  slice_sample(n = 200) %>% 
  ggplot(aes(Intercept, b_PCL_TOT)) +
  geom_point(alpha = 0) +
  geom_abline(aes(intercept = Intercept, slope = b_PCL_TOT), alpha = .2) #+
#lims(x = c(-10, 10))

# Prior predictive check for regression lines
model_hurdle_prior %>% 
  tidybayes::gather_draws(c(Intercept, b_deployed)) %>% 
  pivot_wider(names_from = .variable, values_from = .value) %>% 
  slice_sample(n = 200) %>% 
  ggplot(aes(Intercept, b_deployed)) +
  geom_point(alpha = 0) +
  geom_abline(aes(intercept = Intercept, slope = b_deployed), alpha = .2) #+
#lims(x = c(-10, 10))

