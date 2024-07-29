library(tidyverse)
library(tidymodels)
library(bayesian)
library(rstan)
library(tidybayes)
library(bayesplot)
library(brms)
library(patchwork)
library(modelr)
library(gganimate)
library(gifski)
library(ggsci)


# === # === # === # === # === # === # === # === # === # === # === # === #
# DATA PREP ---------------------------------------------------------------
### Import ----
data <- readr::read_csv(here::here('data/data_clean.csv'))

## Prepare Outcome Data ----
source(here::here('src/02-pre-processing/score-bipf.R'))



# === # === # === # === # === # === # === # === # === # === # === # === #
# RECIPES ---------------------------------------------------------------------


# === # === # === # === # === # === # === # === # === # === # === # === #
### Multivariate Model -----
recipe_multi <-
  recipes::recipe(bipf_category ~ 
                  mios_total +
                  pc_ptsd_positive_screen + 
                  service_era_post_911 +
                  service_era_persian_gulf +
                  sex_male +
                  race_black +
                  race_white,
                data = data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(mios_total, factor = 2)

recipe_multi %>% prep(., data) 

data_baked_multi <- recipe_multi %>% prep(., data) %>% bake(., NULL)


# === # === # === # === # === # === # === # === # === # === # === # === #
### Normal Model -----

recipe_normal <-
  recipes::recipe(bipf_score ~ 
                    mios_total +
                    pc_ptsd_positive_screen + 
                    service_era_post_911 +
                    service_era_persian_gulf +
                    sex_male +
                    race_black +
                    race_white,
                  data = data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(mios_total, factor = 2)

recipe_normal %>% prep(., data) 

data_baked_normal <- recipe_normal %>% prep(., data) %>% bake(., NULL)


# === # === # === # === # === # === # === # === # === # === # === # === #
# SET PRIORS ---------------------------------------------------------------

### Plot Priors ----
plot_priors <- 
  tibble(
    beta_prior = rcauchy(n = 1e5, location = 0, scale = 2.5),
    alpha_prior = rt(n = 1e5, df = 3, ncp = 2.5),
    id = rep(1, 1e5)
  )  %>% 
  pivot_longer(-id) %>% 
  ggplot(aes(value)) +
  geom_density(aes(fill = name), alpha = .25) +
  lims(x = c(-15, 15))
plot_priors %>% print()

### Specify Prior ----
b_prior_weakly_informative <- prior(cauchy(0, 2.5), class = 'b')

b_prior_normal <- prior(normal(0, 2.5), class = 'b')



# === # === # === # === # === # === # === # === # === # === # === # === #
# FIT MODELS --------------------------------------------------------------

### Load Custom Parsnip ----
source(here::here('src/04-models/parsnip-clm.R'))

### Specify Models ----
model_ologit_flat <- 
  bayesian(
    family = cumulative(threshold = "flexible")) %>% 
    set_engine("brms") %>% 
    set_mode("classification")

model_ologit <- 
  bayesian(
    family = cumulative(threshold = "flexible")) %>% 
  set_engine("brms") %>% 
  set_mode("classification") %>% 
  set_args(prior = b_prior_weakly_informative)

model_ologit_prior <- 
  bayesian(
    family = cumulative(threshold = "flexible"),
    sample_prior = "only") %>% 
  set_engine("brms") %>% 
  set_mode("classification") %>% 
  set_args(prior = b_prior_weakly_informative)

model_normal <- 
  bayesian(
    family = gaussian) %>% 
  set_engine("brms") %>% 
  set_mode("regression") %>% 
  set_args(prior = b_prior_normal)

model_ologit_nonbayes <- 
  clm(mode = 'classification') %>% 
  set_engine(engine = 'clm')




# === # === # === # === # === # === # === # === # === # === # === # === #
# WORKFLOW SETS ---------------------------------------------------------------
models <-
  workflow_set(
    preproc = list(
      normal = recipe_normal,
      multivariate = recipe_multi,
      multivariate = recipe_multi,
      multivariate = recipe_multi,
      multivariate = recipe_multi
                   ),
    models = list(
      normal = model_normal,
      flat = model_ologit_flat,
      ologit = model_ologit,
      ologit_prior = model_ologit_prior,
      nonbayes = model_ologit_nonbayes
      
    ),
    cross = FALSE
  )

models %>% print(n = 50)



# === # === # === # === # === # === # === # === # === # === # === # === #
# FIT MODELS --------------------------------------------------------------
fit_normal <- 
  models %>% 
  extract_workflow('normal_normal') %>% 
  fit(data)

fit_multivariate_flat <- 
  models %>% 
  extract_workflow('multivariate_flat') %>% 
  fit(data)

fit_ologit_multi_prior <- 
  models %>% 
  extract_workflow('multivariate_ologit_prior') %>% 
  fit(data)

fit_ologit_multi <- 
  models %>% 
  extract_workflow('multivariate_ologit') %>% 
  fit(data)

fit_nonbayes <- 
  models %>% 
  extract_workflow('multivariate_nonbayes') %>% 
  fit(data)


# === # === # === # === # === # === # === # === # === # === # === # === #
# EXTRACT MODELS ----------------------------------------------------------

fit_normal             <- fit_normal             %>% parsnip::extract_fit_engine()
fit_multivariate_flat  <- fit_multivariate_flat  %>% parsnip::extract_fit_engine()
fit_ologit_multi_prior <- fit_ologit_multi_prior %>% parsnip::extract_fit_engine()
fit_ologit_multi       <- fit_ologit_multi       %>% parsnip::extract_fit_engine()
fit_nonbayes           <- fit_nonbayes           %>% parsnip::extract_fit_engine()



# === # === # === # === # === # === # === # === # === # === # === # === #
# COMPARE MODELS ----------------------------------------------------------

loo_compare(
  loo(fit_normal),
  loo(fit_multivariate_flat),
  loo(fit_ologit_multi)
)


# === # === # === # === # === # === # === # === # === # === # === # === #
# SPREAD DRAWS ------------------------------------------------------------

### Multivariate prior predictive model ----
draws_ologit_multi_prior <-
  fit_ologit_multi_prior %>% 
  recover_types(data_baked_multi) %>% 
  tidybayes::spread_draws(Intercept[1:4], 
                          b_mios_total,
                          b_pc_ptsd_positive_screen,
                          b_sex_male,
                          b_service_era_post_911,
                          b_service_era_persian_gulf,
                          b_race_black,
                          b_race_white) %>% 
  pivot_wider(names_from = `1:4`, values_from = Intercept, names_prefix = 'alpha_')

### Multivariate model ----
draws_ologit_multi <-
  fit_ologit_multi %>% 
  recover_types(data_baked_multi) %>% 
  tidybayes::spread_draws(Intercept[1:4], 
                          b_mios_total,
                          b_pc_ptsd_positive_screen,
                          b_sex_male,
                          b_service_era_post_911,
                          b_service_era_persian_gulf,
                          b_race_black,
                          b_race_white) %>% 
  pivot_wider(names_from = `1:4`, values_from = Intercept, names_prefix = 'alpha_')


# === # === # === # === # === # === # === # === # === # === # === # === #
# DIAGNOS MCMC ------------------------------------------------------------
parameters <- c('Intercept[1]', 
                'Intercept[2]',
                'Intercept[3]',
                'Intercept[4]',
                'b_mios_total', 
                'b_pc_ptsd_positive_screen', 
                'b_service_era_persian_gulf', 
                'b_service_era_post_911',
                'b_sex_male', 
                'b_race_black',
                'b_race_white')

### Multivariate ----
bayesplot::mcmc_rank_overlay(fit_ologit_multi, pars = parameters)
bayesplot::mcmc_trace(fit_ologit_multi, pars = parameters)



# === # === # === # === # === # === # === # === # === # === # === # === #
# PRIOR PREDICTIVE CHECK --------------------------------------------------
# "As with the standard concept of weakly informative priors, it is important that this prior predictive distribution for the data has at least some mass around extreme but plausible data sets. However, there should be no mass on completely implausible data sets (Gabry et al., 2019 p. 393)."

### Check priors ----
brms::get_prior(fit_ologit_multi_prior)
brms::get_prior(fit_ologit_multi)

### Plot Priors ----
plot_priors %>% print()


### Prior Predictive Checks ----
pp_check(fit_ologit_multi_prior, type = 'bars', ndraws = 50)
pp_check(fit_ologit_multi_prior, type = 'dens_overlay', ndraws = 50) + 
  labs(title = 'Prior Predictive Check',
       x = 'Response Categories',
       y = 'Density')
pp_check(fit_ologit_multi_prior, type = 'hist', ndraws = 11)



# === # === # === # === # === # === # === # === # === # === # === # === #
## PLOT POSTERIOR ----------------------------------------------------------
draws_ologit_multi %>%
  select(!contains('alpha')) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, y = name)) +
  tidybayes::stat_halfeye() +
  labs(x = 'Coefficient (Log Odds)',
       y = 'Predictor Variable')

draws_ologit_multi %>%
  select(!contains('b_')) %>% 
  pivot_longer(-c(.chain, .iteration, .draw)) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .2) +
  labs(x = 'Coefficient (Thresholds)',
       y = 'Density')

plot_area_logodds <- 
  draws_ologit_multi %>% 
  bayesplot::mcmc_areas(
    pars = c('b_mios_total'),
    prob = .95,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area') +
  labs(x = 'Log Odds')

plot_area_prob <- 
  draws_ologit_multi %>% 
  mutate(b_mios_p = plogis(b_mios_total)) %>% 
  bayesplot::mcmc_areas(
    pars = c('b_mios_p'),
    prob = .95,
    prob_outer = 1,
    point_est = 'mean',
    area_method = 'equal area') +
  labs(x = 'Probability')

plot_area_logodds + plot_area_prob + 
  patchwork::plot_annotation(
    title = 'Posterior Distribution',
    subtitle = 'Coefficient on Moral Injury Symptoms'
  )


# === # === # === # === # === # === # === # === # === # === # === # === #
# MAKE PREDICTIONS --------------------------------------------------------

#### Specify a model in order to sample from in with data_grid
model_temp <- 
  ordinal::clm(
    bipf_category ~ 
        mios_total + 
        pc_ptsd_positive_screen + 
        service_era_post_911 + 
        service_era_persian_gulf +
        sex_male + 
        race_black + 
      race_white, 
      data = data_baked_multi)


data_grid(data_baked_multi, 
          seq_range(mios_total, n =100),
          .model = model_temp) %>% 
  add_epred_draws(fit_ologit_multi) %>%
  median_qi(.epred) %>% 
  select(1, 8, 9, 10, 11, 12, 13, 14, 15)




# Hypothetical Outcomes Plot ----------------------------------------------
## Predicted probability of each category given the level of MIOS
# https://posit.co/resources/videos/visualizing-uncertainty-with-hypothetical-outcomes-plots/#:~:text=Such%20visualizations%20are%20called%20Hypothetical,between%20the%20different%20hypothetical%20outcomes.

plot_hypothetical_outcomes <-
  data_grid(data_baked_multi, 
            mios_total = seq_range(mios_total, n = 100),
            .model = model_temp) %>% 
  add_epred_draws(fit_ologit_multi, 
                  value = "P(bipf_category | mios_total)", 
                  category = "bipf_category") %>%
  ggplot(aes(x = mios_total, 
             y = `P(bipf_category | mios_total)`, 
             color = bipf_category)) +
  stat_lineribbon(aes(fill = bipf_category), alpha = 1/5) +
  scale_fill_cosmic() +
  scale_color_cosmic()
plot_hypothetical_outcomes





# Hypothetical Outcomes Plot - Animation  ----------------------------------------------

ndraws <- 50

plot_hypothetical_outcomes_animated <-
data_grid(data_baked_multi, 
          mios_total = seq_range(mios_total, n = 100),
          .model = model_temp) %>% 
  add_epred_draws(fit_ologit_multi, 
                  value = "P(bipf_category | mios_total)", 
                  category = "bipf_category") %>%
  ggplot(aes(x = mios_total, 
             y = `P(bipf_category | mios_total)`, 
             color = bipf_category)) +
  stat_lineribbon(aes(fill = bipf_category), 
                  alpha = 1/5, 
                  color = NA, 
                  data = . %>% select(-.draw)) +
  geom_line(aes(group = paste(.draw, bipf_category)), 
            linewidth = 1, 
            data = . %>% sample_draws(ndraws)) +
  scale_fill_cosmic() +
  scale_color_cosmic() +
  gganimate::transition_manual(.draw)

plot_hypothetical_outcomes_animated
