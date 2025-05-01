
library(brms)
library(tidybayes)

# Set global Stan options
CHAINS <- 4
ITER <- 2000
WARMUP <- 1000
SEED <- 999888777




# 1. Hurdle Model - Default Prior - PTSD - Bivariate -------------------
hurdle_default_ptsd_bivariate <- 
  brm(
    bf(bipf_score ~ pcl_total),
    data = data_baked_1,
    family = hurdle_gamma(),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )


# 2. Hurdle Model - Default Prior - PTSD - Adjustment Set --------------
hurdle_default_ptsd_adjustment <- 
  brm(
    bf(bipf_score ~ pcl_total +  deployed + gender_female + race_black + race_white),
    data = data_baked_1,
    family = hurdle_gamma(),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )

prior_summary(model_hurdle_ptsd_2)



# 3. Hurdle Model - Default Prior - PTSD - Lots of Controls ------------
hurdle_default_ptsd_controls <- 
  brm(
    bf(bipf_score ~ pcl_total + deployed + gender_female + race_black + race_white + enlisted + mos_combat + branch_army + branch_marines + branch_airforce),
    data = data_baked_1,
    family = hurdle_gamma(),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )

prior_summary(hurdle_weakly_ptsd_controls)



# 4. Hurdle Model - Weakly Informative Prior - PTSD - Bivaraite -------------------
hurdle_weakly_ptsd_bivariate <- 
  brm(
    bf(bipf_score ~ pcl_total),
    data = data_baked_1,
    family = hurdle_gamma(),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 0, 1.75), class = Intercept),
      prior(beta(1, 1.25), class = hu, lb = 0, ub = 1),
      prior(gamma(2, 1), class = shape, lb = 0) 
    ),
    sample_prior = 'no',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )


# 5. Hurdle Model - Weakly Informative Prior - PTSD - Adjustment Set --------------
hurdle_weakly_ptsd_adjustment <- 
  brm(
    bf(bipf_score ~ pcl_total +  deployed + gender_female + race_black + race_white),
    data = data_baked_1,
    family = hurdle_gamma(),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 0, 1.75), class = Intercept),
      prior(beta(1, 1.25), class = hu, lb = 0, ub = 1),
      prior(gamma(2, 1), class = shape, lb = 0) 
    ),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )

prior_summary(model_hurdle_ptsd_2)



# 6. Hurdle Model - Weakly Informative Prior - PTSD - Lots of Controls ------------
hurdle_weakly_ptsd_controls <- 
  brm(
    bf(bipf_score ~ pcl_total + deployed + gender_female + race_black + race_white + enlisted + mos_combat + branch_army + branch_marines + branch_airforce),
    data = data_baked_1,
    family = hurdle_gamma(),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 0, 1.75), class = Intercept),
      prior(beta(1, 1.25), class = hu, lb = 0, ub = 1),
      prior(gamma(2, 1), class = shape, lb = 0) 
    ),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )

prior_summary(hurdle_weakly_ptsd_controls)


# 7. Hurdle Model - Strongly Informative Prior - PTSD - Bivaraite -------------------
hurdle_strongly_ptsd_bivariate <- 
  brm(
    bf(bipf_score ~ pcl_total),
    data = data_baked_1,
    family = hurdle_gamma(),
    prior = c(
      prior(student_t(5, 1, 1), class = b, coef = 'pcl_total'),
      prior(student_t(5, 0, 1.75), class = Intercept),
      prior(beta(1, 1.25), class = hu, lb = 0, ub = 1),
      prior(gamma(2, 1), class = shape, lb = 0) 
    ),
    sample_prior = 'no',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )


# 8. Hurdle Model - Strongly Informative Prior - PTSD - Adjustment Set --------------
hurdle_strongly_ptsd_adjustment <- 
  brm(
    bf(bipf_score ~ pcl_total +  deployed + gender_female + race_black + race_white),
    data = data_baked_1,
    family = hurdle_gamma(),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 1, 1), class = b, coef = 'pcl_total'),
      prior(student_t(5, 0, 1.75), class = Intercept),
      prior(beta(1, 1.25), class = hu, lb = 0, ub = 1),
      prior(gamma(2, 1), class = shape, lb = 0) 
    ),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )

prior_summary(hurdle_strongly_ptsd_adjustment)



# 9. Hurdle Model - Strongly Informative Prior - PTSD - Lots of Controls ------------
hurdle_strongly_ptsd_controls <- 
  brm(
    bf(bipf_score ~ pcl_total + deployed + gender_female + race_black + race_white + enlisted + mos_combat + branch_army + branch_marines + branch_airforce),
    data = data_baked_1,
    family = hurdle_gamma(),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 1, 1), class = b, coef = 'pcl_total'),
      prior(student_t(5, 0, 1.75), class = Intercept),
      prior(beta(1, 1.25), class = hu, lb = 0, ub = 1),
      prior(gamma(2, 1), class = shape, lb = 0) 
    ),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )

prior_summary(hurdle_strongly_ptsd_controls)

