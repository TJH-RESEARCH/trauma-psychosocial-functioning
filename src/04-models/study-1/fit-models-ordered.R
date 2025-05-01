
library(brms)
library(tidybayes)

# Set global Stan options
CHAINS <- 4
ITER <- 2000
WARMUP <- 1000
SEED <- 999888777




# 1. Ordered Logit Model - Default Prior - PTSD - Bivariate -------------------
ordered_default_ptsd_bivariate <- 
  brm(
    bf(bipf_category ~ pcl_total),
    data = data_baked_1_categorical,
    family = cumulative(threshold = "flexible"),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )


# 2. Ordered Logit Model - Default Prior - PTSD - Adjustment Set --------------
ordered_default_ptsd_adjustment <- 
  brm(
    bf(bipf_category ~ pcl_total +  deployed + gender_female + race_black + race_white),
    data = data_baked_1_categorical,
    family = cumulative(threshold = "flexible"),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )




# 3. Ordered Logit Model - Default Prior - PTSD - Lots of Controls ------------
ordered_default_ptsd_controls <- 
  brm(
    bf(bipf_category ~ pcl_total + deployed + gender_female + race_black + race_white + enlisted + mos_combat + branch_army + branch_marines + branch_airforce),
    data = data_baked_1_categorical,
    family = cumulative(threshold = "flexible"),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )



# 4. Ordered Logit Model - Weakly Informative Prior - PTSD - Bivaraite -------------------
ordered_weakly_ptsd_bivariate <- 
  brm(
    bf(bipf_category ~ pcl_total),
    data = data_baked_1_categorical,
    family = cumulative(threshold = "flexible"),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 0, 1.75), class = Intercept)
    ),
    sample_prior = 'no',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )


# 5. Ordered Logit Model - Weakly Informative Prior - PTSD - Adjustment Set --------------
ordered_weakly_ptsd_adjustment <- 
  brm(
    bf(bipf_category ~ pcl_total +  deployed + gender_female + race_black + race_white),
    data = data_baked_1_categorical,
    family = cumulative(threshold = "flexible"),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 0, 1.75), class = Intercept)
    ),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )




# 6. Ordered Logit Model - Weakly Informative Prior - PTSD - Lots of Controls ------------
ordered_weakly_ptsd_controls <- 
  brm(
    bf(bipf_category ~ pcl_total + deployed + gender_female + race_black + race_white + enlisted + mos_combat + branch_army + branch_marines + branch_airforce),
    data = data_baked_1_categorical,
    family = cumulative(threshold = "flexible"),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 0, 1.75), class = Intercept)
    ),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )



# 7. Ordered Logit Model - Strongly Informative Prior - PTSD - Bivaraite -------------------
ordered_strongly_ptsd_bivariate <- 
  brm(
    bf(bipf_category ~ pcl_total),
    data = data_baked_1_categorical,
    family = cumulative(threshold = "flexible"),
    prior = c(
      prior(student_t(5, 1, 1), class = b, coef = 'pcl_total'),
      prior(student_t(5, 0, 1.75), class = Intercept)
    ),
    sample_prior = 'no',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )


# 8. Ordered Logit Model - Strongly Informative Prior - PTSD - Adjustment Set --------------
ordered_strongly_ptsd_adjustment <- 
  brm(
    bf(bipf_category ~ pcl_total +  deployed + gender_female + race_black + race_white),
    data = data_baked_1_categorical,
    family = cumulative(threshold = "flexible"),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 1, 1), class = b, coef = 'pcl_total'),
      prior(student_t(5, 0, 1.75), class = Intercept)
    ),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )




# 9. Ordered Logit Model - Strongly Informative Prior - PTSD - Lots of Controls ------------
ordered_strongly_ptsd_controls <- 
  brm(
    bf(bipf_category ~ pcl_total + deployed + gender_female + race_black + race_white + enlisted + mos_combat + branch_army + branch_marines + branch_airforce),
    data = data_baked_1_categorical,
    family = cumulative(threshold = "flexible"),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b),
      prior(student_t(5, 1, 1), class = b, coef = 'pcl_total'),
      prior(student_t(5, 0, 1.75), class = Intercept)
    ),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )


