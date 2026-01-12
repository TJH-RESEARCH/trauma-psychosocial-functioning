

library(brms)
library(tidybayes)


# Hurdle gamma model -----------------------------------------------------------
model_1_hurdle <- 
  brm(
    bf(
      bipf_score ~ 
        pcl_total + 
        # MILITARY STATUS  # service_member = REFERENCE GROUP
        veteran +
        civilian +
        # GENDER  # gender_male = REFERENCE GROUP
        gender_female + 
        # AGE  # born_other = REFERENCE GROUP
        born_79_84 +
        born_85_89 + 
        born_90_95 +
        born_96_01 +
        # TRAUMA
        trauma,
      
      hu ~ 
        pcl_total + 
        veteran +
        civilian +
        gender_female + 
        born_79_84 +
        born_85_89 + 
        born_90_95 +
        born_96_01 +
        trauma
    ),
    
    # DATA SET
    data = data_baked_1,
    
    # MODEL
    family = hurdle_gamma(),
    
    # PRIOR OPTIONS
    prior = c(
      # for the non-zero process, in log for 0-100 outcome
      prior(normal(0, 2), class = b),
      prior(normal(2.3, 1), class = Intercept), # baseline around 10 on the 0-100 scale
      
      # for the zero process: probability of being 0. Log odds intercept, log variables, for binary outcome
      prior(normal(0, 1), class = Intercept, dpar = hu),  # hurdle intercept. 
      prior(normal(0, 1), class = b, dpar = hu),          # logistic coefficients
      
      # Shape
      prior(lognormal(log(20), 0.5), class = shape)
    ),
    sample_prior = 'no',
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr"
  )


# Get Draws ---------------------------------------------------------------
draws_hurdle_1 <- model_1_hurdle %>% tidy_draws()





