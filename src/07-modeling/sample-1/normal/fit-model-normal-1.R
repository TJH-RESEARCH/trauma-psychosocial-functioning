
# Hurdle gamma model -----------------------------------------------------------
model_1_normal <- 
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
        trauma
    ),
    
    # DATA SET
    data = data_baked_1,
    
    # MODEL
    family = gaussian(),
    
    # PRIOR OPTIONS
    prior = weakly_informative_priors,
    sample_prior = 'no',
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr",
    
    # Save the model to avoid refitting
    file = here::here("models/normal-1")
  )





