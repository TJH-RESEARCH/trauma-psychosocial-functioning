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
    prior = weakly_informative_priors,
    sample_prior = 'no',
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr",
    
    # Save the model to avoid refitting
    file = here::here("models/hurdle-1")
  )





