
# Hurdle gamma model -----------------------------------------------------------
model_3_hurdle_interact_bi <- 
  brm(
    bf(
      bipf_score ~ pcl_total + mios_total + pcl_total_x_mios_total,
      hu ~ pcl_total + mios_total + pcl_total_x_mios_total
    ),
    
    # DATA SET
    data = data_baked_3_interact,
    
    # MODEL
    family = hurdle_gamma(),
    
    # PRIOR OPTIONS
    prior = weakly_informative_priors,
    sample_prior = 'no',
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr",
    
    # Save the model to avoid refitting
    file = here::here("models/hurdle-3-interact-bi")
  )


