
# Hurdle gamma model -----------------------------------------------------------
model_1_hurdle_bi <- 
  brm(
    bf(
      bipf_score ~ pcl_total,
      hu ~ pcl_total
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
    file = here::here("models/hurdle-1-bi")
  )





