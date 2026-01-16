# Hurdle gamma model - bivariate -----------------------------------------------------------
model_2_hurdle_bi <- 
  brm(
    bf(bipf_score ~ mios_total, 
       hu ~ mios_total),
    
    # DATA SET
    data = data_baked_2,
    
    # MODEL
    family = hurdle_gamma(),
    
    # PRIOR OPTIONS
    prior = weakly_informative_priors,
    sample_prior = 'no',
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr",
    
    # Save the model to avoid refitting
    file = here::here("models/hurdle-2-bi")
  )