
# Hurdle gamma model -----------------------------------------------------------
model_1_hurdle_bi_improper <- 
  brm(
    bf(
      bipf_score ~ pcl_total,
      hu ~ pcl_total
    ),
    
    # DATA SET
    data = data_baked_1,
    
    # MODEL
    family = hurdle_gamma(),
    
    # PRIOR OPTIONS - DEFAULT PRIOR
    prior = vague_priors,
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr",
    
    # Save the model to avoid refitting
    file = here::here("models/hurdle-1-bi-improper")
  )
