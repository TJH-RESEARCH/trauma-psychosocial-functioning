
# Hurdle gamma model -----------------------------------------------------------
model_1_normal_bi_vague <- 
  brm(
    bf(bipf_score ~ pcl_total),
    
    # DATA SET
    data = data_baked_1,
    
    # MODEL
    family = gaussian(),
    
    # PRIOR OPTIONS
    prior = vague_priors,
    sample_prior = 'no',
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr",
    
    # Save the model to avoid refitting
    file = here::here("models/normal-1-bi-vague")
  )





