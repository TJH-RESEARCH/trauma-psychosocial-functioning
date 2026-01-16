# Fit the model to the priors only

# Sample 1, Hurdle Model -------------------------------------------------------
model_1_hurdle_prior <- 
  brm(
    bf(
      bipf_score ~ pcl_total + veteran + civilian + gender_female + born_79_84 + born_85_89 + born_90_95 + born_96_01 + trauma,   
      hu ~ pcl_total + veteran + civilian + gender_female + born_79_84 + born_85_89 + born_90_95 + born_96_01 + trauma   
    ),
    data = data_baked_1,
    family = hurdle_gamma(),
    prior = weakly_informative_priors,
    sample_prior = 'only',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    file = "models/hurdle-1-prior"
  )

## Print the priors to the console
prior_summary(model_1_hurdle_prior)
