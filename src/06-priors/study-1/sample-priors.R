library(brms)
library(cmdstanr)

# Fit 1 ---------------------------------------------------------------
model_1_milandciv_pcl0_prior <- 
  brm(
    bf(
      bipf_score ~ pcl_total + veteran + civilian + gender_female + born_79_84 + born_85_89 + born_90_95 + born_96_01 + trauma,   
      hu ~ pcl_total + veteran + civilian + gender_female + born_79_84 + born_85_89 + born_90_95 + born_96_01 + trauma   
    ),
    data = data_baked_1_milandciv_pcl0,
    family = hurdle_gamma(),
    prior = weakly_informative_priors,
    sample_prior = 'only',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED
  )

prior_summary(model_1_milandciv_pcl0_prior)
