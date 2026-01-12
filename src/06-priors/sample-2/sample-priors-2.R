library(brms)
library(cmdstanr)

# Study 2: Sample from the Prior only ------------------------------------------
model_2_prior <- 
  brm(
    bf(
      bipf_score ~ mios_total + pc_ptsd_positive_screen + military_exp_combat + military_exp_noncombat + military_exp_support + military_exp_peacekeeping + sex_female + sexual_orientation_gay + sexual_orientation_bi + race_asian + race_black + race_latino + race_native + race_other + rank_e1_e3 + rank_e7_e9 + rank_w1_cw5 + rank_o1_o3 + rank_o4_o6 + service_era_korea + service_era_cold_war + service_era_vietnam + service_era_persian_gulf + branch_air_force + branch_marines + branch_navy + branch_public_health,
      hu  ~ mios_total + pc_ptsd_positive_screen + military_exp_combat + military_exp_noncombat + military_exp_support + military_exp_peacekeeping + sex_female + sexual_orientation_gay + sexual_orientation_bi + race_asian + race_black + race_latino + race_native + race_other + rank_e1_e3 + rank_e7_e9 + rank_w1_cw5 + rank_o1_o3 + rank_o4_o6 + service_era_korea + service_era_cold_war + service_era_vietnam + service_era_persian_gulf + branch_air_force + branch_marines + branch_navy + branch_public_health
    ),
    data = data_baked_2,
    family = hurdle_gamma(),
    prior = weakly_informative_priors,
    sample_prior = 'only',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED
  )

prior_summary(model_2_prior)
