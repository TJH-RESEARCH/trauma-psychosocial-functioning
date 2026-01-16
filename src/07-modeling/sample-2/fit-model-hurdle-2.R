
# Hurdle gamma model -----------------------------------------------------------
model_2_hurdle <- 
  brm(
    bf(
      bipf_score ~ 
        mios_total + 
        pc_ptsd_positive_screen + 
        military_exp_combat + 
        military_exp_noncombat +
        military_exp_support + 
        military_exp_peacekeeping +
        #mos +
        # sex_male + # REFERENCE GROUP
        sex_female +
        # sex_nonbinary +    # removed for no variance
        # sexual_orientation_straight + # REFERENCE GROUP
        sexual_orientation_gay + 
        sexual_orientation_bi +
        race_asian +
        race_black +
        race_latino +
        # race_mena +    # removed for no variance
        race_native +
        # race_pacific +   # removed for no variance
        race_other +
        # race_white + # REFERENCE GROUP
        rank_e1_e3 + 
        # rank_e4_e6 + # REFERENCE GROUP
        rank_e7_e9 + 
        rank_w1_cw5 + 
        rank_o1_o3 + 
        rank_o4_o6 + 
        # service_era_post_wwii +   # removed for no variance
        service_era_korea +       
        service_era_cold_war +
        service_era_vietnam +
        service_era_persian_gulf +
        # service_era_post_911 + # REFERENCE GROUP
        # branch_army + # REFERENCE GROUP
        branch_air_force +
        # branch_coast_guard +  # removed for no variance
        branch_marines +
        branch_navy +
        # branch_space_force +   # removed for no variance
        branch_public_health,
      
      hu ~ 
        mios_total + 
        pc_ptsd_positive_screen + 
        military_exp_combat + 
        military_exp_noncombat +
        military_exp_support + 
        military_exp_peacekeeping +
        #mos +
        # sex_male + # REFERENCE GROUP
        sex_female +
        # sex_nonbinary + # removed for no variance
        # sexual_orientation_straight + # REFERENCE GROUP
        sexual_orientation_gay + 
        sexual_orientation_bi +
        race_asian +
        race_black +
        race_latino +
        # race_mena +  # removed for no variance
        race_native +
        # race_pacific +  # removed for no variance
        race_other +
        # race_white + # REFERENCE GROUP
        rank_e1_e3 + 
        # rank_e4_e6 + # REFERENCE GROUP
        rank_e7_e9 + 
        rank_w1_cw5 + 
        rank_o1_o3 + 
        rank_o4_o6 + 
        #service_era_post_wwii +   # removed for no variance
        service_era_korea +       
        service_era_cold_war +
        service_era_vietnam +
        service_era_persian_gulf +
        # service_era_post_911 + # REFERENCE GROUP
        # branch_army + # REFERENCE GROUP
        branch_air_force +
        #branch_coast_guard +   # removed for no variance
        branch_marines +
        branch_navy +
        #branch_space_force +   # removed for no variance
        branch_public_health
    ),
    
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
    file = here::here("models/hurdle-2")
  )


