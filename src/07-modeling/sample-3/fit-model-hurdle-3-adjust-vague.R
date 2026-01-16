library(brms)
library(tidybayes)

# Hurdle gamma model -----------------------------------------------------------
model_3_hurdle_adjust_vague <- 
  brm(
    bf(
      bipf_score ~ 
        pcl_total + 
        mios_total +
        military_exp_combat + 
        military_exp_noncombat +
        military_exp_support + 
        military_exp_peacekeeping +
        # gender_male + # REFERENCE GROUP
        gender_female + 
        gender_nonbinary +
        foreign_born +
        # mos + # NEED TO CODE
        race_asian +
        race_black +
        race_latino +
        #race_mena + # removed for zero variance
        race_native +
        race_pacific + 
        race_other +
        # race_white + # REFERENCE GROUP
        rank_e1_e3 + 
        # rank_e4_e6 + # REFERENCE GROUP
        rank_e7_e9 + 
        rank_w1_cw5 + 
        rank_o1_o3 + 
        rank_o4_o6 + 
        #service_era_post_wwii + #  removed for zero variance
        #service_era_korea +     #  removed for zero variance  
        service_era_cold_war + 
        service_era_vietnam +
        service_era_persian_gulf +
        # service_era_post_911 + # REFERENCE GROUP
        # branch_army + # REFERENCE GROUP
        branch_air_force +
        branch_coast_guard +
        branch_marines +
        branch_navy +
        #branch_space_force +  #  removed for zero variance  
        branch_public_health,
      
      hu ~ 
        pcl_total + 
        mios_total +
        military_exp_combat + 
        military_exp_noncombat +
        military_exp_support + 
        military_exp_peacekeeping +
        # gender_male + # REFERENCE GROUP
        gender_female + 
        gender_nonbinary +
        foreign_born +
        # mos + # NEED TO CODE
        race_asian +
        race_black +
        race_latino +
        #race_mena +    #  removed for zero variance  
        race_native +
        race_pacific + 
        race_other +
        # race_white + # REFERENCE GROUP
        rank_e1_e3 + 
        # rank_e4_e6 + # REFERENCE GROUP
        rank_e7_e9 + 
        rank_w1_cw5 + 
        rank_o1_o3 + 
        rank_o4_o6 + 
        #service_era_post_wwii +  #  removed for zero variance  
        #service_era_korea +      #  removed for zero variance  
        service_era_cold_war +
        service_era_vietnam +
        service_era_persian_gulf +
        # service_era_post_911 + # REFERENCE GROUP
        # branch_army + # REFERENCE GROUP
        branch_air_force +
        branch_coast_guard +
        branch_marines +
        branch_navy +
        #branch_space_force +   #  removed for zero variance  
        branch_public_health
    ),
    
    # DATA SET
    data = data_baked_3,
    
    # MODEL
    family = hurdle_gamma(),
    
    # PRIOR OPTIONS: default improper priors
    prior = vague_priors,
    sample_prior = 'no',
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr",
    
    # Save the model to avoid refitting
    file = here::here("models/hurdle-3-adjust-vague")
  )


