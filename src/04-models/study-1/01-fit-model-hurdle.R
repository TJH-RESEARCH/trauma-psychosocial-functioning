

library(tidybayes)


# Hurdle gamma model -----------------------------------------------------------
model_1_hurdle <- 
  brm(
    bf(
      bipf_score ~ 
        pcl_total + 
        deployed + 
        # gender_male + # REFERENCE GROUP
        gender_female + 
        race_asian +
        race_black +
        race_native +
        #race_pacific + # removed for non-variance i.e., all 0s
        #race_latinx + # removed for non-variance i.e., all 0s
        race_other +
        # race_white + # REFERENCE GROUP
        rank_e1_e4 + 
        # rank_e5_e6 + # REFERENCE GROUP
        rank_e7_e9 + 
        rank_o1_o3_w1_cw2 + 
        rank_o4_o6_cw3_cw5 + 
        mos_combat +
        mos_combat_support + 
        mos_combat_service_support + 
        mos_operations + 
        mos_logistics + 
        mos_support + 
        mos_medical +
        # mos_acquisitions + # removed for non-variance i.e., all 0s
        branch_airforce +
        #branch_army + # REFERENCE GROUP
        branch_marines,
      #branch_navy # removed for non-variance i.e., all 0s
      
      hu ~ 
        pcl_total + 
        deployed + 
        # gender_male + # REFERENCE GROUP
        gender_female + 
        race_asian +
        race_black +
        race_native +
        #race_pacific + # removed for non-variance i.e., all 0s
        #race_latinx + # removed for non-variance i.e., all 0s
        race_other +
        # race_white + # REFERENCE GROUP
        rank_e1_e4 + 
        # rank_e5_e6 + # REFERENCE GROUP
        rank_e7_e9 + 
        rank_o1_o3_w1_cw2 + 
        rank_o4_o6_cw3_cw5 + 
        mos_combat +
        mos_combat_support + 
        mos_combat_service_support + 
        mos_operations + 
        mos_logistics + 
        mos_support + 
        mos_medical +
        # mos_acquisitions + # removed for non-variance i.e., all 0s
        branch_airforce +
        #branch_army + # REFERENCE GROUP
        branch_marines
    ),
    
    # DATA SET
    data = data_baked_1,
    
    # MODEL
    family = hurdle_gamma(),
    
    # PRIOR OPTIONS
    prior = c(
      # for the non-zero process, in log for 0-100 outcome
      prior(normal(0, 2), class = b),
      prior(normal(2.3, 1), class = Intercept), # baseline around 10 on the 0-100 scale
      
      # for the zero process: probability of being 0. Log odds intercept, log variables, for binary outcome
      prior(normal(0, 1), class = Intercept, dpar = hu),  # hurdle intercept. 
      prior(normal(0, 1), class = b, dpar = hu),          # logistic coefficients
      
      # Shape
      prior(lognormal(log(20), 0.5), class = shape)
    ),
    sample_prior = 'no',
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr"
  )


# Get Draws ---------------------------------------------------------------
draws_hurdle_1 <- model_1_hurdle %>% tidy_draws()


# Check Rhat and Effective Sample Size -----------------------------------------
draws_hurdle_1 %>% count(divergent__)
draws_hurdle_1 %>% count(n_leapfrog__)
draws_hurdle_1 %>% count(stepsize__)
draws_hurdle_1 %>% ggplot(aes(accept_stat__)) + geom_density()
bayestestR::effective_sample(model_1_hurdle)
bayestestR::diagnostic_draws(model_1_hurdle)


broom.mixed::tidy(model_1_hurdle)


# exp(3.95)
exp(0.323)
exp(0.323) * 2
