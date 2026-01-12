# This file declares the predictor and outcome variables for Sample 2

# Recipe -----------------------------------------------------------------------
## Declare the outcome variable and potential explanatory variables
recipe_2 <-
  recipes::recipe(bipf_score + bipf_category ~ 
                    mios_total + 
                    pc_ptsd_positive_screen + 
                    military_exp_combat + 
                    military_exp_noncombat +
                    military_exp_support + 
                    military_exp_peacekeeping +
                    #mos +
                    # sex_male + # REFERENCE GROUP
                    sex_female +
                    sex_nonbinary + 
                    # sexual_orientation_straight + # REFERENCE GROUP
                    sexual_orientation_gay + 
                    sexual_orientation_bi +
                    race_asian +
                    race_black +
                    race_latino +
                    race_mena + 
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
                    service_era_post_wwii +
                    service_era_korea +       
                    service_era_cold_war +
                    service_era_vietnam +
                    service_era_persian_gulf +
                    # service_era_post_911 + # REFERENCE GROUP
                    # branch_army + # REFERENCE GROUP
                    branch_air_force +
                    branch_coast_guard +
                    branch_marines +
                    branch_navy +
                    branch_space_force +
                    branch_public_health,
                  data = data_2) %>% 
  
  ## Remove variables with zero variance
  step_zv(all_numeric_predictors()) %>% 
  
  ## Center the continuous predictor and only the binary variables, not the dummy coded categorical variables
  step_center(mios_total,
              pc_ptsd_positive_screen,
              military_exp_combat,
              military_exp_noncombat,
              military_exp_support,
              military_exp_peacekeeping,
  ) %>% 
  
  ## Scale the continuous predictor
  step_scale(mios_total, factor = 1)


# Print the recipe ------------------------------------------------------------
recipe_2 %>% prep(., data_2)


# "Bake" the recipe ----------------------------------------------------------
## i.e., save a prepared dataset with the above transformations:
data_baked_2 <- recipe_2  %>% prep(., data_2) %>% bake(., NULL) 
