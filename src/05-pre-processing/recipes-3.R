
# Data Set 3 --------------------------------------------------------------
## Declare the outcome variable and potential explanatory variables
recipe_3 <-
  recipes::recipe(bipf_score + bipf_category ~ 
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
                  data = data_3) %>% 
  
  
  ## Remove variables with zero variance
  step_zv(all_numeric_predictors()) %>% 
  
  ## Center the continuous predictors and only the binary variables, not the dummy coded categorical variables
  step_center(pcl_total,
              mios_total,
              military_exp_combat,
              military_exp_noncombat,
              military_exp_support,
              military_exp_peacekeeping) %>% 
  
  ## Scale the continuous predictors
  step_scale(pcl_total,
             mios_total)

## Print the recipe
recipe_3 %>% prep(., data_3) 

## "Bake" the recipe i.e., save a prepared dataset with the above transformations
data_baked_3 <- recipe_3 %>% prep(., data_3) %>% bake(., NULL)




# Variation: Interaction --------------------------------------------------
recipe_3_interact <-
  recipe_3 %>% 
  step_interact(terms = ~ pcl_total:mios_total)
recipe_3_interact %>% prep(., data_3)
data_baked_3_interact <- recipe_3_interact %>% prep(., data_3) %>% bake(., NULL)
