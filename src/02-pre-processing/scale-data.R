
# Scale the data

library(recipes) # We'll use the recipes package to manage the preprocessing

# Data Set 1 --------------------------------------------------------------
## Declare the outcome variable and potential explanatory variables
recipe_1 <-
  
  data_1 %>% 
  
  ## Filter out non-military
  filter(civilian != 1) %>% 
  
  recipes::recipe(bipf_score + bipf_category ~ 
                    pcl_total + 
                    deployed + 
                    # gender_male + # REFERENCE GROUP
                    gender_female + 
                    race_asian +
                    race_black +
                    race_native +
                    race_pacific +
                    race_latinx +
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
                    mos_acquisitions + 
                    branch_airforce +
                    #branch_army + # REFERENCE GROUP
                    branch_marines +
                    branch_navy,
                  data = .) %>% 

  
  ## Remove variables with zero variance
  step_zv(all_numeric_predictors()) %>% 
  
  ## Center the continuous predictors and only the binary variables, not the dummy coded categorical variables
  step_center(pcl_total, deployed) %>% 
  
  ## Scale the continuous predictors
  step_scale(pcl_total,
             factor = 1)

## Print the recipe
recipe_1 %>% prep(., data_1) 

## "Bake" the recipe i.e., save a prepared dataset with the above transformations
data_baked_1 <- recipe_1 %>% prep(., data_1) %>% bake(., NULL)



# Data Set 2 --------------------------------------------------------------
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
  step_scale(mios_total, 
             factor = 1)

## Print the recipe
recipe_2 %>% prep(., data_2) 

## "Bake" the recipe i.e., save a prepared dataset with the above transformations
data_baked_2 <- recipe_2 %>% prep(., data_2) %>% bake(., NULL)



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

