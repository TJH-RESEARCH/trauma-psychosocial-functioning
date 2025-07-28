
# Scale the data

library(recipes) # We'll use the recipes package to manage the preprocessing


# DATA SET 1 -------------------------------------------------------------------


## Declare the outcome variable and explanatory variables:
recipe_1 <- 
  recipes::recipe(
    bipf_score ~ 
      pcl_total + 
      veteran + civilian +                                # reference: service_member
      gender_female +                                     # reference: gender_male   
      born_79_84 + born_85_89 + born_90_95 + born_96_01 + # reference: born_other
      trauma,                                                           
    data =  data_1) %>% 
  step_zv(all_numeric_predictors())                       # Remove variables with zero variance


## Recipe 1, Variation 1 -------------------------------------------------------
### Variation 1: Include military and civilian respondents; Code the PCL as 0 if there is no traumatic event endorsement
recipe_1_milandciv_pcl0 <-
  recipe_1 %>% 
  step_mutate(pcl_total = ifelse(trauma == 0, 0, pcl_total)) %>% # Set PCL-5 score to 0 for participants with trauma endorsement:
  step_center(pcl_total) %>%                                    # Center the continuous predictors and only the binary variables, not the dummy coded categorical variables
  step_scale(pcl_total, factor = 1)                             # Scale the continuous predictors

### Print the recipe:
recipe_1_milandciv_pcl0 %>% prep(., data_1) 
### "Bake" the recipe i.e., save a prepared dataset with the above transformations:
data_baked_1_milandciv_pcl0 <- recipe_1_milandciv_pcl0 %>% prep(., data_1) %>% bake(., NULL)


## Recipe 1, Variation 2 -------------------------------------------------------
### Variation 2: Military and Civilian, with PCL left as NA for those who don't endorse trauma
recipe_1_milandciv_pclNA <-
  recipe_1 %>% 
  step_mutate(pcl_total = ifelse(trauma == 0, NA, pcl_total)) %>% # Set PCL-5 score to 0 for participants with trauma endorsement:
  step_center(pcl_total) %>%             # Center the continuous predictors and only the binary variables, not the dummy coded categorical variables
  step_scale(pcl_total, factor = 1)      # Scale the continuous predictors
recipe_1_milandciv_pclNA %>% prep(., data_1)                                                    # Print
data_baked_1_milandciv_pclNA <- recipe_1_milandciv_pclNA %>% prep(., data_1) %>% bake(., NULL)  # Bake


## Recipe 1, Variation 3 -------------------------------------------------------
### Variation 3: Remove civilians; PCL coding not modified
recipe_1_mil <- 
  recipe_1 %>% 
  step_filter(civilian == 0) %>% 
  remove_role(civilian, old_role = "predictor")                              # Remove civilians
recipe_1_mil %>% prep(., data_1)                                             # Print
data_baked_1_mil <- recipe_1_mil_pcl0 %>% prep(., data_1) %>% bake(., NULL)  # Bake


## Recipe 1, Variation 4 -------------------------------------------------------
### Variation 4: Remove civilians; PCL coded as 0 for no trauma endorsement
recipe_1_mil_pcl0 <- 
  recipe_1_milandciv_pcl0 %>% 
  step_filter(civilian == 0) %>%
  remove_role(civilian, old_role = "predictor")                                   # Remove civilians
recipe_1_mil_pcl0 %>% prep(., data_1)                                             # Print
data_baked_1_mil_pcl0 <- recipe_1_mil_pcl0 %>% prep(., data_1) %>% bake(., NULL)  # Bake


## Recipe 1, Variation 5 -------------------------------------------------------
### Variation 5: Remove civilians. PCL coded NA for no trauma endorsement
recipe_1_mil_pclNA <- 
  recipe_1_milandciv_pclNA %>% 
  step_filter(civilian == 0) %>% 
  remove_role(civilian, old_role = "predictor")                                    # Remove civilians
recipe_1_mil_pclNA %>% prep(., data_1)                                             # Print
data_baked_1_mil_pclNA <- recipe_1_mil_pclNA %>% prep(., data_1) %>% bake(., NULL) # Bake
  








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



