
# This file declares the predictor and outcome variables for each study.  

library(recipes) # We'll use the recipes package to manage the preprocessing


# DATA SET 1 -------------------------------------------------------------------
### Study 1 prompts us to make several decisions: 
### 1. Include or exclude civilians?
### 2. How to code the PCL score for people who did not endorse a trauma? Either leave as is, code as 0, or code as NA
### Our approach is to try all of these, then compare how they change the results. This means 3x2=6 combinations for study 1.  


## Recipe 1 basic. ---------------------------------------------------------
### Create a basic recipe to modify
### Declare the outcome variable and explanatory variables:
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
### Variation 1: Include military and civilian respondents; Leave PCL as it is.

### Print the recipe:
recipe_1 %>% 
  step_center(pcl_total) %>% 
  step_scale(pcl_total, factor = 1) %>% 
  prep(., data_1)                      

### "Bake" the recipe i.e., save a prepared dataset with the above transformations:
### Center the continuous predictors and only the binary variables, not the dummy coded categorical variables
data_baked_1 <- 
  recipe_1  %>% 
  step_center(pcl_total) %>% 
  step_scale(pcl_total, factor = 1) %>% 
  prep(., data_1) %>% bake(., NULL) 


## Recipe 1, Variation 2 -------------------------------------------------------
### Variation 2: Include military and civilian respondents; Code the PCL as 0 if there is no traumatic event endorsement
recipe_1_milandciv_pcl0 <-
  recipe_1 %>% 
  step_mutate(pcl_total = ifelse(trauma == 0, 0, pcl_total)) %>% # Set PCL-5 score to 0 for participants with trauma endorsement:
  step_center(pcl_total) %>%                                     # Center the continuous predictors and only the binary variables, not the dummy coded categorical variables
  step_scale(pcl_total, factor = 1)                              # Scale the continuous predictors
recipe_1_milandciv_pcl0 %>% prep(., data_1)                      # Print the recipe:
data_baked_1_milandciv_pcl0 <- recipe_1_milandciv_pcl0 %>% prep(., data_1) %>% bake(., NULL) # "Bake" the recipe i.e., save a prepared dataset with the above transformations:


## Recipe 1, Variation 3 -------------------------------------------------------
### Variation 3: Military and Civilian, with PCL left as NA for those who don't endorse trauma
recipe_1_milandciv_pclNA <-
  recipe_1 %>% 
  step_mutate(pcl_total = ifelse(trauma == 0, NA, pcl_total)) %>% # Set PCL-5 score to 0 for participants with trauma endorsement:
  step_center(pcl_total) %>%                                      # Center the continuous predictors and only the binary variables, not the dummy coded categorical variables
  step_scale(pcl_total, factor = 1)                               # Scale the continuous predictors
recipe_1_milandciv_pclNA %>% prep(., data_1)                                                    # Print
data_baked_1_milandciv_pclNA <- recipe_1_milandciv_pclNA %>% prep(., data_1) %>% bake(., NULL)  # Bake


## Recipe 1, Variation 4 -------------------------------------------------------
### Variation 4: Remove civilians; PCL coding not modified
recipe_1_mil <- 
  recipe_1 %>% 
  step_filter(civilian == 0) %>% 
  remove_role(civilian, old_role = "predictor")                              # Remove civilians
recipe_1_mil %>% prep(., data_1)                                             # Print
data_baked_1_mil <- recipe_1_mil %>% prep(., data_1) %>% bake(., NULL)  # Bake


## Recipe 1, Variation 5 -------------------------------------------------------
### Variation 5: Remove civilians; PCL coded as 0 for no trauma endorsement
recipe_1_mil_pcl0 <- 
  recipe_1_milandciv_pcl0 %>% 
  step_filter(civilian == 0) %>%
  remove_role(civilian, old_role = "predictor")                                   # Remove civilians
recipe_1_mil_pcl0 %>% prep(., data_1)                                             # Print
data_baked_1_mil_pcl0 <- recipe_1_mil_pcl0 %>% prep(., data_1) %>% bake(., NULL)  # Bake

## Recipe 1, Variation 6 -------------------------------------------------------
### Variation 6: Remove civilians. PCL coded NA for no trauma endorsement
recipe_1_mil_pclNA <- 
  recipe_1_milandciv_pclNA %>% 
  step_filter(civilian == 0) %>% 
  remove_role(civilian, old_role = "predictor")                                    # Remove civilians
recipe_1_mil_pclNA %>% prep(., data_1)                                             # Print
data_baked_1_mil_pclNA <- recipe_1_mil_pclNA %>% prep(., data_1) %>% bake(., NULL) # Bake
  



