
# Scale the data

library(recipes) # We'll use the recipes package to manage the preprocessing

# Data Set 1 --------------------------------------------------------------
## Declare the outcome variable and potential explanatory variables
recipe_1 <-
  recipes::recipe(bipf_score + bipf_category ~ 
                    pcl_total + 
                    deployed + 
                    gender_female + 
                    race_black +
                    race_white +
                    enlisted + 
                    mos_combat +
                    branch_army +
                    branch_marines +
                    branch_airforce, 
                  data = data_1) %>% 
  
## Center only the predictors
  step_center(all_numeric_predictors()) %>% 
  
## Scale both the predictors and outcome by a factor of 2
  step_scale(all_numeric_predictors(), 
             bipf_score,
             factor = 2)

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
                    sex_female + 
                    race_black +
                    race_white +
                    enlisted + 
                    #mos +
                    branch_army +
                    branch_marines +
                    branch_air_force, 
                  data = data_2) %>% 
  
  ## Center only the predictors
  step_center(all_numeric_predictors()) %>% 
  
  ## Scale both the predictors and outcome by a factor of 2
  step_scale(all_numeric_predictors(), 
             bipf_score,
             factor = 2)

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
                    gender_female + 
                    race_black +
                    race_white +
                    enlisted + 
                    # mos +
                    branch_army +
                    branch_marines +
                    branch_air_force, 
                  data = data_3) %>% 
  
  ## Center only the predictors
  step_center(all_numeric_predictors()) %>% 
  
  ## Scale both the predictors and outcome by a factor of 2
  step_scale(all_numeric_predictors(), 
             bipf_score,
             factor = 2)

## Print the recipe
recipe_3 %>% prep(., data_3) 

## "Bake" the recipe i.e., save a prepared dataset with the above transformations
data_baked_3 <- recipe_3 %>% prep(., data_3) %>% bake(., NULL)

