# This file declares the predictor and outcome variables for Sample 1
## Choices: Include military and civilian respondents; Leave PCL as it is.

# Recipe -----------------------------------------------------------------------
recipe_1 <- 
  recipes::recipe(

    ## Declare the outcome variable and explanatory variables:
    bipf_score ~ 
      pcl_total + 
      veteran + civilian +                                # reference: service_member
      gender_female +                                     # reference: gender_male   
      born_79_84 + born_85_89 + born_90_95 + born_96_01 + # reference: born_other
      trauma,                                                           
    data =  data_1) %>% 
    
  ## Remove variables with zero variance
  step_zv(all_numeric_predictors()) %>% 
    
  ## Center the continuous predictors and only the binary variables, not the dummy coded categorical variables
  step_center(pcl_total) %>% 
  step_scale(pcl_total, factor = 1)


# Print the recipe ------------------------------------------------------------
recipe_1 %>% prep(., data_1)                      


# "Bake" the recipe ----------------------------------------------------------
## i.e., save a prepared dataset with the above transformations:
data_baked_1 <- recipe_1  %>% prep(., data_1) %>% bake(., NULL) 

