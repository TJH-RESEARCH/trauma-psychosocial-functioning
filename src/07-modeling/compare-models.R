# Compare the models using Cross Validation

# Calculate Cross Validation Fit -----------------------------------------------
loo_model_1_hurdle <- loo(model_1_hurdle)
loo_model_1_hurdle_improper <- loo(model_1_hurdle_improper)
loo_model_1_hurdle_vague <- loo(model_1_hurdle_vague)
k_model_2_hurdle <- kfold(model_2_hurdle, K = 10)
k_model_2_hurdle_improper <- kfold(model_2_hurdle_improper, K = 10)
k_model_2_hurdle_vague <- kfold(model_2_hurdle_vague, K = 10)
k_model_3_hurdle_adjust <- kfold(model_3_hurdle_adjust, K = 10)
k_model_3_hurdle_adjust_improper <- kfold(model_3_hurdle_adjust_improper, K = 10)
k_model_3_hurdle_adjust_vague <- kfold(model_3_hurdle_adjust_vague, K = 10)
k_model_3_hurdle_interact <- kfold(model_3_hurdle_interact, K = 10)
k_model_3_hurdle_interact_improper <- kfold(model_3_hurdle_interact_improper, K = 10)
k_model_3_hurdle_interact_vague <- kfold(model_3_hurdle_interact_vague, K = 10)
k_model_3_hurdle_mios <- kfold(model_3_hurdle_mios, K = 10)
k_model_3_hurdle_mios_improper <- kfold(model_3_hurdle_mios_improper, K = 10)
k_model_3_hurdle_mios_vague <- kfold(model_3_hurdle_mios_vague, K = 10)
k_model_3_hurdle_pcl <- kfold(model_3_hurdle_pcl, K = 10)
k_model_3_hurdle_pcl_improper <- kfold(model_3_hurdle_pcl_improper, K = 10)
k_model_3_hurdle_pcl_vague <- kfold(model_3_hurdle_pcl_vague, K = 10)

# Compare Models ---------------------------------------------------------------

## Sample 1 - Compare Priors
loo_compare_1_hurdle <- loo_compare(loo_model_1_hurdle, loo_model_1_hurdle_vague, loo_model_1_hurdle_improper)

## Sample 2 - Compare Priors
k_compare_2_hurdle <- loo_compare(k_model_2_hurdle, k_model_2_hurdle_improper, k_model_2_hurdle_vague)

## Sample 3 - MIOS - Compare Priors
k_compare_3_hurdle_mios <- loo_compare(loo_model_3_hurdle_mios, loo_model_3_hurdle_mios_vague, loo_model_3_hurdle_mios_improper)

## Sample 3 - PCL - Compare Priors
k_compare_3_hurdle_pcl <- loo_compare(k_model_3_hurdle_pcl, k_model_3_hurdle_pcl_vague, k_model_3_hurdle_pcl_improper)

## Sample 3 - Adjusted - Compare Priors
k_compare_3_hurdle_adjust <- loo_compare(k_model_3_hurdle_adjust, k_model_3_hurdle_adjust_vague, k_model_3_hurdle_adjust_improper)

## Sample 3 - Interaction - Compare Priors
k_compare_3_hurdle_interact <- loo_compare(k_model_3_hurdle_interact, k_model_3_hurdle_interact_vague, k_model_3_hurdle_interact_improper)

## Sample 3 - Compare the Interaction to non-interaction models
k_compare_3_vs_interact <- loo_compare(k_model_3_hurdle_adjust, k_model_3_hurdle_interact)


## Print
loo_compare_1_hurdle %>% print()
k_compare_2_hurdle %>% print()
k_compare_3_hurdle_mios %>% print()
k_compare_3_hurdle_pcl %>% print()
k_compare_3_hurdle_adjust %>% print()
k_compare_3_hurdle_interact %>% print()
k_compare_3_vs_interact %>% print()



## The Sample 3 interaction model fits slightly better (about 1SD) than the no-interaction model
## The weakly informative priors fit better in Samples 1 and 2 than the vague or improper priors, both by about 2SD
## But the weakly informative priors are not the best fit for the Sample 3 models, with or without interaction


