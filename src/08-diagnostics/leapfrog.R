
# Helper Function ---------------------------------------------------------
## This fetches the leapfrog and adds a label depending on the model
leapfrog <- 
  function(model){
    
    # Deparse the model name to pass to get_model_labels()
    model_name <- deparse(substitute(model))
    
    # Get the model labels using a helper function
    model_labels <- get_model_labels(model_name)
    
    # Fetch the leapfrog stats from the object  
    results <- model %>% tidy_draws() %>% count(n_leapfrog__)
    
    results$sample <- model_labels$model_num
    results$model <- model_labels$model_type
    results$prior <- model_labels$model_prior
    results$controls <- model_labels$model_controls
    
    results <-
      results %>% 
      select(sample, model, prior, controls, everything()) %>% # Reorder columns
      tibble() # Make it a tibble format
    
    return(results)
  }



# Combine Results ---------------------------------------------------------
diagnostics_leapfrog <- 
  bind_rows(
    leapfrog(model_1_hurdle),
    leapfrog(model_1_hurdle_improper),
    leapfrog(model_1_hurdle_vague),
    leapfrog(model_1_hurdle_bi),
    leapfrog(model_1_hurdle_bi_improper),
    leapfrog(model_1_hurdle_bi_vague),
    leapfrog(model_2_hurdle),
    leapfrog(model_2_hurdle_improper),
    leapfrog(model_2_hurdle_vague),
    leapfrog(model_3_hurdle_adjust),
    leapfrog(model_3_hurdle_adjust_improper),
    leapfrog(model_3_hurdle_adjust_vague),
    leapfrog(model_3_hurdle_mios),
    leapfrog(model_3_hurdle_mios_improper),
    leapfrog(model_3_hurdle_mios_vague),
    leapfrog(model_3_hurdle_pcl),
    leapfrog(model_3_hurdle_pcl_improper),
    leapfrog(model_3_hurdle_pcl_vague),
    leapfrog(model_3_hurdle_interact),
    leapfrog(model_3_hurdle_interact_improper),
    leapfrog(model_3_hurdle_interact_vague)
  )


# Print a sample of row to console ---------------------------------------------
diagnostics_leapfrog %>% slice_sample(n = 20) %>% print()

# Save to file -----------------------------------------------------------------
diagnostics_leapfrog %>% write_csv(here::here("output/diagnostics/diagnostics-leapfrog.csv"))




