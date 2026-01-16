
# Helper Function ---------------------------------------------------------
## This fetches the stepsize and adds a label depending on the model
stepsize <- 
  function(model){
    
    # Deparse the model name to pass to get_model_labels()
    model_name <- deparse(substitute(model))
    
    # Get the model labels using a helper function
    model_labels <- get_model_labels(model_name)
    
    # Fetch the stepsize stats from the object  
    results <- model %>% tidy_draws() %>% count(stepsize__)
    
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
diagnostics_stepsize <- 
  bind_rows(
    stepsize(model_1_hurdle),
    stepsize(model_1_hurdle_improper),
    stepsize(model_1_hurdle_vague),
    stepsize(model_1_hurdle_bi),
    stepsize(model_1_hurdle_bi_improper),
    stepsize(model_1_hurdle_bi_vague),
    stepsize(model_2_hurdle),
    stepsize(model_2_hurdle_improper),
    stepsize(model_2_hurdle_vague),
    stepsize(model_3_hurdle_adjust),
    stepsize(model_3_hurdle_adjust_improper),
    stepsize(model_3_hurdle_adjust_vague),
    stepsize(model_3_hurdle_mios),
    stepsize(model_3_hurdle_mios_improper),
    stepsize(model_3_hurdle_mios_vague),
    stepsize(model_3_hurdle_pcl),
    stepsize(model_3_hurdle_pcl_improper),
    stepsize(model_3_hurdle_pcl_vague),
    stepsize(model_3_hurdle_interact),
    stepsize(model_3_hurdle_interact_improper),
    stepsize(model_3_hurdle_interact_vague)
  )


# Print a sample of row to console ---------------------------------------------
diagnostics_stepsize %>% slice_sample(n = 20) %>% print()

# Save to file -----------------------------------------------------------------
diagnostics_stepsize %>% write_csv(here::here("output/diagnostics/diagnostics-stepsize.csv"))




