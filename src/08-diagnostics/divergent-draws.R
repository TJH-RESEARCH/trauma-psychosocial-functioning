
# Helper Function ---------------------------------------------------------
## This fetches the divergent draws and adds a label depending on the model
divergent <- 
  function(model){
    
    # Deparse the model name to pass to get_model_labels()
    model_name <- deparse(substitute(model))
    
    # Get the model labels using a helper function
    model_labels <- get_model_labels(model_name)
    
    # Fetch the divergent stats from the object  
    results <- model %>% tidy_draws() %>% count(divergent__)
    
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
diagnostics_divergent <- 
  bind_rows(
    divergent(model_1_hurdle),
    divergent(model_1_hurdle_improper),
    divergent(model_1_hurdle_vague),
    divergent(model_1_hurdle_bi),
    divergent(model_1_hurdle_bi_improper),
    divergent(model_1_hurdle_bi_vague),
    divergent(model_2_hurdle),
    divergent(model_2_hurdle_improper),
    divergent(model_2_hurdle_vague),
    divergent(model_3_hurdle_adjust),
    divergent(model_3_hurdle_adjust_improper),
    divergent(model_3_hurdle_adjust_vague),
    divergent(model_3_hurdle_mios),
    divergent(model_3_hurdle_mios_improper),
    divergent(model_3_hurdle_mios_vague),
    divergent(model_3_hurdle_pcl),
    divergent(model_3_hurdle_pcl_improper),
    divergent(model_3_hurdle_pcl_vague),
    divergent(model_3_hurdle_interact),
    divergent(model_3_hurdle_interact_improper),
    divergent(model_3_hurdle_interact_vague)
  )


# Print a sample of row to console ---------------------------------------------
diagnostics_divergent %>% slice_sample(n = 20) %>% print()

# Save to file -----------------------------------------------------------------
diagnostics_divergent %>% write_csv(here::here("output/diagnostics/diagnostics-divergent.csv"))




