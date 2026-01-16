# MCM Diagnostic: Effective Sample Size 

# Helper Function ---------------------------------------------------------
## This calculates the ESS and adds a label depending on the model
ess <- 
  function(model){
    
    # Deparse the model name to pass to get_model_labels()
    model_name <- deparse(substitute(model))
    
    # Get the model labels using a helper function
    model_labels <- get_model_labels(model_name)
    
    # Fetch the ESS stats from the object  
    results <- bayestestR::effective_sample(model)
    results$sample <- model_labels$model_num
    results$model <- model_labels$model_type
    results$prior <- model_labels$model_prior
    results$controls <- model_labels$model_controls
      
    results %>% 
      select(sample, model, prior, controls, everything()) %>% # Reorder columns
      tibble() # Make it a tibble format
    
  return(results)
}



# Combine Results ---------------------------------------------------------
diagnostics_ess <- 
  bind_rows(
    ess(model_1_hurdle),
    ess(model_1_hurdle_improper),
    ess(model_1_hurdle_vague),
    ess(model_1_hurdle_bi),
    ess(model_1_hurdle_bi_improper),
    ess(model_1_hurdle_bi_vague),
    ess(model_2_hurdle),
    ess(model_2_hurdle_improper),
    ess(model_2_hurdle_vague),
    ess(model_3_hurdle_adjust),
    ess(model_3_hurdle_adjust_improper),
    ess(model_3_hurdle_adjust_vague),
    ess(model_3_hurdle_mios),
    ess(model_3_hurdle_mios_improper),
    ess(model_3_hurdle_mios_vague),
    ess(model_3_hurdle_pcl),
    ess(model_3_hurdle_pcl_improper),
    ess(model_3_hurdle_pcl_vague),
    ess(model_3_hurdle_interact),
    ess(model_3_hurdle_interact_improper),
    ess(model_3_hurdle_interact_vague)
  )
  

# Print a sample of row to console ---------------------------------------------
diagnostics_ess %>% slice_sample(n = 20) %>% print()

# Save to file -----------------------------------------------------------------
diagnostics_ess %>% write_csv(here::here("output/diagnostics/diagnostics-ess.csv"))




