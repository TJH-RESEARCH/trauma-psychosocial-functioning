

summarize_posterior <- function(model, x_var = "NULL"){
  
  results <-
    model %>% 
    tidy_draws() %>% 
    tidybayes::summarise_draws(default_summary_measures()) %>% 
    filter(str_detect(variable, "b_")) %>% 
    mutate(across(where(is.numeric), exp)) %>% 
    mutate(term = ifelse(str_detect(variable, "b_hu"), "Odds Ratio", "Multiplicative")) %>% 
    arrange(term, variable)
  
  # Save the file
  model_name <- deparse(substitute(model)) # Deparse the model name to pass to get_model_labels()
  model_labels <- get_model_labels(model_name) # Get the model labels using a helper function
  
  # Create x variable label for name
  if (x_var != "NULL") {
    x_var_lab <- paste0("-", str_remove(x_var, "_total"))
  } else if (x_var == "NULL") {
    x_var_lab <- ""
  }
  
  path <- here::here(paste0("output/posterior-summary/summary-", str_remove(model_labels$model_num, "Sample "), "-", str_to_lower(model_labels$model_type), "-", str_to_lower(model_labels$model_controls), "-", str_to_lower(model_labels$model_prior), x_var_lab, ".csv"))
  
  write_csv(results, file = path)
  
  
  return(results)
}



# Save results for each model ----------------------------------------------------------

## Sample 1
summarize_posterior(model_1_hurdle)
summarize_posterior(model_1_hurdle_bi)

## Sample 2
summarize_posterior(model_2_hurdle)

## Sample 3, Adjusted
summarize_posterior(model_3_hurdle_adjust, x_var = "mios_total")

## Sample 3, MIOS
summarize_posterior(model_3_hurdle_mios)

## Sample 3, PCL
summarize_posterior(model_3_hurdle_pcl)

## Sample 3, Interaction
summarize_posterior(model_3_hurdle_interact)



