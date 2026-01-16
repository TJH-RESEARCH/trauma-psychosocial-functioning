
# Trace Rank Plots


# Helper Function ---------------------------------------------------------
## This function plots the trace_rank for a given model
trace_rank <- 
  function(model){
    
    # Deparse the model name to pass to get_model_labels()
    model_name <- deparse(substitute(model))
    
    # Get the model labels using a helper function
    model_labels <- get_model_labels(model_name)
    
    # Get the variables used in the model
    pars <- model %>% tidy_draws() %>% select(starts_with("b_")) %>% names()
    
    # Plot the trace rank
    trace_rank_plot <-
      bayesplot::mcmc_rank_overlay(model, pars = pars) +
      scale_x_continuous(breaks = c(0, 16000)) +
      scale_y_continuous(breaks = c(0, 250)) + 
      labs(title = "MCMC Trace Rank", 
           subtitle = paste(model_labels$model_num, "-", model_labels$model_type, "-", model_labels$model_controls, "-", model_labels$model_prior)
      ) +
      theme(axis.text = element_text(size = 6))
    
    path <- here::here(paste0("output/diagnostics/plot-trace-rank-", str_remove(model_labels$model_num, "Sample "), "-", str_to_lower(model_labels$model_type), "-", str_to_lower(model_labels$model_controls), "-", str_to_lower(model_labels$model_prior), ".jpg"))
    
    ggsave(plot = trace_rank_plot, file = path, width = 10, height = 10)
    
    return(trace_rank_plot)
  }



# Save a plot for each model ----------------------------------------------------------

## Sample 1
trace_rank(model_1_hurdle)
trace_rank(model_1_hurdle_improper)
trace_rank(model_1_hurdle_vague)

trace_rank(model_1_hurdle_bi)
trace_rank(model_1_hurdle_bi_improper)
trace_rank(model_1_hurdle_bi_vague)

## Sample 2
trace_rank(model_2_hurdle)
trace_rank(model_2_hurdle_improper)
trace_rank(model_2_hurdle_vague)

## Sample 3, Adjusted
trace_rank(model_3_hurdle_adjust)
trace_rank(model_3_hurdle_adjust_improper)
trace_rank(model_3_hurdle_adjust_vague)

## Sample 3, MIOS
trace_rank(model_3_hurdle_mios)
trace_rank(model_3_hurdle_mios_improper)
trace_rank(model_3_hurdle_mios_vague)

## Sample 3, PCL
trace_rank(model_3_hurdle_pcl)
trace_rank(model_3_hurdle_pcl_improper)
trace_rank(model_3_hurdle_pcl_vague)

## Sample 3, Interaction
trace_rank(model_3_hurdle_interact)
trace_rank(model_3_hurdle_interact_improper)
trace_rank(model_3_hurdle_interact_vague)




