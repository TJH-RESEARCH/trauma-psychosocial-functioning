
# Acceptance Stat Plots


# Helper Function ---------------------------------------------------------
## This function plots the acceptance_plot for a given model
acceptance_plot <- 
  function(model){
    
    # Deparse the model name to pass to get_model_labels()
    model_name <- deparse(substitute(model))
    
    # Get the model labels using a helper function
    model_labels <- get_model_labels(model_name)
    
    # Plot the acceptance stat
    plot_acceptance <-
      model %>% 
      tidy_draws() %>% 
      ggplot(aes(accept_stat__)) + 
      geom_histogram() + 
      theme_scatter +
      labs(x = "Acceptance Stat", 
           y = "Count", 
           title = "MCMC Diagnostic: Acceptance Stat", 
           subtitle = paste(model_labels$model_num, "-", model_labels$model_type, "-", model_labels$model_controls, "-",  model_labels$model_prior)
           )
    
    path <- here::here(paste0("output/diagnostics/plot-acceptance-", str_remove(model_labels$model_num, "Sample "), "-", str_to_lower(model_labels$model_type), "-", str_to_lower(model_labels$model_controls), "-", str_to_lower(model_labels$model_prior), ".jpg"))
    
    ggsave(plot = plot_acceptance, file = path, width = 6, height = 4)
    
    return(plot_acceptance)
  }



# Save a plot for each model ----------------------------------------------------------

## Sample 1
acceptance_plot(model_1_hurdle)
acceptance_plot(model_1_hurdle_improper)
acceptance_plot(model_1_hurdle_vague)

acceptance_plot(model_1_hurdle_bi)
acceptance_plot(model_1_hurdle_bi_improper)
acceptance_plot(model_1_hurdle_bi_vague)

## Sample 2
acceptance_plot(model_2_hurdle)
acceptance_plot(model_2_hurdle_improper)
acceptance_plot(model_2_hurdle_vague)

## Sample 3, Adjusted
acceptance_plot(model_3_hurdle_adjust)
acceptance_plot(model_3_hurdle_adjust_improper)
acceptance_plot(model_3_hurdle_adjust_vague)

## Sample 3, MIOS
acceptance_plot(model_3_hurdle_mios)
acceptance_plot(model_3_hurdle_mios_improper)
acceptance_plot(model_3_hurdle_mios_vague)

## Sample 3, PCL
acceptance_plot(model_3_hurdle_pcl)
acceptance_plot(model_3_hurdle_pcl_improper)
acceptance_plot(model_3_hurdle_pcl_vague)

## Sample 3, Interaction
acceptance_plot(model_3_hurdle_interact)
acceptance_plot(model_3_hurdle_interact_improper)
acceptance_plot(model_3_hurdle_interact_vague)




