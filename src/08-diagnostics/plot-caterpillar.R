# Caterpillar Plots


# Helper Function ---------------------------------------------------------
## This function plots the catepillar for a given model
catepillar <- 
  function(model){
    
    # Deparse the model name to pass to get_model_labels()
    model_name <- deparse(substitute(model))
    
    # Get the model labels using a helper function
    model_labels <- get_model_labels(model_name)
    
    catepillar_plot <-
      model %>% 
      tidy_draws() %>% 
      select(.chain, .iteration, .draw, contains('b_')) %>% 
      pivot_longer(-c(.chain, .iteration, .draw)) %>%
      ggplot(aes(.iteration, value)) + 
      geom_line(aes(color = factor(.chain), alpha = .5)) +
      ggsci::scale_color_aaas() +
      facet_grid(vars(name), scales = 'free_y') +
      labs(title = "MCMM Sampling Diagnostics", 
           subtitle = paste(model_labels$model_num, "-", model_labels$model_type, "-", model_labels$model_controls, "-", model_labels$model_prior)
      )
    
    path <- here::here(paste0("output/diagnostics/plot-catepillar-", str_remove(model_labels$model_num, "Sample "), "-", str_to_lower(model_labels$model_type), "-", str_to_lower(model_labels$model_controls), "-", str_to_lower(model_labels$model_prior), ".jpg"))
    
    ggsave(plot = catepillar_plot, file = path, width = 6, height = 40)
    
    return(catepillar_plot)
  }



# Save a plot for each model ----------------------------------------------------------

## Sample 1
catepillar(model_1_hurdle)
catepillar(model_1_hurdle_improper)
catepillar(model_1_hurdle_vague)

catepillar(model_1_hurdle_bi)
catepillar(model_1_hurdle_bi_improper)
catepillar(model_1_hurdle_bi_vague)

## Sample 2
catepillar(model_2_hurdle)
catepillar(model_2_hurdle_improper)
catepillar(model_2_hurdle_vague)

## Sample 3, Adjusted
catepillar(model_3_hurdle_adjust)
catepillar(model_3_hurdle_adjust_improper)
catepillar(model_3_hurdle_adjust_vague)

## Sample 3, MIOS
catepillar(model_3_hurdle_mios)
catepillar(model_3_hurdle_mios_improper)
catepillar(model_3_hurdle_mios_vague)

## Sample 3, PCL
catepillar(model_3_hurdle_pcl)
catepillar(model_3_hurdle_pcl_improper)
catepillar(model_3_hurdle_pcl_vague)

## Sample 3, Interaction
catepillar(model_3_hurdle_interact)
catepillar(model_3_hurdle_interact_improper)
catepillar(model_3_hurdle_interact_vague)




