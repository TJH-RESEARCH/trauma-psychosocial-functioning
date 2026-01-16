# Posterior Distribution





# Helper Function ---------------------------------------------------------
## This function plots the conditional_effects for a given model

plot_ppc <- 
  function(model, x_var = 'NULL', dpar = "NULL"){
    
    # Deparse the model name to pass to get_model_labels()
    model_name <- deparse(substitute(model))
    
    # Get the model labels using a helper function
    model_labels <- get_model_labels(model_name)
    
    title <- paste(model_labels$model_num, "-", model_labels$model_type, "-", model_labels$model_controls, "-", model_labels$model_prior)
    
    plot <-
      pp_check(model, ndraws = 100) +
      labs(title = title,
           subtitle = paste0("<span style = 'color:#9e9e9e'> **Posterior Predictive Check**</span> (", model_labels$model_num, ")"),
           x = "Predicted Dysfunction (bIPF)",
           y = 'Density') +
      lims(x = c(0, 200)) +
      theme_ppc
    
    
    # Create dpar label for name
    if(dpar == "NULL"){ dpar <- NULL}
    if (!is.null(dpar)) {
      dpar_lab <- paste0("-", dpar)
    } else if (is.null(dpar)) {
      dpar_lab <- ""
    }
    
    # Create x variable label for name
    if (x_var != "NULL") {
      x_var_lab <- paste0("-", str_remove(x_var, "_total"))
    } else if (x_var == "NULL") {
      x_var_lab <- ""
    }
    
    
    path <- here::here(paste0("output/ppc/plot-ppc-", str_remove(model_labels$model_num, "Sample "), "-", str_to_lower(model_labels$model_type), "-", str_to_lower(model_labels$model_controls),"-", str_to_lower(model_labels$model_prior), dpar_lab, x_var_lab, ".jpg"))
    
    ggsave(plot = plot, file = path, width = 6, height = 4)
    
    return(plot)
  }



# Save a plot for each model ----------------------------------------------------------

## Sample 1
plot_ppc(model_1_hurdle, x_var = "pcl_total", dpar = "hu")
plot_ppc(model_1_hurdle, x_var = "pcl_total", dpar = "mu")

plot_ppc(model_1_hurdle_bi, x_var = "pcl_total", dpar = "hu")
plot_ppc(model_1_hurdle_bi, x_var = "pcl_total", dpar = "mu")

## Sample 2
plot_ppc(model_2_hurdle, x_var = "mios_total", dpar = "hu")
plot_ppc(model_2_hurdle, x_var = "mios_total", dpar = "mu")


## Sample 3, Adjusted
plot_ppc(model_3_hurdle_adjust, x_var = "mios_total", dpar = "mu")
plot_ppc(model_3_hurdle_adjust, x_var = "mios_total", dpar = "hu")
plot_ppc(model_3_hurdle_adjust, x_var = "pcl_total", dpar = "mu")
plot_ppc(model_3_hurdle_adjust, x_var = "pcl_total", dpar = "hu")

## Sample 3, MIOS
plot_ppc(model_3_hurdle_mios, x_var = "mios_total", dpar = "mu")
plot_ppc(model_3_hurdle_mios, x_var = "mios_total", dpar = "hu")

## Sample 3, PCL
plot_ppc(model_3_hurdle_pcl, x_var = "pcl_total", dpar = "mu")
plot_ppc(model_3_hurdle_pcl, x_var = "pcl_total", dpar = "mu")

## Sample 3, Interaction
plot_ppc(model_3_hurdle_interact, x_var = "pcl_total_x_mios_total", dpar = "mu")
plot_ppc(model_3_hurdle_interact, x_var = "pcl_total_x_mios_total", dpar = "hu")





