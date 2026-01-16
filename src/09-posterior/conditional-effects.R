# Conditional Effects Models


# Helper Function ---------------------------------------------------------
## This function plots the conditional_effects for a given model

plot_conditional_effects <- 
  function(model, x_var = 'NULL', dpar = "NULL"){
  
    # Deparse the model name to pass to get_model_labels()
    model_name <- deparse(substitute(model))
    
    # Get the model labels using a helper function
    model_labels <- get_model_labels(model_name)
    
    # Set the X variable depending on the model
    
    x_lab <- 
      case_when(
        model_labels$model_num == "Sample 1" ~ "Posttraumatic Symptoms (PCL)",
        model_labels$model_num == "Sample 2" ~ "Moral Injury Symptoms (MIOS)",
        model_labels$model_num == "Sample 3" & model_labels$model_type == "MIOS" ~ "Moral Injury Symptoms (MIOS)",
        model_labels$model_num == "Sample 3" & model_labels$model_type == "PCL" ~ "Posttraumatic Symptoms (PCL)",
        model_labels$model_num == "Sample 3" & model_labels$model_type == "Adjusted" & x_var == "pcl_total" ~ "Posttraumatic Symptoms (PCL)",
        model_labels$model_num == "Sample 3" & model_labels$model_type == "Adjusted" & x_var == "mios_total" ~ "Moral Injury Symptoms (MIOS)",
        model_labels$model_num == "Sample 3" & model_labels$model_type == "Interaction" & x_var == "pcl_total" ~ "Posttraumatic Symptoms (PCL)",
        model_labels$model_num == "Sample 3" & model_labels$model_type == "Interaction" & x_var == "mios_total" ~ "Moral Injury Symptoms (MIOS)",
        model_labels$model_num == "Sample 3" & model_labels$model_type == "Interaction" & x_var == "NULL" ~ "Interaction Term (PCL * MIOS)",
        .default = NA
    )
    
    # Set the X limits depending on the X variable
    
    x_limits <- 
      case_when(
          x_lab == "Posttraumatic Symptoms (PCL)" ~ c(-1,80),
          x_lab == "Moral Injury Symptoms (MIOS)" ~ c(-1,60),
          x_lab == "Interaction Term (PCL * MIOS)" ~ c(-1, 3),
          .default = NA
      )
    
    x_breaks <- 
      case_when(
        x_lab == "Posttraumatic Symptoms (PCL)" ~ seq(0, 80, 20),
        x_lab == "Moral Injury Symptoms (MIOS)" ~ seq(0, 60, 15),
        x_lab == "Interaction Term (PCL * MIOS)" ~ seq(-1, 3, 1),
        .default = NA
      )
      
    # Set the Y variable depending on the model
      
    y_lab <- 
      case_when(
        dpar == "NULL" ~ "Predicted Dysfunction (bIPF)",
        dpar == "mu" ~ "Predicted Dysfunction (bIPF)",
        dpar == "hu" ~ "Pr (Dysfunction = 0)",
        .default = NA
    )
      
    # Set the Y limits depending on the model
    y_limits <- 
      case_when(
        model_labels$model_num == "Sample 1" & dpar != "hu" ~ c(0, 115),
        model_labels$model_num == "Sample 2" & dpar != "hu" ~ c(0, 170), 
        model_labels$model_num == "Sample 3" & dpar != "hu" ~ c(0, 170),
        dpar == "hu" ~ c(0, 1),
        .default = NA
    )
    
    y_breaks <- 
      case_when(
        model_labels$model_num == "Sample 1" & dpar != "hu" ~ seq(0, 100, 25),
        model_labels$model_num == "Sample 2" & dpar != "hu" ~ seq(0, 160, 40),
        model_labels$model_num == "Sample 3" & dpar != "hu" ~ seq(0, 160, 40),
        dpar != "hu" ~ seq(0, 1, .25),
        .default = NA
      )
    
    title <- paste(model_labels$model_num, "-", model_labels$model_type,  "-", model_labels$model_controls, "-", model_labels$model_prior)
    
    subtitle <-
      case_when(
        dpar == "NULL" ~ "Combined Model (Hurdle + Gamma)",
        dpar == "hu" ~ "Hurdle Model",
        dpar == "mu"  ~ "Gamma Model",
        .default = NA
      )
    
    line_color <-
      case_when(
        dpar == "NULL" ~ colors_tam[1],
        dpar == "hu" ~ colors_tam[3],
        dpar == "mu"  ~ colors_tam[5],
        .default = NA
      )
    
    # Data
    
    if(dpar == "NULL"){ dpar <- NULL}
    
    data <- 
      plot(brms::conditional_effects(model, dpar = dpar), plot = FALSE)[[1]]$data
      
    # transform mios from SD to original units
    data <-
      data %>% 
      mutate(
        effect1__ = 
          case_when(
            model_labels$model_num == "Sample 1" & x_lab == "Posttraumatic Symptoms (PCL)" ~ effect1__ * sd(data_1$pcl_total) + mean(data_1$pcl_total),
            model_labels$model_num == "Sample 2" & x_lab == "Moral Injury Symptoms (MIOS)" ~ effect1__ * sd(data_2$mios_total) + mean(data_2$mios_total),
            model_labels$model_num == "Sample 3" & x_lab == "Posttraumatic Symptoms (PCL)" ~ effect1__ * sd(data_3$pcl_total, na.rm = TRUE) + mean(data_3$pcl_total, na.rm = TRUE),
            model_labels$model_num == "Sample 3" & x_lab == "Moral Injury Symptoms (MIOS)" ~ effect1__ * sd(data_3$mios_total) + mean(data_3$mios_total),
            .default = effect1__
          )
      )
          
    
    # Combined Model
    plot <- 
        data %>% 
        ggplot(aes(x = effect1__, y = estimate__)) +
        geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, color = NA) +
        geom_line(linewidth = 1, color = line_color) +
        scale_x_continuous(limits = x_limits, breaks = x_breaks) +
        scale_y_continuous(limits = y_limits, breaks = y_breaks) +
        labs(x = x_lab, 
             y = y_lab,
             title = title, 
             subtitle = subtitle) +
        theme_scatter
    
    # Create dpar label for name
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
    
      
      
    path <- here::here(paste0("output/conditional-effects/plot-conditional-effects-", str_remove(model_labels$model_num, "Sample "), "-", str_to_lower(model_labels$model_type), "-", str_to_lower(model_labels$model_controls),"-", str_to_lower(model_labels$model_prior), dpar_lab, x_var_lab, ".jpg"))
    
    ggsave(plot = plot, file = path, width = 6, height = 4)
    
    return(plot)
  }



# Save a plot for each model ----------------------------------------------------------

## Sample 1
plot_conditional_effects(model_1_hurdle)
plot_conditional_effects(model_1_hurdle, dpar = "mu")
plot_conditional_effects(model_1_hurdle, dpar = "hu")

plot_conditional_effects(model_1_hurdle_bi)
plot_conditional_effects(model_1_hurdle_bi, dpar = "mu")
plot_conditional_effects(model_1_hurdle_bi, dpar = "hu")

## Sample 2
plot_conditional_effects(model_2_hurdle)
plot_conditional_effects(model_2_hurdle, dpar = "mu")
plot_conditional_effects(model_2_hurdle, dpar = "hu")

## Sample 3, Adjusted
plot_conditional_effects(model_3_hurdle_adjust, x_var = "mios_total")
plot_conditional_effects(model_3_hurdle_adjust, x_var = "mios_total", dpar = "mu")
plot_conditional_effects(model_3_hurdle_adjust, x_var = "mios_total", dpar = "hu")
plot_conditional_effects(model_3_hurdle_adjust, x_var = "pcl_total")
plot_conditional_effects(model_3_hurdle_adjust, x_var = "pcl_total", dpar = "mu")
plot_conditional_effects(model_3_hurdle_adjust, x_var = "pcl_total", dpar = "hu")

## Sample 3, MIOS
plot_conditional_effects(model_3_hurdle_mios)
plot_conditional_effects(model_3_hurdle_mios, dpar = "mu")
plot_conditional_effects(model_3_hurdle_mios, dpar = "hu")

## Sample 3, PCL
plot_conditional_effects(model_3_hurdle_pcl)
plot_conditional_effects(model_3_hurdle_pcl, dpar = "mu")
plot_conditional_effects(model_3_hurdle_pcl, dpar = "hu")

## Sample 3, Interaction
plot_conditional_effects(model_3_hurdle_interact)
plot_conditional_effects(model_3_hurdle_interact, dpar = "mu")
plot_conditional_effects(model_3_hurdle_interact, dpar = "hu")





