# Marginal Effects


# Helper Function ---------------------------------------------------------
## This function plots the marginal effects for a given model

plot_marginal_effects <- 
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
        model_labels$model_num == "Sample 3" & model_labels$model_type == "Interaction" ~ "Interaction Term (PCL * MIOS)",
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
        dpar == "NULL" ~ "Marginal Change in Dysfunction",
        dpar == "mu" ~ "Marginal Change in Dysfunction",
        dpar == "hu" ~ "Marginal Change in Pr (Dysfunction = 0)",
        .default = NA
      )
    
    # Set the Y limits depending on the model
    y_limits <- 
      case_when(
        model_labels$model_num == "Sample 1" & dpar != "hu" ~ c(-20, 20),
        model_labels$model_num == "Sample 2" & dpar != "hu" ~ c(-20, 20), 
        model_labels$model_num == "Sample 3" & dpar != "hu" ~ c(-20, 20),
        dpar == "hu" ~ c(-.5, .5),
        .default = NA
      )
    
    y_breaks <- 
      case_when(
        model_labels$model_num == "Sample 1" & dpar != "hu" ~ seq(-20, 20, 2),
        model_labels$model_num == "Sample 2" & dpar != "hu" ~ seq(-20, 20, 2),
        model_labels$model_num == "Sample 3" & dpar != "hu" ~ seq(-20, 20, 2),
        dpar == "hu" ~ seq(-.5, .5, .050),
        .default = NA
      )
    
    line_color <-
      case_when(
        dpar == "NULL" ~ colors_tam[6],
        dpar == "hu" ~ colors_tam[7],
        dpar == "mu"  ~ colors_tam[8],
        .default = NA
      )
    
    title <- paste(model_labels$model_num, "-", model_labels$model_type,  "-", model_labels$model_controls, "-", model_labels$model_prior)
    
    subtitle_label <-
      case_when(
        dpar == "NULL" ~ "Combined Model (Hurdle + Gamma)",
        dpar == "hu" ~ "Hurdle Model",
        dpar == "mu"  ~ "Gamma Model",
        .default = NA
      )
    
    subtitle <- paste0("<span style = 'color:", line_color,"'> **Average marginal effects**</span> for ", subtitle_label," (", model_labels$model_num,")")
    
  
    # Data
    if(dpar == "NULL"){type <- "combo"} else if(dpar == "mu"){ type <- "mu"} else if(dpar == "hu"){ type <- "hu"}
    if(dpar == "NULL"){ dpar <- NULL}
    
    data <-
      plot_slopes(
        model,
        dpar = dpar, 
        variables = x_var,
        condition = x_var,
        draw = FALSE # to get the data set
      ) %>% 
      tibble() 
    
  
    
    # transform mios from SD to original units
    data <-
      data %>% 
      mutate(
        estimate = 
          case_when(
            model_labels$model_num == "Sample 1" & x_lab == "Posttraumatic Symptoms (PCL)" ~ estimate / sd(data_1$pcl_total),
            model_labels$model_num == "Sample 2" & x_lab == "Moral Injury Symptoms (MIOS)" ~ estimate / sd(data_2$mios_total),
            model_labels$model_num == "Sample 3" & x_lab == "Posttraumatic Symptoms (PCL)" ~ estimate / sd(data_3$pcl_total, na.rm = TRUE),
            model_labels$model_num == "Sample 3" & x_lab == "Moral Injury Symptoms (MIOS)" ~ estimate / sd(data_3$mios_total, na.rm = TRUE),
            .default = estimate
          ),
        conf.low = 
          case_when(
            model_labels$model_num == "Sample 1" & x_lab == "Posttraumatic Symptoms (PCL)" ~ conf.low / sd(data_1$pcl_total),
            model_labels$model_num == "Sample 2" & x_lab == "Moral Injury Symptoms (MIOS)" ~ conf.low / sd(data_2$mios_total),
            model_labels$model_num == "Sample 3" & x_lab == "Posttraumatic Symptoms (PCL)" ~ conf.low / sd(data_3$pcl_total, na.rm = TRUE),
            model_labels$model_num == "Sample 3" & x_lab == "Moral Injury Symptoms (MIOS)" ~ conf.low / sd(data_3$mios_total, na.rm = TRUE),
            .default = conf.low
          ),
        conf.high = 
          case_when(
            model_labels$model_num == "Sample 1" & x_lab == "Posttraumatic Symptoms (PCL)" ~ conf.high / sd(data_1$pcl_total),
            model_labels$model_num == "Sample 2" & x_lab == "Moral Injury Symptoms (MIOS)" ~ conf.high / sd(data_2$mios_total),
            model_labels$model_num == "Sample 3" & x_lab == "Posttraumatic Symptoms (PCL)" ~ conf.high / sd(data_3$pcl_total, na.rm = TRUE),
            model_labels$model_num == "Sample 3" & x_lab == "Moral Injury Symptoms (MIOS)" ~ conf.high / sd(data_3$mios_total, na.rm = TRUE),
            .default = conf.high
          )
      )
    
    # Scale the X variable depending on which variable
    if(x_var == "pcl_total"){
      data <-
        data %>% 
        mutate(pcl_total = 
                 case_when(
                   model_labels$model_num == "Sample 1" ~ pcl_total * sd(data_1$pcl_total) + mean(data_1$pcl_total),
                   model_labels$model_num == "Sample 3" ~ pcl_total * sd(data_3$pcl_total, na.rm = TRUE) + mean(data_3$pcl_total, na.rm = TRUE),
                   .default = pcl_total
                 ))
    }
    
    if(x_var == "mios_total"){
      data <-
        data %>% 
        mutate(mios_total = 
                 case_when(
                   model_labels$model_num == "Sample 2" ~ mios_total * sd(data_2$mios_total, na.rm = TRUE) + mean(data_3$mios_total, na.rm = TRUE),
                   model_labels$model_num == "Sample 3" ~ mios_total * sd(data_3$mios_total, na.rm = TRUE) + mean(data_3$mios_total, na.rm = TRUE),
                   .default = mios_total
                 )
        )
    }
    
    
    ### Plot
    plot <-
      data %>% 
      ggplot(aes(.data[[x_var]], estimate)) +
      geom_smooth(method = "loess", 
                  formula = "y ~ x",
                  color = line_color,
                  linewidth = 2) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, color = NA) +
      labs(
        x = x_lab,
        y = y_lab,
        title = title, 
        subtitle = subtitle
      ) + 
      scale_y_continuous(limits = y_limits, breaks = y_breaks) +
      theme_scatter
    
    # Replace limits if hurdle model
    if(type == "hu"){plot <- plot + scale_y_continuous(limits = y_limits, breaks = y_breaks, labels = scales::label_percent())}
    

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
    
    
    
    path <- here::here(paste0("output/conditional-effects/plot-conditional-effects-", str_remove(model_labels$model_num, "Sample "), "-", str_to_lower(model_labels$model_type), "-", str_to_lower(model_labels$model_prior), dpar_lab, x_var_lab, ".jpg"))
    
    ggsave(plot = plot, file = path, width = 6, height = 4)
    
    return(plot)
  }



# Save a plot for each model ----------------------------------------------------------

## Sample 1
plot_marginal_effects(model_1_hurdle, x_var = "pcl_total")
plot_marginal_effects(model_1_hurdle, x_var = "pcl_total", dpar = "mu")
plot_marginal_effects(model_1_hurdle, x_var = "pcl_total", dpar = "hu")

plot_marginal_effects(model_1_hurdle_bi, x_var = "pcl_total")
plot_marginal_effects(model_1_hurdle_bi, x_var = "pcl_total", dpar = "mu")
plot_marginal_effects(model_1_hurdle_bi, x_var = "pcl_total", dpar = "hu")

## Sample 2
plot_marginal_effects(model_2_hurdle, x_var = "mios_total")
plot_marginal_effects(model_2_hurdle, x_var = "mios_total", dpar = "mu")
plot_marginal_effects(model_2_hurdle, x_var = "mios_total", dpar = "hu")

## Sample 3, Adjusted
plot_marginal_effects(model_3_hurdle_adjust, x_var = "mios_total")
plot_marginal_effects(model_3_hurdle_adjust, x_var = "mios_total", dpar = "mu")
plot_marginal_effects(model_3_hurdle_adjust, x_var = "mios_total", dpar = "hu")
plot_marginal_effects(model_3_hurdle_adjust, x_var = "pcl_total")
plot_marginal_effects(model_3_hurdle_adjust, x_var = "pcl_total", dpar = "mu")
plot_marginal_effects(model_3_hurdle_adjust, x_var = "pcl_total", dpar = "hu")

## Sample 3, MIOS
plot_marginal_effects(model_3_hurdle_mios, x_var = "mios_total")
plot_marginal_effects(model_3_hurdle_mios, x_var = "mios_total", dpar = "mu")
plot_marginal_effects(model_3_hurdle_mios, x_var = "mios_total", dpar = "hu")

## Sample 3, PCL
plot_marginal_effects(model_3_hurdle_pcl, x_var = "pcl_total")
plot_marginal_effects(model_3_hurdle_pcl, x_var = "pcl_total", dpar = "mu")
plot_marginal_effects(model_3_hurdle_pcl, x_var = "pcl_total", dpar = "hu")

## Sample 3, Interaction
plot_marginal_effects(model_3_hurdle_interact, x_var = "pcl_total_x_mios_total")
plot_marginal_effects(model_3_hurdle_interact, x_var = "pcl_total_x_mios_total", dpar = "mu")
plot_marginal_effects(model_3_hurdle_interact, x_var = "pcl_total_x_mios_total", dpar = "hu")





