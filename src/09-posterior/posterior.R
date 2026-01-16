# Posterior Distribution





# Helper Function ---------------------------------------------------------
## This function plots the conditional_effects for a given model

plot_posterior <- 
  function(model, x_var = 'NULL', dpar = "NULL"){
    
    # Deparse the model name to pass to get_model_labels()
    model_name <- deparse(substitute(model))
    
    # Get the model labels using a helper function
    model_labels <- get_model_labels(model_name)
    
    # Set the X variable depending on the model
    
    x_lab <- 
      case_when(
        x_var == "pcl_total" & dpar == "hu" ~ 'Odds Ratio\n(Posttraumatic Symptoms)',
        x_var == "pcl_total" & dpar == "mu" ~ 'Gamma Coefficient\n(Posttraumatic Symptoms)',
        x_var == "mios_total" & dpar == "hu" ~ 'Odds Ratio\n(Moral Injury Symptoms)',
        x_var == "mios_total" & dpar == "mu" ~ 'Gamma Coefficient\n(Moral Injury Symptoms)',
        x_var == "pcl_total_x_mios_total" & dpar == "hu" ~ 'Odds Ratio\n(Interaction PCL * MIOS)',
        x_var == "pcl_total_x_mios_total" & dpar == "mu" ~ 'Gamma Coefficient\n(Interaction PCL * MIOS)',
        
        .default = NA
      )
    
    
    # Set the X limits depending on the X variable
    
    x_limits <- 
      case_when(
        str_detect(x_lab, "Odds Ratio") ~ c(-.1,2),
        str_detect(x_lab, "Gamma") ~ c(.5, 2.5),
        .default = NA
      )
    
    x_breaks <- 
      case_when(
        str_detect(x_lab, "Odds Ratio") ~ seq(0, 2, .5),
        str_detect(x_lab, "Gamma") ~ seq(.5, 2.5, .5),
        .default = NA
      )
    
    
    title <- paste(model_labels$model_num, "-", model_labels$model_type, "-", model_labels$model_controls, "-", model_labels$model_prior)
    
    subtitle <-
      case_when(
        dpar == "NULL" ~ "Combined Model (Hurdle + Gamma)",
        dpar == "hu" ~ "Hurdle Model",
        dpar == "mu"  ~ "Gamma Model",
        .default = NA
      )
    
    color_palette <-
      case_when(
        dpar == "hu" ~ c("#72aeb6", "#4692b0"),
        dpar == "mu"  ~ c("#ffb242", "#ef8737"),
        .default = NA
      )
    
    # match X variable to tidy_draws_syntax
    x_var <-
      case_when(
        dpar == "hu" & x_var == "pcl_total" ~ "b_hu_pcl_total",
        dpar == "hu" & x_var == "mios_total" ~ "b_hu_mios_total",
        dpar == "hu" & x_var == "pcl_total_x_mios_total" ~ "b_hu_pcl_total_x_mios_total",
        dpar == "mu" & x_var == "pcl_total" ~ "b_pcl_total",
        dpar == "mu" & x_var == "mios_total" ~ "b_mios_total",
        dpar == "mu" & x_var == "pcl_total_x_mios_total" ~ "b_pcl_total_x_mios_total",
        .default = NA
      )
    
    
    plot <-
      model %>% 
      tidy_draws() %>%
      select(.chain, .iteration, .draw, x_var) %>% 
      pivot_longer(-c(.chain, .iteration, .draw)) %>% 
      # tranform from log scale to something easier to understand:
      mutate(value = exp(value)) %>% 
      ggplot(aes(x = value)) + 
      stat_slab(point_interval = mode_hdci, 
                aes(fill = after_stat(level)), color = "black", linewidth = .1, alpha = 1, .width = c(.95, 1)) +
      stat_spike(at = "Mode", linetype = "dotted", color =  "black", linewidth = .35) +
      stat_spike(
        at = function(x) hdci(x, .width = .95),
        size = 0, color =  "#3e3e3e", linewidth = 0.35, linetype = "dotted"
      ) +
      #tidybayes::stat_halfeye(fill = MetPalettes$Peru1[[1]][5]) +
      labs(y = "Posterior Density", 
           x = x_lab,
           title = title, 
           subtitle = paste0("<span style = 'color:#0a3351'> **Posterior distribution**</span> (", model_labels$model_num, ")")
      ) + 
      theme_custom +
      scale_fill_manual(values = color_palette) +
      scale_x_continuous(breaks = x_breaks, limits = x_limits) +
      scale_thickness_shared()
    
    
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
    
    
    path <- here::here(paste0("output/posterior/plot-posterior-", str_remove(model_labels$model_num, "Sample "), "-", str_to_lower(model_labels$model_type), "-", str_to_lower(model_labels$model_controls),"-", str_to_lower(model_labels$model_prior), dpar_lab, x_var_lab, ".jpg"))
    
    ggsave(plot = plot, file = path, width = 6, height = 4)
    
    return(plot)
  }



# Save a plot for each model ----------------------------------------------------------

## Sample 1
plot_posterior(model_1_hurdle, x_var = "pcl_total", dpar = "hu")
plot_posterior(model_1_hurdle, x_var = "pcl_total", dpar = "mu")
plot_posterior(model_1_hurdle_bi, x_var = "pcl_total", dpar = "hu")
plot_posterior(model_1_hurdle_bi, x_var = "pcl_total", dpar = "mu")

## Sample 2
plot_posterior(model_2_hurdle, x_var = "mios_total", dpar = "hu")
plot_posterior(model_2_hurdle, x_var = "mios_total", dpar = "mu")


## Sample 3, Adjusted
plot_posterior(model_3_hurdle_adjust, x_var = "mios_total", dpar = "mu")
plot_posterior(model_3_hurdle_adjust, x_var = "mios_total", dpar = "hu")
plot_posterior(model_3_hurdle_adjust, x_var = "pcl_total", dpar = "mu")
plot_posterior(model_3_hurdle_adjust, x_var = "pcl_total", dpar = "hu")

## Sample 3, MIOS
plot_posterior(model_3_hurdle_mios, x_var = "mios_total", dpar = "mu")
plot_posterior(model_3_hurdle_mios, x_var = "mios_total", dpar = "hu")

## Sample 3, PCL
plot_posterior(model_3_hurdle_pcl, x_var = "pcl_total", dpar = "mu")
plot_posterior(model_3_hurdle_pcl, x_var = "pcl_total", dpar = "mu")

## Sample 3, Interaction
plot_posterior(model_3_hurdle_interact, x_var = "pcl_total_x_mios_total", dpar = "mu")
plot_posterior(model_3_hurdle_interact, x_var = "pcl_total_x_mios_total", dpar = "hu")





