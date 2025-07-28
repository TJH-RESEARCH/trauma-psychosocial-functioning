library(MetBrewer)

# Colors from MetBrewer
color_palette <- MetBrewer::met.brewer("Paquin")


# Study 1 -----------------------------------------------------------------
plot_bivariate_1 <-
  data_1 %>% 
    ggplot(aes(pcl_total, bipf_score)) + 
    geom_smooth(se = TRUE, linewidth = 1.5, span = 1, color = color_palette[10], fill = color_palette[8]) +
    geom_point(size = 2, alpha = .8,  shape = 21, fill = color_palette[7], position = 'jitter') +
    labs(x = "PTSD", y = "Difficulty", title = 'Study 1') +
    lims(y = c(-1,101)) +
    theme_custom

plot_bivariate_1 |> print()

# Study 2 -----------------------------------------------------------------
plot_bivariate_2 <-
  data_2 %>% 
    mutate(pc_ptsd_positive_screen = factor(pc_ptsd_positive_screen, levels = c(0,1), labels = c('No', 'Yes'))) |> 
    ggplot(aes(mios_total, bipf_score, fill = pc_ptsd_positive_screen)) + 
    geom_smooth(se = TRUE, linewidth = 1.5, span = 1, color = color_palette[10], fill = color_palette[8]) +
    geom_point(size = 2, alpha = .8,  shape = 21, position = 'jitter') +
    labs(x = "Moral Injury", y = "Difficulty", title = 'Study 2', fill = 'PTSD Detected') +
    lims(y = c(-1,101)) +
    scale_fill_met_d('VanGogh3') +
    theme_custom

plot_bivariate_2 |> print()


# Study 3 -----------------------------------------------------------------

## MIOS on x axis, PCL as color
plot_bivariate_3a <-
  data_3 %>% 
    ggplot(aes(mios_total, bipf_score, fill = pcl_total)) + 
    geom_smooth(se = TRUE, linewidth = 1.5, span = 1, color = color_palette[10], fill = color_palette[8]) +
    geom_point(size = 2, alpha = .8, shape = 21, position = 'jitter') +
    labs(x = "Moral Injury", y = "Difficulty", fill = 'PTSD', title = 'Study 3') +
    scale_fill_met_c('VanGogh3') +
    theme_custom +
    lims(y = c(-1,101)) +
    theme(legend.position = 'bottom')

plot_bivariate_3a |> print()


## PCL on x axis, MIOS as color
plot_bivariate_3b <-
  data_3 %>% 
    ggplot(aes(pcl_total, bipf_score, fill = mios_total)) + 
    geom_smooth(se = TRUE, linewidth = 1.5, span = 1, color = color_palette[10], fill = color_palette[8]) +
    geom_point(size = 2, alpha = .8, shape = 21, position = 'jitter') +
    labs(x = "PTSD", y = "Difficulty", fill = 'Moral Injury', title = 'Study 3') +
    scale_fill_met_c('VanGogh3') +
    theme_custom +
    lims(y = c(-1,101)) +
    theme(legend.position = 'bottom')

plot_bivariate_3b |> print()
