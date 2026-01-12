# Scatter plots between Moral Injury and Posttraumatic Symptoms


# Study 1 -----------------------------------------------------------------
plot_bivariate_1 <-
  data_1 %>% 
    ggplot(aes(pcl_total, bipf_score)) + 
    geom_smooth(se = FALSE, linewidth = 1, span = 1, color = "black", fill = "black") +
    geom_point(size = 2, alpha = .8,  shape = 1, fill = "#343434", position = 'jitter') +
    labs(x = "PTSD", y = "Difficulty", subtitle = 'Study 1') +
    lims(y = c(-1,101), x = c(0,80)) +
    theme_scatter
plot_bivariate_1 %>% print()

# Study 2 -----------------------------------------------------------------
plot_bivariate_2 <-
  data_2 %>% 
    mutate(pc_ptsd_positive_screen = factor(pc_ptsd_positive_screen, levels = c(0,1), labels = c('No', 'Yes'))) %>% 
    ggplot(aes(mios_total, bipf_score, fill = pc_ptsd_positive_screen)) + 
    geom_smooth(se = FALSE, linewidth = 1, span = 1, color = "black", fill = "black") +
    geom_point(size = 2, alpha = .8,  shape = 21, position = 'jitter') +
    labs(x = "Moral Injury", y = "Difficulty", subtitle = 'Study 2', fill = 'PTSD Detected') +
    lims(y = c(-1,101), x = c(0,60)) +
    scale_fill_discrete(palette = c("white", "black")) +
    theme_scatter +
    theme(legend.position = 'bottom')

plot_bivariate_2 %>% print()


# Study 3 -----------------------------------------------------------------

## MIOS on x axis, PCL as color
plot_bivariate_3a <-
  data_3 %>% 
    ggplot(aes(mios_total, bipf_score, fill = pcl_total)) + 
    geom_smooth(se = FALSE, linewidth = 1, span = 1, color = "black", fill = "black") +
    geom_point(size = 2, alpha = .8, shape = 21, position = 'jitter') +
    labs(x = "Moral Injury", y = "Difficulty", fill = 'PTSD', subtitle = 'Study 3') +
    scale_fill_gradient(low = "white", high = "black") +  
    #scale_fill_met_c('VanGogh3') +
    theme_scatter +
    lims(y = c(-1,101), x = c(0,60)) +
    theme(legend.position = 'bottom')

plot_bivariate_3a %>% print()


## PCL on x axis, MIOS as color
plot_bivariate_3b <-
  data_3 %>% 
    ggplot(aes(pcl_total, bipf_score, fill = mios_total)) + 
    geom_smooth(se = FALSE, linewidth = 1, span = 1, color = "black", fill = "black") +
    geom_point(size = 2, alpha = .8, shape = 21, position = 'jitter') +
    labs(x = "PTSD", y = "Difficulty", fill = 'Moral Injury', subtitle = 'Study 3') +
    scale_fill_gradient(low = "white", high = "black") +  
    theme_scatter +
    lims(y = c(-1,101), x = c(0,80)) +
    theme(legend.position = 'bottom')

plot_bivariate_3b %>% print()



# Combine Plots -----------------------------------------------------------
plot_bivariate <-
(plot_bivariate_1 + plot_bivariate_2) /
(plot_bivariate_3b + plot_bivariate_3a)

# Write to file
ggsave(here::here("output/plot-bivariate.jpg"), width = 6, height = 8)

