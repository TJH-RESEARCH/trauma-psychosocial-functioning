# Plot the raw Prior Distributions to understand them

n <- 1e5

# Generate Distributions -------------------------------------------------------------------
## Generate random distributions and save in a table
priors <-
  tibble(
    
    # Non-zero (nz) process -------------------------------------------------------------------
    
    ## Weakly informative priors
    nz_coef_Weak = rnorm(n, mean = 0, sd = 2),
    nz_intercept_Weak = rnorm(n, mean = 2.3, sd = 1),
    
    ## Vague priors
    nz_coef_Vague = rnorm(n, mean = 0, sd = 10),
    nz_intercept_Vauge = rnorm(n, mean = 2.3, sd = 10),
    
    ## Informative priors
    nz_coef_Informative = rnorm(n, mean = 0, sd = 2),
    nz_intercept_Informative = rnorm(n, mean = 2.3, sd = 1),
    
    
    # Zero process (zp) -------------------------------------------------------------------
    
    ## Weakly informative priors
    zp_coef_Weak = rnorm(n, mean = 0, sd = 1),
    zp_intercept_Weak = rnorm(n, mean = 0, sd = 1),
    
    ## Vauge priors
    zp_coef_Vague = rnorm(n, mean = 0, sd = 10),
    zp_intercept_Vague = rnorm(n, mean = 0, sd = 10),
    
    ## Informative priors
    zp_coef_Informative = rnorm(n, mean = 0, sd = 1),
    zp_intercept_Informative = rnorm(n, mean = 0, sd = 1),
    
    
    # Shape -------------------------------------------------------------------
    
    ## Weakly informative priors
    shape_weak = rlnorm(n = n, meanlog = 20, sdlog = 0.5),
    
    ## Vauge priors
    shape_vague = rlnorm(n = n, meanlog = 20, sdlog = 10),
    
    ## Informative priors
    shape_inform = rlnorm(n = n, meanlog = 20, sdlog = 0.5)
  ) 
  


# Plot --------------------------------------------------------------------

## Plot Non-zero Process Coefficient Prior
plot_priors_nonzero <-
  priors %>% 
  select(contains("coef")) %>% 
  pivot_longer(everything()) %>% 
  mutate(name = str_replace(name, "nz_coef_", "Non-Zero "),
         name = str_replace(name, "zp_coef_", "Zero-Process ")) %>% 
  ggplot(aes(value)) +
  geom_density(fill = colors_tam[4], linewidth = .2, alpha = .25) +
  labs(x = NULL, y = "Density", subtitle = "Coefficients") +
  lims(x = c(-20, 20)) +
  facet_wrap(~name) +
  theme_scatter

## Plot Non-zero Process Intercept Prior
plot_priors_zero <-
  priors %>% 
  select(contains("intercept")) %>% 
  pivot_longer(everything()) %>% 
  mutate(name = str_replace(name, "nz_intercept_", "Non-Zero "),
         name = str_replace(name, "zp_intercept_", "Zero-Process ")) %>% 
  ggplot(aes(value)) +
  geom_density(fill = colors_tam[7], linewidth = .2, alpha = .25) +
  labs(x = NULL, y = "Density", subtitle = "Intercepts") +
  lims(x = c(-20, 20)) +
  facet_wrap(~name) +
  theme_scatter


## Plot Shape Parameter Prior... not much to see here. this is just a meaningless term that is needed to make gammar regression work
plot_priors_shape <-
  priors %>% 
  select(contains("shape")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value)) +
  geom_density(fill = colors_tam[4], color = colors_tam[4], linewidth = .5, alpha = .25) +
  labs(x = NULL, y = "Density") +
  facet_wrap(~name) +
  theme_scatter


# Combine the plots ------------------------------------------------------------
plot_priors_combo <-
  plot_priors_nonzero / plot_priors_zero + plot_layout(axis_titles = "collect")


# Print to console -------------------------------------------------------------
plot_priors_combo %>% print()
plot_priors_shape %>% print()

# Save to file -----------------------------------------------------------------
ggsave(plot = plot_priors_combo, file = here::here("output/priors/plot-priors.jpg"), width = 6, height = 8)
ggsave(plot = plot_priors_shape, file = here::here("output/priors/plot-priors-shape.jpg"), width = 6, height = 4)

# Remove objects from environment -----------------------------------------
rm(plot_priors_combo, plot_priors_shape, plot_priors_nonzero, 
   plot_priors_zero, n, priors)

