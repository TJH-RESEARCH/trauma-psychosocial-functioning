
# Plot Prior Distributions

n <- 1e5

# Generate Distributions -------------------------------------------------------------------
## Generate random distributions and save in a table
priors <-
  tibble(
    
    # Non-zero (nz) process -------------------------------------------------------------------
    
    ## Weakly informative priors
    nz_coef_weak = rnorm(n, mean = 0, sd = 2),
    nz_intercept_weak = rnorm(n, mean = 2.3, sd = 1),
    
    ## Vague priors
    nz_coef_vague = rnorm(n, mean = 0, sd = 10),
    nz_intercept_vauge = rnorm(n, mean = 2.3, sd = 10),
    
    ## Informative priors
    nz_coef_inform = rnorm(n, mean = 0, sd = 2),
    nz_intercept_inform = rnorm(n, mean = 2.3, sd = 1),
    
    
    # Zero process (zp) -------------------------------------------------------------------
    
    ## Weakly informative priors
    zp_coef_weak = rnorm(n, mean = 0, sd = 1),
    zp_intercept_weak = rnorm(n, mean = 0, sd = 1),
    
    ## Vauge priors
    zp_coef_vague = rnorm(n, mean = 0, sd = 10),
    zp_intercept_vague = rnorm(n, mean = 0, sd = 10),
    
    ## Informative priors
    zp_coef_inform = rnorm(n, mean = 0, sd = 1),
    zp_intercept_inform = rnorm(n, mean = 0, sd = 1),
    
    
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
priors %>% 
  select(contains("coef")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value)) +
  geom_density(fill = 'red', alpha = .25) +
  lims(x = c(-20, 20)) +
  facet_wrap(~name)

## Plot Non-zero Process Intercept Prior
priors %>% 
  select(contains("intercept")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value)) +
  geom_density(fill = 'green', alpha = .25) +
  lims(x = c(-20, 20)) +
  facet_wrap(~name)


## Plot Shape Parameter Prior... not much to see hear
priors %>% 
  select(contains("shape")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value)) +
  geom_density(fill = 'blue', alpha = .25) +
  #lims(x = c(-100, 100)) +
  facet_wrap(~name)



