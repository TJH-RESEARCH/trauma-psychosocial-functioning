
# Plot Prior Distributions

n <- 1e5

# Data Set 1 -------------------------------------------------------------------
## Generate random distributions and save in a table
priors <-
  tibble(
    normal07 = rnorm(n, mean = 0, sd = .7),
    normal05 = rnorm(n, mean = 0, sd = .5),
    logistic = rlogis(n, 0, 1),
    gamma = rgamma(n, .01, .01)
  ) 
  

## Plot Beta (Linear Predictor) Parameter Prior: Normal
priors %>% 
  ggplot(aes(normal07)) +
  geom_density(fill = 'red', alpha = .25) +
  lims(x = c(-5, 5))

## Plot Hurdle Intercept Prior
priors %>% 
  ggplot(aes(normal05)) +
  geom_density(fill = 'blue', alpha = .25) +
  lims(x = c(-5, 5))


## Plot Hurdle Shape Prior
priors %>% 
  ggplot(aes(logistic)) +
  geom_density(fill = 'green', alpha = .25)


## Plot Hurdle Shape Prior: Default
priors %>% 
  ggplot(aes(gamma)) +
  geom_density(fill = 'purple', alpha = .25)





