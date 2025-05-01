
# Plot Prior Distributions
  
# Data Set 1 -------------------------------------------------------------------
## Generate random distributions and save in a table
priors <-
  tibble(
    student_t = rstudent_t(n = 1e5, mu = 0, df = 5, sigma = 1.75),
    beta = rbeta(n = 1e5, shape1 = 1, shape2 = 1.25),
    shape = rgamma(n = 1e5, 2, 1),
    id = rep(1, 1e5)
  ) 
  

## Plot Beta (Linear Predictor) Parameter Prior
priors %>% 
  ggplot(aes(student_t)) +
  geom_density(fill = 'red', alpha = .25) +
  lims(x = c(-10, 10))

## Plot Pi (Logistic Regression) Prior
priors %>% 
  ggplot(aes(beta)) +
  geom_density(fill = 'green', alpha = .25) +
  lims(x = c(-.5, 1.5))

## Plot Gamma Shape Parameter Prior
priors %>% 
  ggplot(aes(shape)) +
  geom_density(fill = 'blue', alpha = .25) +
  lims(x = c(-5, 10))





