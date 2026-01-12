# Declare different kinds of prior distributions to use in the models


# Weakly Informative Priors -----------------------------------------------
weakly_informative_priors = 
  c(
    # for the non-zero process, in log for 0-100 outcome
    prior(normal(0, 2), class = b),
    prior(normal(2.3, 1), class = Intercept), # baseline around 10 on the 0-100 scale
    
    # for the zero process: probability of being 0. Log odds intercept, log variables, for binary outcome
    prior(normal(0, 1), class = b, dpar = hu),          # logistic coefficients
    prior(normal(0, 1), class = Intercept, dpar = hu),  # hurdle intercept. 
    
    # Shape
    prior(lognormal(log(20), 0.5), class = shape)
  )


# Vague Priors ------------------------------------------------------------
vague_priors = 
  c(
    # for the non-zero process, in log for 0-100 outcome
    prior(normal(0, 10), class = b),
    prior(normal(2.3, 10), class = Intercept), # baseline around 10 on the 0-100 scale
    
    # for the zero process: probability of being 0. Log odds intercept, log variables, for binary outcome
    prior(normal(0, 10), class = Intercept, dpar = hu),  # hurdle intercept. 
    prior(normal(0, 10), class = b, dpar = hu),          # logistic coefficients
    
    # Shape
    prior(lognormal(log(20), 10), class = shape)
  )


# Informative Priors ------------------------------------------------------
informative_priors = 
  c(
    # for the non-zero process, in log for 0-100 outcome
    prior(normal(0, 2), class = b),
    prior(normal(2.3, 1), class = Intercept), # baseline around 10 on the 0-100 scale
    
    # for the zero process: probability of being 0. Log odds intercept, log variables, for binary outcome
    prior(normal(0, 1), class = b, dpar = hu),          # logistic coefficients
    prior(normal(0, 1), class = Intercept, dpar = hu),  # hurdle intercept. 
    
    # Shape
    prior(lognormal(log(20), 0.5), class = shape)
  )
