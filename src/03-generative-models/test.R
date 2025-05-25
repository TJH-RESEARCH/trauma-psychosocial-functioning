library(brms)
library(tidybayes)

data("starwars")
starwars %>% names()

# Hurdle gamma model -----------------------------------------------------------
model_test <- 
  brm(
    bf(mass ~ height,
       hu ~ height),
    data = starwars,
    family = hurdle_gamma(),
    prior = c(
      # for the linear part
      prior(normal(10, 100), class = b),
      prior(normal(100, 100), class = Intercept),
      
      # for the logistic part: probability of being 0. 
      prior(normal(0, 1), class = Intercept, dpar = hu), # hurdle intercept. 
      prior(normal(0, 1), class = b, dpar = hu), # logistic coefficients
      prior(logistic(0, 1), class = shape)
    ),
    sample_prior = 'no',
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr"
  )


# Get Draws ---------------------------------------------------------------
model_test %>% tidy()
draws_test <- model_test %>% tidy_draws()


# Check Rhat and Effective Sample Size -----------------------------------------
draws_hurdle_1 %>% count(divergent__)
draws_hurdle_1 %>% count(n_leapfrog__)
draws_hurdle_1 %>% count(stepsize__)
draws_hurdle_1 %>% ggplot(aes(accept_stat__)) + geom_density()
bayestestR::effective_sample(model_1_hurdle)
bayestestR::diagnostic_draws(model_1_hurdle)


broom.mixed::tidy(model_1_hurdle)





