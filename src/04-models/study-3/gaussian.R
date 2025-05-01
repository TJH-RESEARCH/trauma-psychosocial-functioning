
library(brms)
library(tidybayes)


gaussian_weakly_interaction <- 
  brm(
    bf(bipf_score ~ pcl_total * mios_total),
    data = data_baked_3,
    family = gaussian(),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b, coef = pcl_total),
      prior(student_t(5, 0, 1.75), class = b, coef = mios_total),
      prior(student_t(5, 5, 1.75), class = b, coef = pcl_total:mios_total),
      prior(student_t(5, 0, 1.75), class = Intercept)
    ),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )

gaussian_weakly_interaction


## Interaction term
gaussian_weakly_interaction %>% 
  tidybayes::tidy_draws() %>% 
  ggplot(aes(`b_pcl_total:mios_total`)) +
  geom_density(size = 0, fill = 'red', alpha = .5)
  

## R-squared
bayes_R2(gaussian_weakly_interaction,summary = TRUE)

tibble(
    r2 = data.frame(bayes_R2(gaussian_weakly_interaction,summary = FALSE))$R2  
  ) %>% 
  ggplot(aes(r2)) +
  geom_density(size = 0, fill = 'red', alpha = .5) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:1) +
  xlab(expression(italic(R)^{2}))



# No Interaction ----------------------------------------------------------


gaussian_weakly_no_interaction <- 
  brm(
    bf(bipf_score ~ pcl_total + mios_total),
    data = data_baked_3,
    family = gaussian(),
    prior = c(
      prior(student_t(5, 0, 1.75), class = b, coef = pcl_total),
      prior(student_t(5, 0, 1.75), class = b, coef = mios_total),
      prior(student_t(5, 0, 1.75), class = Intercept)
    ),
    sample_prior = 'yes',
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED # Stan options
  )

gaussian_weakly_no_interaction


# R-squared
bayes_R2(gaussian_weakly_no_interaction,summary = TRUE)

tibble(
  r2 = data.frame(bayes_R2(gaussian_weakly_no_interaction, summary = FALSE))$R2  
) %>% 
  ggplot(aes(r2)) +
  geom_density(size = 0, fill = 'red', alpha = .5) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:1) +
  xlab(expression(italic(R)^{2}))

