
library(brms)

# BIVARIATE - PRIOR
model_bi_prior <- 
  brm(bipf_category ~ 
        mios_scaled,
      data = data_scaled,
      prior(cauchy(0, 2.5), class = 'b'),
      family = cumulative(threshold = "flexible"),
      sample_prior = "only",
      cores = 4)

#brms::get_prior(model_bi_prior)
#pp_check(model_bi_prior, type = 'bars')
#summary(model_bi_prior)
