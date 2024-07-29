
# BIVARIATE

model_bi <- 
  brm(bipf_category ~ 
        mios_scaled,
      data = data_scaled,
      prior(cauchy(0, 2.5), class = 'b'),
      family = cumulative(threshold = "flexible"),
      cores = 4)

#brms::get_prior(model_bi)
#pp_check(model_bi, type = 'bars')
#summary(model_bi)

