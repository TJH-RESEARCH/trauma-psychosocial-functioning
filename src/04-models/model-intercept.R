
m_intercept <- 
  brm(bipf_category ~ 1,
      data = data_scaled,
      family = cumulative(threshold = "flexible"),
      cores = 4)

summary(m_intercept)
get_prior(m_intercept)