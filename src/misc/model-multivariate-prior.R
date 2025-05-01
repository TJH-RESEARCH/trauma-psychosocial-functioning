
# MULTIVARIATE - PRIOR
model_multi_prior <- brm(bipf_category ~ 
                           mios_scaled +
                           pc_ptsd_positive_screen_scaled + 
                           service_era_post_911_scaled + 
                           service_era_persian_gulf_scaled + 
                           sex_female_scaled +
                           race_black_scaled + 
                           race_white_scaled,
                         data = data_scaled,
                         prior(cauchy(0, 2.5), class = 'b'),
                         family = cumulative(threshold = "flexible"),
                         sample_prior = "only",
                         cores = 4)

# cauchy(0, 1) not good
# cauchy(0, 2.5) not as good as # cauchy(0, 2.5)

#brms::get_prior(model_multi_prior)
#pp_check(model_multi_prior, type = 'bars')
#pp_check(model_multi_prior, type = 'dens')
#pp_check(model_multi_prior, type = 'hist')
#summary(model_multi_prior)