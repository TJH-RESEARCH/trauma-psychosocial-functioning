
# MULTIVARIATE - PRIOR
model_multi <- brm(bipf_category ~ 
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
                   cores = 4)

#brms::get_prior(model_multi)
#pp_check(model_multi, type = 'bars')
#pp_check(model_multi, type = 'dens')
#pp_check(model_multi, type = 'hist')

#summary(model_multi)

#get_variables(model_multi)
#brms::stancode(model_multi)

