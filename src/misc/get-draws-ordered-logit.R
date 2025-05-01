


draws_hurdle_1 <-
  model_hurdle_1 %>% 
  spread_draws(
    b_Intercept[1:4], 
    b_mios_scaled, 
    b_pc_ptsd_positive_screen_scaled, 
    b_service_era_post_911_scaled, 
    b_service_era_persian_gulf_scaled,
    b_sex_female_scaled, 
    b_race_black_scaled,
    b_race_white_scaled) %>% 
  pivot_wider(values_from = b_Intercept, 
              names_from = `1:4`, 
              names_prefix = 'intercept_')
