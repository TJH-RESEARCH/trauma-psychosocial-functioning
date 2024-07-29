


# SCALE INPUT DATA --------------------------------------------------------


data_scaled <-
  data %>% 
  select(
    military_exp_combat,
    pc_ptsd_positive_screen,
    # MOS,
    service_era_post_911,
    service_era_persian_gulf,
    sex_female,
    race_black,
    race_white) %>% 
  transmute(across(everything(), ~ .x - mean(.x), .names = "{.col}_scaled")) %>% 
  bind_cols(
    data %>% transmute(mios_scaled = (mios_total - mean(mios_total)) / sd(mios_total) / 2),
    data %>% select(bipf_category)
  )


  




  