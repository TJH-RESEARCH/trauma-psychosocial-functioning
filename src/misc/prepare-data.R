data_list <- 
  data %>% 
  select(bipf_category) %>% 
  tidybayes::compose_data()

data_list$x <-
  with(data,
       model.matrix(
         bipf_category ~ 
           mios_total + 
           military_exp_combat +
           pc_ptsd_positive_screen +
           # MOS +
           service_era_post_911 + 
           service_era_persian_gulf +
           sex_female +
           race_black +
           race_white
       )[,-1]
  )

data_list$n_predictors <- ncol(data_list$x)
data_list %>% str()