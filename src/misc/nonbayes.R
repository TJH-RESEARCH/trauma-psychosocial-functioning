library(ordinal)
source(here::here('src/02-pre-processing/prepare-data.R'))
source(here::here('src/02-pre-processing/scale-predictors.R'))
m_clm <-
  ordinal::clm(bipf_category ~ 
      mios_scaled +
      pc_ptsd_positive_screen_scaled + 
      service_era_post_911_scaled + 
      service_era_persian_gulf_scaled + 
      sex_female_scaled +
      race_black_scaled + 
      race_white_scaled,
    data = data_scaled)

summary(m_clm)

