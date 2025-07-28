
# MIOS Item 9 was erroneously not collected for Data Set 3. 
## The internal consistency for the scale is high, so impute the value as the
## within person mean. 
## This will facilitate comparison of the distributions of the MIOS, 
## although sense we are scaling the data before modeling, it shouldn't
## have an impact on the statistical models. 

data_3 <-
data_3 |> 
  rowwise() |> 
  mutate(mios_9_imputed = 
           mean(mios_1, mios_2, mios_3, mios_4, mios_5, mios_6, mios_7, mios_8, 
                mios_10, mios_11, mios_12, mios_13, mios_14),
         mios_total_imputed = mios_total + mios_9_imputed
         ) |> 
  ungroup()
  