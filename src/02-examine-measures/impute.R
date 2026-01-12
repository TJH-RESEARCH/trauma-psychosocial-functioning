# Impute Sample 3 MIOS Item #9

## MIOS Item 9 was erroneously not collected for Sample 3. 
## The internal consistency for the scale is high, so we'll impute the value as 
## the within person mean across the other 13 MIOS items. 
## This will facilitate between sample comparison of the distributions of the MIOS, 
## although sense we are scaling the MIOS score before modeling, 
## the imputed value shouldn't have an impact on the statistical models. 

data_3 <-
  data_3 %>%  
  rowwise() %>% ## for the within person means
  mutate(
         ## Calculate the within person mean
         mios_9_imputed = 
           mean(mios_1, mios_2, mios_3, mios_4, mios_5, mios_6, mios_7, mios_8, 
                mios_10, mios_11, mios_12, mios_13, mios_14),
         
         ## Save a copy of the original score
         mios_total_original = mios_total,
         
         # Overwrite the original value with the imputed value
         mios_total = mios_total + mios_9_imputed
         ) %>% 
  ungroup()
  