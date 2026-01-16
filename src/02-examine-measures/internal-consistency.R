# Calculate the internal consistency for the measures


# Specify and fit the Measurement Models ---------------------------------------
## PCL -------------------------------------------------------------------------
### Specification
model_pcl <- 
  'pcl =~ pcl_1 + pcl_2 + pcl_3 + pcl_4 + pcl_5 + pcl_6 + pcl_7 + pcl_8 + pcl_9 + pcl_10 + pcl_11 + pcl_12 + pcl_13 + pcl_14 + pcl_15 + pcl_16 + pcl_17 + pcl_18 + pcl_19 + pcl_20'

### Fit
fit_pcl_1 <- lavaan::cfa(model_pcl, data_1, std.lv = F, ordered = F, estimator = 'MLR')
fit_pcl_3 <- lavaan::cfa(model_pcl, data_3, std.lv = F, ordered = F, estimator = 'MLR')


## MIOS ------------------------------------------------------------------------
### Sample 2 used the enitre MIOS
model_mios <-   'mios =~ mios_1 + mios_2 + mios_3 + mios_4 + mios_5 + mios_6 + mios_7 + mios_8 + mios_9 + mios_10 + mios_11 + mios_12 + mios_13 + mios_14'

### However, most results for Sample 3 were missing MIOS Iten #9 due to an error when creating the survey
model_mios_3 <- 'mios =~ mios_1 + mios_2 + mios_3 + mios_4 + mios_5 + mios_6 + mios_7 + mios_8 +          mios_10 + mios_11 + mios_12 + mios_13 + mios_14'

### Fit
fit_mios_2 <- lavaan::cfa(model_mios, data_2, std.lv = F, ordered = F, estimator = 'MLR')
fit_mios_3 <- lavaan::cfa(model_mios_3, data_3, std.lv = F, ordered = F, estimator = 'MLR')

# BIPF --------------------------------------------------------------------
### Specification
model_bipf <- 
  'bipf =~ bipf_children + bipf_daily + bipf_education + bipf_family + bipf_friends + bipf_spouse + bipf_work'

### Fit
fit_bipf_1 <- lavaan::cfa(model_bipf, data_1, std.lv = F, ordered = F, estimator = 'MLR')
fit_bipf_2 <- lavaan::cfa(model_bipf, data_2, std.lv = F, ordered = F, estimator = 'MLR')
fit_bipf_3 <- lavaan::cfa(model_bipf, data_3, std.lv = F, ordered = F, estimator = 'MLR')


# RESULTS ----------------------------------------------------------------------
## Create a table with the internal consistency results
internal_consistency <-
  tibble(
    variable = 
      c(
        rep('Moral Injury Symptoms', 3),
        rep('PTSD Symptoms', 3),
        rep('Psychosocial Functioning', 3)
    ),
    
    measure = 
      c(
        rep('Moral Injury Outcomes Scale (MIOS)', 3),
        rep('PTSD Checklist for DSM-5 (PCL-5)', 3),
        rep('Brief Inventory of Psychosocial Functioning (b-IPF)', 3)
      ),
    
    sample =
      rep(c(1,2,3), 3),
    
    omega = 
      c(
        NA, # no mios for study 1
        fit_mios_2 %>% semTools::compRelSEM() %>% as.numeric(),
        fit_mios_3 %>% semTools::compRelSEM() %>% as.numeric(),
        fit_pcl_1 %>% semTools::compRelSEM() %>% as.numeric(),
        
        NA, # no pcl for study 2
        fit_pcl_3 %>% semTools::compRelSEM() %>% as.numeric(),
        
        fit_bipf_1 %>% semTools::compRelSEM() %>% as.numeric(),
        fit_bipf_2 %>% semTools::compRelSEM() %>% as.numeric(),
        fit_bipf_3 %>% semTools::compRelSEM() %>% as.numeric()
        
      )
  )

# Print to console -------------------------------------------------------------
internal_consistency %>% 
  pivot_wider(names_from = sample, values_from = omega) |> 
  rename(`Sample 1` = `1`, `Sample 2` = `2`, `Sample 3` = `3`) |> 
  print(n = 100)

# Save to file  ----------------------------------------------------------------
internal_consistency %>% write_csv(here::here('output/internal-consistency.csv'))


# Remove objects from environment -----------------------------------------
rm(internal_consistency, model_pcl, model_bipf, model_mios, model_mios_3,
   fit_pcl_1, fit_pcl_3, fit_bipf_1, fit_bipf_2, fit_bipf_3, 
   fit_mios_2, fit_mios_3)


