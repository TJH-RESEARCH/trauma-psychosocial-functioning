library(tidymodels)
library(bayesian)
library(rstan)
library(tidybayes)
library(brms)


# A. RECIPES ----------------------------------------------------------------------


### Total bIPF -----------------------------------------------------------------

recipe_total <-
  recipes::recipe(bipf_category ~ 
                    mios_total +
                    pc_ptsd_positive_screen + 
                    military_exp_combat +
                    #MOS +
                    service_era_post_911 +
                    service_era_persian_gulf +
                    sex_male +
                    race_black +
                    race_white,
                  data = data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(mios_total, factor = 2)

recipe_total %>% prep(., data) 

data_baked_total <- recipe_total %>% prep(., data) %>% bake(., NULL)


### 1. bIPF Children -----------------------------------------------------------
recipe_children <-
  recipes::recipe(bipf_children ~ 
                    mios_total +
                    pc_ptsd_positive_screen + 
                    military_exp_combat +
                    #MOS +
                    service_era_post_911 +
                    service_era_persian_gulf +
                    sex_male +
                    race_black +
                    race_white,
                  data = data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(mios_total, factor = 2)

recipe_children %>% prep(., data) 

data_baked_children <- recipe_children %>% prep(., data) %>% bake(., NULL)


### 2. bIPF Daily --------------------------------------------------------------
recipe_daily <-
  recipes::recipe(bipf_daily ~ 
                    mios_total +
                    pc_ptsd_positive_screen + 
                    military_exp_combat +
                    #MOS +
                    service_era_post_911 +
                    service_era_persian_gulf +
                    sex_male +
                    race_black +
                    race_white,
                  data = data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(mios_total, factor = 2)

recipe_daily %>% prep(., data) 

data_baked_daily <- recipe_daily %>% prep(., data) %>% bake(., NULL)


### 3. bIPF Education ----------------------------------------------------------
recipe_education <-
  recipes::recipe(bipf_education ~ 
                    mios_total +
                    pc_ptsd_positive_screen + 
                    military_exp_combat +
                    #MOS +
                    service_era_post_911 +
                    service_era_persian_gulf +
                    sex_male +
                    race_black +
                    race_white,
                  data = data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(mios_total, factor = 2)

recipe_education %>% prep(., data) 

data_baked_education <- recipe_education %>% prep(., data) %>% bake(., NULL)


### 4. bIPF Family -------------------------------------------------------------
recipe_family<-
  recipes::recipe(bipf_family ~ 
                    mios_total +
                    pc_ptsd_positive_screen + 
                    military_exp_combat +
                    #MOS +
                    service_era_post_911 +
                    service_era_persian_gulf +
                    sex_male +
                    race_black +
                    race_white,
                  data = data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(mios_total, factor = 2)

recipe_family %>% prep(., data) 

data_baked_family <- recipe_family %>% prep(., data) %>% bake(., NULL)

### 5. bIPF Friends ------------------------------------------------------------
recipe_friends <-
  recipes::recipe(bipf_friends ~ 
                    mios_total +
                    pc_ptsd_positive_screen + 
                    military_exp_combat +
                    #MOS +
                    service_era_post_911 +
                    service_era_persian_gulf +
                    sex_male +
                    race_black +
                    race_white,
                  data = data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(mios_total, factor = 2)

recipe_friends %>% prep(., data) 

data_baked_friends <- recipe_friends %>% prep(., data) %>% bake(., NULL)


### 6. bIPF Spouse -------------------------------------------------------------
recipe_spouse <-
  recipes::recipe(bipf_spouse ~ 
                    mios_total +
                    pc_ptsd_positive_screen + 
                    military_exp_combat +
                    #MOS +
                    service_era_post_911 +
                    service_era_persian_gulf +
                    sex_male +
                    race_black +
                    race_white,
                  data = data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(mios_total, factor = 2)

recipe_spouse %>% prep(., data) 

data_baked_spouse <- recipe_spouse %>% prep(., data) %>% bake(., NULL)


### 7. bIPF Work ---------------------------------------------------------------
recipe_work <-
  recipes::recipe(bipf_work ~ 
                    mios_total +
                    pc_ptsd_positive_screen + 
                    military_exp_combat +
                    #MOS +
                    service_era_post_911 +
                    service_era_persian_gulf +
                    sex_male +
                    race_black +
                    race_white,
                  data = data) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(mios_total, factor = 2)

recipe_work %>% prep(., data) 

data_baked_work <- recipe_work %>% prep(., data) %>% bake(., NULL)



# B. DEFINE MODELS ----------------------------------------------------------------



model_weakly_informed <- 
  bayesian(
    family = cumulative(threshold = "flexible"),
    file = paste0(here::here(), '/output/models/fit_weakly_informed_', model_name)
    ) %>% 
  set_engine("brms") %>% 
  set_mode("classification") %>% 
  set_args(prior = b_prior_weakly_informative)

model_weakly_informed_pp <- ## prior predictive
  bayesian(
    family = cumulative(threshold = "flexible"),
    file = paste0(here::here(), '/output/models/fit_weakly_informed_pp_', model_name),
    sample_prior = "only") %>% 
  set_engine("brms") %>% 
  set_mode("classification") %>% 
  set_args(prior = b_prior_weakly_informative)

model_improper <-
  bayesian(
    family = cumulative(threshold = "flexible"),
    file = paste0(here::here(), '/output/models/fit_weakly_informed_pp_', model_name)) %>% 
  set_engine("brms") %>% 
  set_mode("classification")



# C. WORKFLOW SETS ----------------------------------------------------------------
models <-
  workflow_set(
    preproc = list(
      total = recipe_total,
      children = recipe_children, 
      daily = recipe_daily,
      education = recipe_education,
      family = recipe_family,
      friends = recipe_friends,
      spouse = recipe_spouse,
      work = recipe_work
    ),
    models = list(
      improper = model_improper,
      weakly_informed = model_weakly_informed,
      weakly_informed_pp = model_weakly_informed_pp
      
    ),
    cross = TRUE
  )

models %>% print(n = 50)



# D. FIT MODELS -------------------------------------------------------------------

### Total bIPF -----------------------------------------------------------------
if(!dir.exists(here::here('output'))){dir.create(here::here('output'))}
if(!dir.exists(here::here('output/models'))){dir.create(here::here('output/models'))}

model_name <- 'total'
fit_total_improper <- 
  models %>% 
  extract_workflow('total_improper') %>% 
  fit(data)


fit_total_weakly_informed <- 
  models %>% 
  extract_workflow('total_weakly_informed') %>% 
  fit(data)

fit_total_weakly_informed_pp <- 
  models %>% 
  extract_workflow('total_weakly_informed_pp') %>% 
  fit(data)


### 1. Children -------------------------------------------------------------------
model_name <- 'children'
fit_children_improper <- 
  models %>% 
  extract_workflow('children_improper') %>% 
  fit(data)

fit_children_weakly_informed <- 
  models %>% 
  extract_workflow('children_weakly_informed') %>% 
  fit(data)

fit_children_weakly_informed_pp <- 
  models %>% 
  extract_workflow('children_weakly_informed_pp') %>% 
  fit(data)


### 2. Daily -------------------------------------------------------------------
model_name <- 'daily'
fit_daily_improper <- 
  models %>% 
  extract_workflow('daily_improper') %>% 
  fit(data)

fit_daily_weakly_informed <- 
  models %>% 
  extract_workflow('daily_weakly_informed') %>% 
  fit(data)

fit_daily_weakly_informed_pp <- 
  models %>% 
  extract_workflow('daily_weakly_informed_pp') %>% 
  fit(data)


### 3. Education -------------------------------------------------------------------
model_name <- 'education'
fit_education_improper <- 
  models %>% 
  extract_workflow('education_improper') %>% 
  fit(data)

fit_education_weakly_informed <- 
  models %>% 
  extract_workflow('education_weakly_informed') %>% 
  fit(data)

fit_education_weakly_informed_pp <- 
  models %>% 
  extract_workflow('education_weakly_informed_pp') %>% 
  fit(data)


### 4. Family -------------------------------------------------------------------
model_name <- 'family'
fit_family_improper <- 
  models %>% 
  extract_workflow('family_improper') %>% 
  fit(data)

fit_family_weakly_informed <- 
  models %>% 
  extract_workflow('family_weakly_informed') %>% 
  fit(data)

fit_family_weakly_informed_pp <- 
  models %>% 
  extract_workflow('family_weakly_informed_pp') %>% 
  fit(data)


### 5. Friends -------------------------------------------------------------------
model_name <- 'friends'
fit_friends_improper <- 
  models %>% 
  extract_workflow('friends_improper') %>% 
  fit(data)

fit_friends_weakly_informed <- 
  models %>% 
  extract_workflow('friends_weakly_informed') %>% 
  fit(data)

fit_friends_weakly_informed_pp <- 
  models %>% 
  extract_workflow('friends_weakly_informed_pp') %>% 
  fit(data)


### 6. Spouse -------------------------------------------------------------------
model_name <- 'spouse'
fit_spouse_improper <- 
  models %>% 
  extract_workflow('spouse_improper') %>% 
  fit(data)

fit_spouse_weakly_informed <- 
  models %>% 
  extract_workflow('spouse_weakly_informed') %>% 
  fit(data)

fit_spouse_weakly_informed_pp <- 
  models %>% 
  extract_workflow('spouse_weakly_informed_pp') %>% 
  fit(data)


### 7. Work -------------------------------------------------------------------
model_name <- 'work'
fit_work_improper <- 
  models %>% 
  extract_workflow('work_improper') %>% 
  fit(data)

fit_work_weakly_informed <- 
  models %>% 
  extract_workflow('work_weakly_informed') %>% 
  fit(data)

fit_work_weakly_informed_pp <- 
  models %>% 
  extract_workflow('work_weakly_informed_pp') %>% 
  fit(data)



# E. EXTRACT MODELS ---------------------------------------------------------------

### Total ----
fit_total_improper           <- fit_total_improper %>% parsnip::extract_fit_engine()
fit_total_weakly_informed    <- fit_total_weakly_informed %>% parsnip::extract_fit_engine()
fit_total_weakly_informed_pp <- fit_total_weakly_informed_pp %>% parsnip::extract_fit_engine()

### 1. Children ----
fit_children_improper           <- fit_children_improper %>% parsnip::extract_fit_engine()
fit_children_weakly_informed    <- fit_children_weakly_informed %>% parsnip::extract_fit_engine()
fit_children_weakly_informed_pp <- fit_children_weakly_informed_pp %>% parsnip::extract_fit_engine()

### 2. Daily ----
fit_daily_improper           <- fit_daily_improper %>% parsnip::extract_fit_engine()
fit_daily_weakly_informed    <- fit_daily_weakly_informed %>% parsnip::extract_fit_engine()
fit_daily_weakly_informed_pp <- fit_daily_weakly_informed_pp %>% parsnip::extract_fit_engine()

### 3. Education ----
fit_education_improper           <- fit_education_improper %>% parsnip::extract_fit_engine()
fit_education_weakly_informed    <- fit_education_weakly_informed %>% parsnip::extract_fit_engine()
fit_education_weakly_informed_pp <- fit_education_weakly_informed_pp %>% parsnip::extract_fit_engine()

### 4. Family ----
fit_family_improper           <- fit_family_improper %>% parsnip::extract_fit_engine()
fit_family_weakly_informed    <- fit_family_weakly_informed %>% parsnip::extract_fit_engine()
fit_family_weakly_informed_pp <- fit_family_weakly_informed_pp %>% parsnip::extract_fit_engine()

### 5. Friends ----
fit_friends_improper           <- fit_friends_improper %>% parsnip::extract_fit_engine()
fit_friends_weakly_informed    <- fit_friends_weakly_informed %>% parsnip::extract_fit_engine()
fit_friends_weakly_informed_pp <- fit_friends_weakly_informed_pp %>% parsnip::extract_fit_engine()

### 6. Spouse ----
fit_spouse_improper           <- fit_spouse_improper %>% parsnip::extract_fit_engine()
fit_spouse_weakly_informed    <- fit_spouse_weakly_informed %>% parsnip::extract_fit_engine()
fit_spouse_weakly_informed_pp <- fit_spouse_weakly_informed_pp %>% parsnip::extract_fit_engine()

### 7. Work ----
fit_work_improper           <- fit_work_improper %>% parsnip::extract_fit_engine()
fit_work_weakly_informed    <- fit_work_weakly_informed %>% parsnip::extract_fit_engine()
fit_work_weakly_informed_pp <- fit_work_weakly_informed_pp %>% parsnip::extract_fit_engine()
