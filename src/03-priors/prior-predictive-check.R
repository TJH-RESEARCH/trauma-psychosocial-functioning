library(brms)
library(cmdstanr)


# Hurdle gamma model: Priors  --------------------------------------------------
model_1_hurdle_prior <- 
  brm(
    bf(
      
      # Linear Model to Predict outcomes for non-zero values
      bipf_score ~ 
        pcl_total + 
        deployed + 
        # gender_male + # REFERENCE GROUP
        gender_female + 
        race_asian +
        race_black +
        race_native +
        #race_pacific + # removed for non-variance i.e., all 0s
        #race_latinx + # removed for non-variance i.e., all 0s
        race_other +
        # race_white + # REFERENCE GROUP
        rank_e1_e4 + 
        # rank_e5_e6 + # REFERENCE GROUP
        rank_e7_e9 + 
        rank_o1_o3_w1_cw2 + 
        rank_o4_o6_cw3_cw5 + 
        mos_combat +
        mos_combat_support + 
        mos_combat_service_support + 
        mos_operations + 
        mos_logistics + 
        mos_support + 
        mos_medical +
        # mos_acquisitions + # removed for non-variance i.e., all 0s
        branch_airforce +
        #branch_army + # REFERENCE GROUP
        branch_marines,
      #branch_navy # removed for non-variance i.e., all 0s
      
      
      # Logistic Model to Predict 0 values
      hu ~ 
        pcl_total + 
        deployed + 
        # gender_male + # REFERENCE GROUP
        gender_female + 
        race_asian +
        race_black +
        race_native +
        #race_pacific + # removed for non-variance i.e., all 0s
        #race_latinx + # removed for non-variance i.e., all 0s
        race_other +
        # race_white + # REFERENCE GROUP
        rank_e1_e4 + 
        # rank_e5_e6 + # REFERENCE GROUP
        rank_e7_e9 + 
        rank_o1_o3_w1_cw2 + 
        rank_o4_o6_cw3_cw5 + 
        mos_combat +
        mos_combat_support + 
        mos_combat_service_support + 
        mos_operations + 
        mos_logistics + 
        mos_support + 
        mos_medical +
        # mos_acquisitions + # removed for non-variance i.e., all 0s
        branch_airforce +
        #branch_army + # REFERENCE GROUP
        branch_marines
    ),
    
    # DATA SET
    data = data_baked_1,
    
    # MODEL
    family = hurdle_gamma(),
    
    # PRIOR OPTIONS
    prior = c(
      # for the linear part
      prior(normal(0, .75), class = b),
      prior(normal(0, .75), class = Intercept),
      
      # for the logistic part: probability of being 0. 
      prior(normal(0, .5), class = Intercept, dpar = hu), # hurdle intercept. 
      prior(normal(0, .75), class = b, dpar = hu), # logistic coefficients
      prior(logistic(0, 1), class = shape)
    ),
    sample_prior = 'only',
    
    # Stan options
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = SEED,
    backend = "cmdstanr"
  )

prior_summary(model_1_hurdle_prior)

# Prior Predictive

## Simulate some data
data_sim <-
  expand_grid(
    pcl_total = seq(0, 3.75, by = .05), 
    deployed = c(0,1),
    gender_female = c(0,1),
    race = c('asian', 'black', 'native', 'other', 'white'),
    rank = c('e1_e4', 'e5_e6', 'e7_e9', 'o1_o3_w1_cw2', 'o4_o6_cw3_cw5'),
    mos = c('combat', 'combat_support', 'combat_service_support', 'operations', 'logistics', 'support', 'medical'),
    branch = c('airforce', 'army', 'marines')
  ) %>% 
  
  # Dummary Dummy Variables for Categorical
  mutate(
    race_asian = ifelse(race == 'asian', 1, 0),
    race_black = ifelse(race == 'black', 1, 0),
    race_native = ifelse(race == 'native', 1, 0),
    race_other = ifelse(race == 'other', 1, 0),
    rank_e1_e4 = ifelse(rank == 'e1_e4', 1, 0),
    rank_e7_e9 = ifelse(rank == 'e7_e9', 1, 0),
    rank_o1_o3_w1_cw2 = ifelse(rank == 'o1_o3_w1_cw2', 1, 0),
    rank_o4_o6_cw3_cw5 = ifelse(rank == 'o4_o6_cw3_cw5', 1, 0),
    mos_combat = ifelse(mos == 'combat', 1, 0),
    mos_combat_support = ifelse(mos == 'combat_support', 1, 0),
    mos_combat_service_support = ifelse(mos == 'combat_service_support', 1, 0),
    mos_operations = ifelse(mos == 'operations', 1, 0),
    mos_logistics = ifelse(mos == 'logistics', 1, 0),
    mos_support = ifelse(mos == 'support', 1, 0),
    mos_medical = ifelse(mos == 'medical', 1, 0),
    branch_airforce = ifelse(branch == 'airforce', 1, 0),
    branch_marines = ifelse(branch == 'marines', 1, 0)
  ) %>% 
  sample_n(size = 3000, replace = FALSE)


# Run Predictions
preds_prior <-
  bind_cols(
    data_sim,
    predict(object = model_1_hurdle_prior, newdata = data_sim)
  )

# Plot Predictions
preds_prior %>% 
  ggplot(aes(pcl_total, Estimate)) + 
  geom_point(alpha = .75, position = 'jitter')

## prior predictive produced about 5 extreme outliers; change from a student t to a normal for less density in the tails
## a normal(0,2) still produces lots of extreme preds
## my outcome data are on a 0 to 7 scale
## my categorical predictors are dummies
## my continuous predictor is standardized
## so a 1sd change in the continuous prediction might, at an extreme, move the outcome from 0 to 7. That's a coeficient of 7. 
## im currently getting estimates in the 1000s, so I need to restrict the priors
## i'll try a normal(0,1)...that changed the extreme preds to the left from the right
## let's adjust the intercept to a normal(0,1) from a students t(df = 5, 0, 1.75)
## THat looks at lot better! although a weird curvilinearity, things are now much closed to scale, with most below 50, and the outlier at 150, and someoutliers between 50 and 100
## let's restrict the coef further, to N(0, .5) -- that was slightly better but mostly the same
## this whole time I haven't touched the logistic priors...
## bring in the intercept a little more, N(0,.5) - ok, still the curvilinear stuff, but the estimated outcomes are all almost reasonable (i.e., at or below 2) which is actually a little low
## using N(0, .7) for intercept and betas works well, with estimates mostly under 7...a few slightly over 7
## There is still the wierd curvilinear thing, with the highest estimated being at 0 on the predictor... I think that is from the hurdle priors. I'll adjust those now

## I def had the wrong priors on there...those are from a previous version of me that didn't understand this stuff
## As I understand now, there is an intercept 'hu' and a coefficient. All are in log odds. 
## So let's assume there's a baseline 40-60% chance of being 0 (log odds of 0, +- .5) and coefficients move that around +- 35% which is a 1.5 log odds swing
## I can use a normal of N(0, .25) for the intercept and a N(0, .75)
## I got a Warning: 1 of 16000 (0.0%) transitions ended with a divergence....
## But the outcomes look reasonable... there are a couple of outlier outcome estimates (20, 15, five or so around 10). but the rest are in the 0 to 7 range
## I may try to bring the coefficient in slightly N(0, .7). that reduced the outliers. now everything is under 10, with only five of three thousand preds between 7 and 10.
 



