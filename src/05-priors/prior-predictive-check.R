library(brms)
library(cmdstanr)
library(modelr)


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
      # for the non-zero process, in log for 0-100 outcome
      prior(normal(0, 2), class = b),
      prior(normal(2.3, 1), class = Intercept), # baseline around 10 on the 0-100 scale
      
      # for the zero process: probability of being 0. Log odds intercept, log variables, for binary outcome
      prior(normal(0, 1), class = Intercept, dpar = hu),  # hurdle intercept. 
      prior(normal(0, 1), class = b, dpar = hu),          # logistic coefficients
      
      # Shape
      prior(lognormal(log(20), 0.5), class = shape)
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


# Get linear predictor on log scale
linpred_mat <- posterior_linpred(model_1_hurdle_prior, 
                                 newdata = data_sim, 
                                 dpar = "mu", 
                                 transform = FALSE)

# Summarize across draws to get a single estimate per row (mean of the posterior)
data_sim$log_mu <- colMeans(linpred_mat)

# compute posterior mean on outcome scale
data_sim$mu <- exp(data_sim$log_mu)

# Plot linear predictor (log-mu)
ggplot(data_sim, aes(x = pcl_total, y = log_mu)) +
  geom_point(alpha = 0.75) +
  labs(y = "Linear Predictor (log-mu)")

# Plot linear predictor (log-mu)
ggplot(data_sim, aes(x = pcl_total, y = mu)) +
  geom_point(alpha = 0.75) +
  labs(y = "Linear Predictor (outcome scale)")

## Prior draws for coefficients. 
as_draws_df(model_1_hurdle_prior) %>%
  select(starts_with("b_")) %>%
  pivot_longer(cols = everything(), names_to = "term", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~term, scales = "free") +
  labs(title = "Prior Draws for Coefficients")







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
 

## OK so I was working on the wrong scale before...not 0 to 7 but 0 to 100
## And I didn't understand the shape parameter well enough
## So let's start over. 

# b N(0,2), int N(1, 2), zero int N(0,1), b (0,1), shape lognormal(log(20), .5))
## neutral relationships between linear predicted outcomes and PCL total: good
## but the center is too low: 2.6 to 2.9 when the outcome scale is 0 to 100
## So I'll move the intercept up from around 0 to around 50 (4 in log) N(4,2)
## The outcomes scale moved up, to around 54. That's good. A slight negative relationship tho. 
## Now it isn't necessarily bad that the previous outcomes were around 0, since most of the coefficients should have been around 0. 
## so maybe I'll move the intercept to around 10 -- this assumes that a baseline for people without zero scores is around 20, with demos and PCL moving it around

## So I'll try int N(2.3, 1) which gives 95% of mass between 1 and 74. 

## Ok I went with the N(0,2). Feeling confident enough to progress, I fit the model to the data. But I'm a little skeptical of the results. The intercept for the non-zero process is 3.95 (which is about 52 on the outcome scale, right?). And the coefficient of my explanatory variable is 0.323, which would mean that one standard deviation change only moves the outcome about 1.3 on a scale from 0-100. That seems a little too weak. Obviously I don't want to bake hypothesis into the result, but it seems too weak.
### actually, I think I am misinterpreting the coefficient with the log shift. It isn't additive but multiplicative. So that's actually about a 20 point swing per SD:
#### $$ \Delta \mu = \mu \cdot \left( e^{\beta} - 1 \right) = 52 \cdot \left( e^{0.323} - 1 \right) \approx 52 \cdot \left( 1.381 - 1  \right) \approx 19.8 $$

