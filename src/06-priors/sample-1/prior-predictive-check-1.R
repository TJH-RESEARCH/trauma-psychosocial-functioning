
# Prior Predictive Check


# Grid Data ---------------------------------------------------------------
## Create data "grid" similar to the real data, 
## with a sequence of values for the explanatory variable from -1.5 to 2 
## (roughly the min and max of pcl_total in data_baked_1 -- in SD not original units)
## and every combination of the dummy-coded covariates.

data_sim_1 <-
  expand_grid(
    pcl_total = seq(-1.5, 2, by = .05), # Range
    status = c('service_member', 'veteran', 'civilian'),
    gender_female = c(0,1),
    birth_year = c('born_79_84', 'born_85_89', 'born_90_95', 'born_96_01', 'born_other'), 
    trauma = c(0,1)
  ) %>% 
  
  mutate(
    ## Dummy code categorical variables
    veteran = ifelse(status == 'veteran', 1, 0),
    civilian = ifelse(status == 'civilian', 1, 0),
    born_79_84 = ifelse(birth_year == 'born_79_84', 1, 0),
    born_85_89 = ifelse(birth_year == 'born_85_89', 1, 0),
    born_90_95 = ifelse(birth_year == 'born_90_95', 1, 0),
    born_96_01 = ifelse(birth_year == 'born_96_01', 1, 0),
    
    ## and transform the pcl_total SD to original units, saved in a separate variable
    pcl_total_og = pcl_total * sd(data_1$pcl_total) + mean(data_1$pcl_total)
  )
  


# Linear Predictor --------------------------------------------------------
## Calculate the "linear predictor" i.e.,  μ = βX + α  , on log scale
## using the simulated grid data above and predictions from
## the model fit to the priors only. In other words, predict this model μ given 
## the prior specifications of β and α
## and with X = the range of possible observable values.

linpred_prior_1 <- 
  posterior_linpred(model_1_hurdle_prior, 
                    newdata = data_sim_1, 
                    dpar = "mu", 
                    transform = FALSE)

# Summarize across draws to get a single estimate per row (mean of the posterior)
data_sim_1$log_mu <- colMeans(linpred_prior_1)

# compute posterior mean on outcome scale (not the log scale)
data_sim_1$mu <- exp(data_sim_1$log_mu)


# Plot linear predictor (Response Scale) ---------------------------------------
plot_prior_predictive_1 <-
ggplot(data_sim_1, aes(x = pcl_total_og, y = mu)) +
  geom_smooth(method = 'loess', color = colors_tam[2]) +
  labs(
    title = "Sample from weakly informative prior",
    subtitle = "With beta around 0, the effect should be flat.",
    x = "PCL Total", 
    y = "Linear Predictor (μ)",
    caption = "This basically assumes most people would have a dysfunction around 10<br>if none of the variables (explanatory or covariates) had a consistent effect."
    ) +
  lims(y = c(0,100), x = c(0,80)) +  # to put on the scale of the outcome variable 0-100
  theme_scatter


## Print to console ------------------------------------------------------------
plot_prior_predictive_1 %>% print()


## Save to file ----------------------------------------------------------------
ggsave(plot = plot_prior_predictive_1, file = here::here("output/plot-prior-predictive-1.jpg"), width = 6, height = 4)


# Plot prior draws for coefficients  -------------------------------------------
plot_prior_draws_1 <-
  as_draws_df(model_1_hurdle_prior) %>%
  select(starts_with("b_")) %>%
  pivot_longer(cols = everything(), names_to = "term", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~term, scales = "free") +
  labs(title = "Prior Draws for Coefficients") +
  lims(x = c(-12, 12), y = c(0, 3500)) + 
  theme_scatter


# Print to console --------------------------------------------------------
plot_prior_draws_1 %>% print()

# Save to file ------------------------------------------------------------
ggsave(plot = plot_prior_draws_1, file = here::here("output/priors/plot-prior-draws-1.jpg"), width = 8, height = 8)

# Remove objects from environment -----------------------------------------
rm(plot_prior_draws_1, data_sim_1, plot_prior_predictive_1, linpred_prior_1)





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

