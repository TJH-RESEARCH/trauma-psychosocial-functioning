
 
pp_check(model_1_hurdle, ndraws = 100)
pp_check(model_1_hurdle, type = "hist", ndraws = 11)
pp_check(model_1_hurdle, type = "error_hist", ndraws = 12)
pp_check(model_1_hurdle, type = "scatter_avg", ndraws = 100)

## The Hurdle model is okay, but it is not constrained at the highest end
## So there is error at the high range.
## A few ways to approach this. 
## First, ignore it, assuming that these are basically censored. 
## We can only measure difficulty on this range, but some at the max score have more difficulty than others. 
## Second, try to model the censoring.
## Third, we perform a transformation on the data.
## Fourth, we use the categories and model an ordered logit -- this will capture the high and low boundaries, but lose information. Indeed, the model will almost certainly fit better because we are getting rid of variance in the dependent variable.


