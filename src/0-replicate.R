# Run this entire script to replicate the analysis



# 0. CONFIGURATION -------------------------------------------------------------
## Load packages:
library(tidyverse)          # Data manipulation

library(lavaan)             # Internal consistency

library(cmdstanr)           # MCMC sampler
library(brms)               # Interface to Stan
library(tidybayes)          # working with brms outputs
library(posterior)          # Working with Bayesian posteriors
library(marginaleffects)    # Calculating marginal effects
library(ggdist)             # Plotting Bayesian posteriors
library(bayesplot)          # Posterior predictive plots
library(modelr)             # Data grids for model predictions

library(recipes)            # Model pre-processing

library(ggtext)             # Fonts
library(scales)             # Axis labels
library(MetBrewer)          # Color palettes
library(patchwork)          # Combining plots
library(ggdag)              # Plotting DAGs
library(dagitty)            # Creating DAGs

## Loads the themes for visualizations:
source(here::here('src/00-config/theme-custom.R'))    # Custom theme for plots
source(here::here('src/00-config/theme-histogram.R')) # Custom theme for histograms
source(here::here('src/00-config/theme-scatter.R'))   # Custom theme for scatter plots
source(here::here('src/00-config/theme-ppc.R'))       # Custom theme for Posterior Predictive Checks
source(here::here('src/00-config/color-palettes.R'))  # Color palette

## Load the data sets:
data_1 <- read_csv(here::here('data/data-moore-dissertation.csv'))
data_2 <- read_csv(here::here('data/data-dissertation-main.csv'))
data_3 <- read_csv(here::here('data/data-first-year-scholars-cleaned.csv'))

## Score and categorize the bIPF in each of the 3 data sets:
source(here::here('src/00-config/score-bipf.R'))

## Set global Stan Monte Carlo Markov Chain (MCMC) options:
source(here::here('src/00-config/program-mcmc.R'))



# 1. DESCRIBE THE SAMPLES ------------------------------------------------------
## Save the sample size
source(here::here('src/01-describe-sample/sample-size.R'))

## Create demographic tables for the 3 samples
source(here::here('src/01-describe-sample/helper-functions.R'))
source(here::here('src/01-describe-sample/demographic-table-1.R'))
source(here::here('src/01-describe-sample/demographic-table-2.R'))
source(here::here('src/01-describe-sample/demographic-table-3.R'))
source(here::here('src/01-describe-sample/demographic-tables-combine.R'))



# 2. EXAMINE MEASURES -----------------------------------------------------
## Examine the internal consistency of the PCL-5 and bIPF measures
source(here::here('src/02-examine-measures/internal-consistency.R'))

## Impute the missing value for Sample 3, MIOS Item #9
source(here::here('src/02-examine-measures/impute.R'))



# 3. EXAMINE VARIABLES ----------------------------------------------------
## Examine the univariate and bivariate distributions of the variables
source(here::here('src/03-examine-variables/plot-univariate.R'))
source(here::here('src/03-examine-variables/plot-bivariate.R'))

## Count the number of zeros in the outcome variable
source(here::here('src/03-examine-variables/count-zeros.R'))

## Examine covariates
source(here::here('src/03-examine-variables/groups.R'))


# 4. GENERATIVE MODELS -------------------------------------------------------
## Draw DAGs (graphical causal models)
#source(here::here('src/01-generative-models/dags.R'))





# 5. DATA PRE-PROCESSING -----------------------------------------------------------------
## Select the needed variables, scale the data, and handle issues such as non-variance and missingness

## Prepare the Data by Standardizing continuous variables, removing non-variance, centering dummy variables
source(here::here('src/05-pre-processing/recipes-1.R'))
source(here::here('src/05-pre-processing/recipes-2.R'))
source(here::here('src/05-pre-processing/recipes-3.R')) # includes the interaction model



# 6. PRIORS ----------------------------------------------------------------------
## Specify priors and adjust them through prior predictive checks
## in practice, this was an iterative process

#### Specify the priors used across the studies
source(here::here("src/06-priors/specify-priors.R"))

### Plot priors used across studies
source(here::here('src/06-priors/plot-priors.R'))

### Sample 1
source(here::here("src/06-priors/sample-1/sample-priors-1.R")) #### Sample from the Prior Distributions
source(here::here('src/06-priors/sample-1/prior-predictive-check-1.R')) #### Prior Predictive Check

### Sample 2
source(here::here("src/06-priors/sample-2/sample-priors-2.R")) #### Sample from the Prior Distributions
source(here::here('src/06-priors/sample-2/prior-predictive-check-2.R')) #### Prior Predictive Check

### Sample 3
source(here::here("src/06-priors/sample-3/sample-priors-3.R")) #### Sample from the Prior Distributions
source(here::here('src/06-priors/sample-3/prior-predictive-check-3.R')) #### Prior Predictive Check


## FIT MODELS  -----------------------------------------------------------------

### Sample 1 - Hurdle - Full Controls
source(here::here('src/07-modeling/sample-1/fit-model-hurdle-1.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-1/fit-model-hurdle-1-vague.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-1/fit-model-hurdle-1-improper.R'))   # Fit the model 

### Sample 1 - Hurdle - Bivariate
source(here::here('src/07-modeling/sample-1/fit-model-hurdle-1-bi.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-1/fit-model-hurdle-1-bi-vague.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-1/fit-model-hurdle-1-bi-improper.R'))   # Fit the model 

### Sample 2 - Hurdle
source(here::here('src/07-modeling/sample-2/fit-model-hurdle-2.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-2/fit-model-hurdle-2-vague.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-2/fit-model-hurdle-2-improper.R'))   # Fit the model 

### Sample 3 - Hurdle Adjusted
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-adjust.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-adjust-vague.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-adjust-improper.R'))   # Fit the model 

### Sample 3 - Hurdle MIOS
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-mios.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-mios-vague.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-mios-improper.R'))   # Fit the model 

### Sample 3 - Hurdle PCL
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-pcl.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-pcl-vague.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-pcl-improper.R'))   # Fit the model 

### Sample 3 - Hurdle with Interaction
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-interact.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-interact-vague.R'))   # Fit the model 
source(here::here('src/07-modeling/sample-3/fit-model-hurdle-3-interact-improper.R'))   # Fit the model 


# MCMC SAMPLING DIAGNOSTICS -----------------------------------------------
## Helper Function
source(here::here('src/08-diagnostics/model-labels.R'))

## Divergent Draws
source(here::here('src/08-diagnostics/divergent-draws.R'))

## ESS
source(here::here('src/08-diagnostics/ess.R'))

## Leapfrog
source(here::here('src/08-diagnostics/leapfrog.R'))

## Step Size
source(here::here('src/08-diagnostics/step-size.R'))

## Plot Acceptance
source(here::here('src/08-diagnostics/plot-acceptance.R'))

## Plot Catepillar
source(here::here('src/08-diagnostics/plot-caterpillar.R'))

## Plot Trace Rank
source(here::here('src/08-diagnostics/plot-trace-rank.R'))


# POSTERIOR DISTRIBUTION ----------------------------------------------------

## Posterior Predictive Check
source(here::here('src/09-posterior/posterior-predictive-check.R'))

## Model Comparison
#source(here::here('src/07-modeling/compare-models.R'))

## Posterior Distributions
source(here::here('src/09-posterior/posterior.R'))

## Summarize the posterior probability
source(here::here('src/09-posterior/summarize-posterior.R'))

## Conditional Effects
source(here::here('src/09-posterior/conditional-effects.R'))

## Marginal Effects
source(here::here('src/09-posterior/marginal-effects.R'))


