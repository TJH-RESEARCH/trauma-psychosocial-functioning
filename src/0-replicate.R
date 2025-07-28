
# Run this entire script to replicate the analysis


# 0. CONFIGURATION -------------------------------------------------------------

## Load packages:
library(tidyverse)
library(cmdstanr)

## Loads the themes for visualizations:
source(here::here('src/00-config/themes.R'))

## Set global Stan Monte Carlo Markov Chain (MCMC) options:
CHAINS <- 4                                  # Number of sample chains for MCMC
ITER <- 6000                                 # Number of samples for MCMC
WARMUP <- 2000                               # Number of warm-up samples for MCMC
SEED <- 999888777                            # Random seed to replicate MCMC sampling
options(mc.cores = parallel::detectCores())  # Detects the number of cores on your computer for parallel processing

## Set MCMC sampler to cmdstanr if it is available on your machine. If not, it will use rstan.
if ("cmdstanr" %in% rownames(installed.packages()) &
    cmdstanr::cmdstan_version() >= "2.30") {
  options(brms.backend = "cmdstanr")
} else {
  message("CmdStan not installed. Falling back to rstan.")
  options(brms.backend = "rstan")
}

## if you get an error about missing a path to cmnstan, try re-installing with install_cmdstan()

## Load the data sets:
data_1 <- read_csv(here::here('data/data-moore-dissertation.csv'))
data_2 <- read_csv(here::here('data/data-dissertation-main.csv'))
data_3 <- read_csv(here::here('data/data-first-year-scholars-cleaned.csv'))

## Score and categorize the bIPF in each of the 3 data sets:
source(here::here('src/00-config/score-bipf.R'))


# 1. DESCRIBE THE SAMPLES ------------------------------------------------------
## Create demographic tables for the 3 samples
source(here::here('src/01-describe-sample/demographic-tables.R'))


# 2. EXAMINE MEASURES -----------------------------------------------------
## Examine the internal consistency of the PCL-5 and bIPF measures
source(here::here('src/02-examine-measures/consistency.R'))
source(here::here('src/02-examine-measures/impute.R'))


# 3. EXAMINE VARIABLES ----------------------------------------------------
## Examine the univariate and bivariate distributions of the variables

source(here::here('src/03-examine-variables/plot-univariate.R'))
source(here::here('src/03-examine-variables/count-zeros.R'))
source(here::here('src/03-examine-variables/plot-bivariate.R'))


# 4. GENERATIVE MODELS -------------------------------------------------------
## Create graphical causal models and simulate fake data from them

## Draw DAGs 
#source(here::here('src/01-generative-models/dags.R'))

## Simulations



# 5. DATA PRE-PROCESSING -----------------------------------------------------------------
## Select the needed variables, scale the data, and handle issues such as non-variance and missingness

## Prepare the Data by Standardizing continuous variables, removing non-variance, centering dummy variables
source(here::here('src/05-pre-processing/recipes.R'))



# STUDY 1 ----------------------------------------------------------------------

# 6. PRIORS ----------------------------------------------------------------------
## Specify priors and adjust them through prior predictive checks
## in practice, this was an iterative process

### Plot the priors
source(here::here('src/06-priors/study-1/plot-priors.R'))

### Specify the priors
source(here::here("src/06-priors/study-1/specify-priors.R"))

### Sample from the Prior Distributions
source(here::here("src/06-priors/study-1/sample-priors.R"))
  
### Prior Predictive Check
source(here::here('src/06-priors/study-1/prior-predictive-check.R'))


## FIT MODELS  -----------------------------------------------------------------

### Fit the model 
source(here::here('src/04-models/study-1/01-fit-model-hurdle.R'))

### Check the MCMC diagnostics
source(here::here('src/04-models/study-1/02-diagnostics.R'))

### Posterior predictive check
source(here::here('src/04-models/study-1/03-posterior-predictive-check.R'))

### Sensitivity Analysis & Model Comparison

#### fit same model but with improper flat priors
source(here::here('src/04-models/study-1/04-fit-improper-hurdle.R'))

#### try to fit the same model but with a vague prior
source(here::here('src/04-models/study-1/05-fit-vague-hurdle.R'))

#### Compare the preferred model to the vague and flat prior models
source(here::here('src/04-models/study-1/06-compare-models.R'))



# POSTERIOR DISTRIBUTION ----------------------------------------------------

### Plot the posterior probability

### Summarize the posterior probability




# PREDICTIONS -------------------------------------------------------------


