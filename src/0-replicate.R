# CONFIGURATION ----------------------------------------------------------------

## Packages
library(tidyverse)

## Parallel Processing
cores <- ifelse(parallel::detectCores() < 4, parallel::detectCores(), 4) ## Ensure computer has enough cores

## Plot themes
source(here::here('src/0-theme.R'))

# 1. GENERATIVE MODELS ---------------------------------------------------------
## Draw DAGs 
#source(here::here('src/01-generative-models/dags.R'))

## Simulations



# 2. DATA PREP -----------------------------------------------------------------

## Load the data and score the bIPF:

### Load functions to score and categorize the bIPF
source(here::here('src/02-pre-processing/score-bipf.R'))

### Dataset 1
data_1 <- read_csv(here::here('data/data-brians-dissertation.csv'))
data_1 <- data_1 %>% categorize_bipf() # calculate the categories for the outcome variable
data_1 %>% count(bipf_category) # print to show scoring/categorization worked
data_1 %>% count(bipf_NAs, bipf_answered) # Data 1 has no NAs

### Dataset 2
data_2 <- read_csv(here::here('data/data-dissertation-main.csv'))
data_2 <- data_2 %>% score_bipf() # calculate the scores for the outcome variable
data_2 <- data_2 %>% categorize_bipf() # calculate the categories for the outcome variable
data_2 %>% count(bipf_category) # print to show scoring/categorization worked
data_2 %>% count(bipf_NAs, bipf_answered) # Data 2 has various numbers of NAs. About 15 did not answer 4 or more.
data_2 <- data_2 %>% filter(bipf_answered >= 4) # remove the 15 results that did not answer at least 4 bIPF items 

### Dataset 3
data_3 <- read_csv(here::here('data/data-first-year-scholars-cleaned.csv'))
data_3 <- data_3 %>% score_bipf() # calculate the scores for the outcome variable
data_3 <- data_3 %>% categorize_bipf() # calculate the categories for the outcome variable
data_3 %>% count(bipf_category) # print to show scoring/categorization worked
data_3 %>% count(bipf_NAs, bipf_answered) # Data 3 has various numbers of NAs. About 15 did not answer 4 or more.
data_3 <- data_3 %>% filter(bipf_answered >= 4) # remove the 15 results that did not answer at least 4 bIPF items 


## Prepare the Data by Standardizing continuous variables, removing non-variance, centering dummy variables
source(here::here('src/02-pre-processing/scale-data.R'))



# PREPARE MCMC SAMPLING ---------------------------------------------------
## Set global Stan options
CHAINS <- 4
ITER <- 6000
WARMUP <- 2000
SEED <- 999888777
options(mc.cores = parallel::detectCores())


# STUDY 1 ----------------------------------------------------------------------

## PRIORS ----------------------------------------------------------------------

### Plot the priors
source(here::here('src/03-priors/plot-priors.R'))
    
### Prior Predictive Check
source(here::here('src/03-priors/prior-predictive-check.R'))


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


