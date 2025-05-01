# CONFIGURATION ----------------------------------------------------------------

## Packages
library(tidyverse)

## Parallel Processing
cores <- ifelse(parallel::detectCores() < 4, parallel::detectCores(), 4) ## Ensure computer has enough cores


# 1. GENERATIVE MODELS ---------------------------------------------------------
## Draw DAGs 
source(here::here('src/01-generative-models/dags.R'))

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


## Standardize the data and center the predictors
source(here::here('src/02-pre-processing/scale-data.R'))



# STUDY 1 --------------------------------------------------------------------


## Set global Stan options
CHAINS <- 4
ITER <- 2000
WARMUP <- 1000
SEED <- 999888777

## Priors



## PLOT PRIORS -----------------------------------------------------------------
source(here::here('src/03-priors/plot-priors.R'))
    
## SPECIFY PRIORS --------------------------------------------------------------
source(here::here('src/03-priors/specify-priors.R'))






# MODELLING ---------------------------------------------------------------





# POST PROCESSING ---------------------------------------------------------
source(here::here('src/05-post-processing/spread-draws-multivariate.R'))





# MCMC DIAGNOSTICS ---------------------------------------------------------
source(here::here('src/06-mcmc-diagnostics/diagnostics.R'))





# POSTERIOR PREDICTIVE CHECK ----------------------------------------------

  




# POSTERIOR DISTRIBUTION ----------------------------------------------------


## PLOT POSTERIOR ----------------------------------------------------------


## SUMMARIZE POSTERIOR -----------------------------------------------------





# PREDICTIONS -------------------------------------------------------------


