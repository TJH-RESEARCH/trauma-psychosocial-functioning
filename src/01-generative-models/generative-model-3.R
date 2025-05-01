
# Simulate Data - DAG 3

##### helper function to regularize between 0 and 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

## Sample size  ----------------------------------------------------------------
n <- 100 

## Education -------------------------------------------------------------------
### coefficients
b_education_high_pf <- .1
b_education_medium_pf <- .1
b_education_low_pf <- .1

### probabilities
# p_education <- range01( variables )
p_education_high <- .30 # ifelse(p_education >= .8, 1, 0)
p_education_medium <- .35 # ifelse(p_education < .8 & p_education > .4, 1, 0)
p_education_low <- .35 # ifelse(p_education <= .4, 1, 0)

### simulated observations
education <- sample(c('low', 'medium', 'high'), size = n, replace = TRUE, prob = c(p_education_low, p_education_medium, p_education_high))
education_high <- ifelse(education == 'high', 1, 0)
education_medium <- ifelse(education == 'medium', 1, 0)
education_low <- ifelse(education == 'low', 1, 0)

## Gender ----------------------------------------------------------------------
b_female <- .1
p_female <- .5
female <- rbinom(n, 1, prob = p_female)


## Income  -----------------------------------------------------------------------
### Coefficients - income
b_income_low <- .1
b_income_medium <- .1
b_income_high <- .1

### Probabilities - income
p_income_low <- .5
p_income_medium <- .3
p_income_high <- .2

### Simulated observations - income
income <- sample(c('low', 'medium', 'high'), size = n, replace = TRUE, prob = c(p_income_low, p_income_medium, p_income_high))
income_low <- ifelse(income == 'low', 1, 0)
income_medium <- ifelse(income == 'medium', 1, 0)
income_high <- ifelse(income == 'high', 1, 0)


## Married ---------------------------------------------------------------------
b_married <- .1
married <- rbinom(n, 1, prob = .5)

## Non-OSI mental Health -------------------------------------------------------
b_mental_health <- .1
mental_health <- rnorm(n, mean = 0, sd = 1)

## Number of Children ----------------------------------------------------------
b_children <- .1
children <- rbinom(n, 1, prob = .5)

## Service Era -----------------------------------------------------------------
### Coefficients = Service Era
b_era_911 <- .1
b_era_persian <- .1
b_era_viet <- .1
  
### Probability - Service Era
p_era_911 <- .34
p_era_persian <- .33
p_era_viet <- .33

### Simulated Observations - Service Era
era <- sample(c('vietnam', 'persian', '911'), size = n, replace = TRUE, prob = c(p_era_viet, p_era_persian, p_era_911))
era_viet <- ifelse(era == 'vietnam', 1, 0)
era_911 <- ifelse(era == '911', 1, 0)
era_persian <- ifelse(era == 'persian', 1, 0)


## Race ------------------------------------------------------------------------
#### Coefficients - Race
b_race_white <- .1
b_race_black <- .1
b_race_latino <- .1
b_race_asian <- .1
b_race_native <- .1
b_race_other <- .1

### Probabilities - Race
p_race_asian <- .1
p_race_black <- .1
p_race_latino <- .1
p_race_native <- .1
p_race_other <- .1
p_race_white <- .1

### Simulated observations - Race
race <- sample(c('asian', 'black', 'latino', 'native', 'other', 'white'), size = n, replace = TRUE, prob = c(p_race_asian, p_race_black, p_race_latino, p_race_native, p_race_other, p_race_white))

### Dummy variables - Race
race_asian <- ifelse(race == 'asian', 1, 0)
race_black <- ifelse(race == 'black', 1, 0)
race_latino <- ifelse(race == 'latino', 1, 0)
race_native <- ifelse(race == 'native', 1, 0)
race_other <- ifelse(race == 'other', 1, 0)
race_white <- ifelse(race == 'white', 1, 0)

## Rank  -----------------------------------------------------------------------
### Coefficients - Rank
b_rank_low <- .1
b_rank_medium <- .1
b_rank_high <- .1

### Probabilities - Rank
p_rank_low <- .5
p_rank_medium <- .3
p_rank_high <- .2

### Simulated observations - Rank
rank <- sample(c('low', 'medium', 'high'), size = n, replace = TRUE, prob = c(p_rank_low, p_rank_medium, p_rank_high))
rank_low <- ifelse(rank == 'low', 1, 0)
rank_medium <- ifelse(rank == 'medium', 1, 0)
rank_high <- ifelse(rank == 'high', 1, 0)


## Combat ----------------------------------------------------------------------
p_combat <- 
  range01(
    b_era_viet * era_viet  + 
    b_era_persian * era_persian +
    b_female * female + 
    b_race_asian * race_asian + 
    b_race_black * race_black + 
    b_race_latino * race_latino + 
    b_race_native * race_native + 
    b_race_white * race_white + error
  )

combat <- rbinom(n, 1, prob = p_combat)
b_combat <- .1



## Moral Injury ----------------------------------------------------------------
b_mi_pf <- .1
mi <- b_combat * combat + b_era_viet * era_viet  + 
  b_era_persian * era_persian +
  b_female * female + 
  b_race_asian * race_asian + 
  b_race_black * race_black + 
  b_race_latino * race_latino + 
  b_race_native * race_native + 
  b_race_white * race_white + rnorm(n, mean = 0, sd = 1)


## PTSD ------------------------------------------------------------------------
b_ptsd_pf <- .1
ptsd <- b_combat * combat + b_era_viet * era_viet  + 
  b_era_persian * era_persian +
  b_female * female + 
  b_race_asian * race_asian + 
  b_race_black * race_black + 
  b_race_latino * race_latino + 
  b_race_native * race_native + 
  b_race_white * race_white + rnorm(n, mean = 0, sd = 1)


## Other Operational Stress Injury ---------------------------------------------
b_osi_pf <- .1
osi <- b_combat * combat + b_era_viet * era_viet  + 
  b_era_persian * era_persian +
  b_female * female + 
  b_race_asian * race_asian + 
  b_race_black * race_black + 
  b_race_latino * race_latino + 
  b_race_native * race_native + 
  b_race_white * race_white + rnorm(n, mean = 0, sd = 1)


## Error -----------------------------------------------------------------------
error <- rnorm(n, mean = 0, sd = 1)


## Psychosocial functioning formula --------------------------------------------
PF_3 <- b_mi_pf * mi + b_ptsd_pf * ptsd + b_osi_pf * osi + 
        b_era_viet * era_viet  + 
        b_era_persian * era_persian +
        b_female * female + 
        b_race_asian * race_asian + 
        b_race_black * race_black + 
        b_race_latino * race_latino + 
        b_race_native * race_native + 
        b_race_white * race_white + 
        b_education_low_pf * education_low +
        b_education_medium_pf * education_medium +
        b_rank_low * rank_low +
        b_rank_medium * rank_medium +
        b_income_low * income_low +
        b_income_medium * income_medium +
        b_married * married + 
        b_children * children + 
        b_mental_health + mental_health
        error

# Does a linear regression recover the coefficient?
lm(PF_3 ~ mi)



