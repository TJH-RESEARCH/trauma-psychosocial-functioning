
# Simulate Data


# DAG 2 -------------------------------------------------------------------

## Sample size
n <- 100 

## Coefficients
b_mi_pf <- .1
b_ptsd_pf <- .1
b_osi_pf <- .1


## Gender
b_female <- .1
p_female <- .5
female <- rbinom(n, 1, prob = p_female)

## Service Era
b_era_911 <- .1
b_era_persian <- .1
b_era_viet <- .1
  
### Probability of being in each service era
p_era_911 <- .34
p_era_persian <- .33
p_era_viet <- .33

era <- sample(c('vietnam', 'persian', '911'), size = n, replace = TRUE, prob = c(p_era_viet, p_era_persian, p_era_911))
era_viet <- ifelse(era == 'vietnam', 1, 0)
era_911 <- ifelse(era == '911', 1, 0)
era_persian <- ifelse(era == 'persian', 1, 0)

## Race
b_race_white <- .1
b_race_black <- .1
b_race_latino <- .1
b_race_asian <- .1
b_race_native <- .1
b_race_other <- .1

p_race_asian <- .1
p_race_black <- .1
p_race_latino <- .1
p_race_native <- .1
p_race_other <- .1
p_race_white <- .1

race <- sample(c('asian', 'black', 'latino', 'native', 'other', 'white'), size = n, replace = TRUE, prob = c(p_race_asian, p_race_black, p_race_latino, p_race_native, p_race_other, p_race_white))

race_asian <- ifelse(race == 'asian', 1, 0)
race_black <- ifelse(race == 'black', 1, 0)
race_latino <- ifelse(race == 'latino', 1, 0)
race_native <- ifelse(race == 'native', 1, 0)
race_other <- ifelse(race == 'other', 1, 0)
race_white <- ifelse(race == 'white', 1, 0)

## Combat
#### helper function to regularize between 0 and 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

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


## Mental Health
mi <- b_combat * combat + b_era_viet * era_viet  + 
  b_era_persian * era_persian +
  b_female * female + 
  b_race_asian * race_asian + 
  b_race_black * race_black + 
  b_race_latino * race_latino + 
  b_race_native * race_native + 
  b_race_white * race_white + rnorm(n, mean = 0, sd = 1)
ptsd <- b_combat * combat + b_era_viet * era_viet  + 
  b_era_persian * era_persian +
  b_female * female + 
  b_race_asian * race_asian + 
  b_race_black * race_black + 
  b_race_latino * race_latino + 
  b_race_native * race_native + 
  b_race_white * race_white + rnorm(n, mean = 0, sd = 1)
osi <- b_combat * combat + b_era_viet * era_viet  + 
  b_era_persian * era_persian +
  b_female * female + 
  b_race_asian * race_asian + 
  b_race_black * race_black + 
  b_race_latino * race_latino + 
  b_race_native * race_native + 
  b_race_white * race_white + rnorm(n, mean = 0, sd = 1)
error <- rnorm(n, mean = 0, sd = 1)

## Psychosocial functioning formula
PF_2 <- b_mi_pf * mi + b_ptsd_pf * ptsd + b_osi_pf * osi + 
        b_era_viet * era_viet  + 
        b_era_persian * era_persian +
        b_female * female + 
        b_race_asian * race_asian + 
        b_race_black * race_black + 
        b_race_latino * race_latino + 
        b_race_native * race_native + 
        b_race_white * race_white + 
        error

# Does a linear regression recover the coefficient?
lm(PF_2 ~ mi)



