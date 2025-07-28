
# Simulate Data

##### helper function to regularize between 0 and 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# DAG 1 -------------------------------------------------------------------
## Sample size
n <- 1000 

b_ind_mil <- 3

b_ind_tr <- 2
b_mil_tr <- 4

b_ind_pts <- 2
b_mil_pts <- 1
b_tr_pts <- 5

b_ind_pf <- 1
b_mil_pf <- 1
b_tr_pf <- 2
b_pts_pf <- 5


ind <- rnorm(n, mean = 0, 2)
mil <- rbinom(n, size = 1, 
              prob = range01(
                b_ind_mil * ind +            # Individual Characteristics on Military Service
                rnorm(n, mean = 0, sd = 1)   # Error
              )) 
tr <- rbinom(n, size = 1, 
                prob = range01(
                  b_ind_tr * ind +            # Individual Characteristics on Trauma Exposure 
                  b_mil_tr * mil +            # Military Service on Trauma Exposure 
                  rnorm(n, mean = 0, sd = 1)  # Error
                )) 
pts <- rnorm(n, mean = 
                  b_ind_pts * ind +           # Individual Characteristics on Post-Traumatic Stress
                  b_mil_pts * mil +           # Military Service on Post-Traumatic Stress
                  b_tr_pts  * tr  +           # Trauma Exposure on Post-Traumatic Stress
                  rnorm(n, mean = 0, sd = 1), # Error
             sd = 1
              ) 
pf <- rnorm(n,
             mean =
                  b_ind_pf * ind +            # Individual Characteristics on Psychosocial Functioning
                  b_mil_pf * mil +            # Military Service on Psychosocial Functioning
                  b_tr_pf  * tr  +            # Trauma Exposure on Psychosocial Functioning
                  b_pts_pf * pts +            # Post-Traumatic Stress on Psychosocial Functioning
                  rnorm(n, mean = 0, sd = 1), # Error
             sd = 1 
             ) 

lm(pf ~ pts)
lm(pf ~ pts + ind + mil + tr)


# DAG 2 -------------------------------------------------------------------


## Coefficients
b_mi_pf <- .1
b_ptsd_pf <- .1
b_osi_pf <- .1
b_combat <- .1
p_combat <- .5

## Observed
combat <- rbinom(n, 1, prob = p_combat)
mi <- b_combat * combat + rnorm(n, mean = 0, sd = 1)
ptsd <- b_combat * combat + rnorm(n, mean = 0, sd = 1)
osi <- b_combat * combat + rnorm(n, mean = 0, sd = 1)
error <- rnorm(n, mean = 0, sd = 1)

## Psychosocial functioning formula
PF_1 <- b_mi_pf * mi + b_ptsd_pf * ptsd + b_osi_pf * osi + error




