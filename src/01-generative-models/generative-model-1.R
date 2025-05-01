
# Simulate Data


# DAG 1 -------------------------------------------------------------------

## Sample size
n <- 100 

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




