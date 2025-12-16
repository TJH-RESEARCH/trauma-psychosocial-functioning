
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
# https://mc-stan.org/cmdstanr/
# 
# we recommend running this in a fresh R session or restarting your current session
# install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))