// Lower censored tobit model - tobit_lower_flat.stan
data {
  int<lower=0> N_obs;          // Number of observed data points
  int<lower=0> N_cens;         // Number of censored data points
  int<lower=0> K;              // Number of predictors
  matrix[N_obs, K] X_obs;      // Observed predictor matrix
  matrix[N_cens, K] X_cens;    // Censored predictor matrix
  real y_obs[N_obs];           // Observations (non-censored)
  real y_cens[N_cens];           // Observations (non-censored)
  real<upper=min(y_obs)> L;    // Value where censoring occurs
  
}

transformed data {
  real<lower=0> mean_y_obs = mean(to_vector(y_obs));
  real<lower=0> sd_y_obs = sd(to_vector(y_obs));
  real<lower=0> mean_y_cens = mean(to_vector(y_cens));
  real<lower=0> sd_y_cens = sd(to_vector(y_cens));
  }

parameters {
  vector[K] beta;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
}

model {
  y_obs ~ normal(X_obs * beta, sigma);  // target denasity - observed points
  y_cens ~ normal(X_cens * beta, sigma); // target density - censored points
}

generated quantities {
    real y_obs_rep[N_obs] = normal_rng(X_obs * beta, sigma);
    real y_cens_rep[N_cens] = normal_rng(X_cens * beta, sigma);
    
    real mean_y_obs_rep = mean(to_vector(y_obs_rep));
    real mean_y_cens_rep = mean(to_vector(y_cens_rep));
    
    real<lower=0> sd_y_obs_rep = sd(to_vector(y_obs_rep));
    real<lower=0> sd_y_cens_rep = sd(to_vector(y_cens_rep));
    
    
    int<lower=0, upper=1> mean_gte_y_obs = (mean_y_obs_rep >= mean_y_obs);
    int<lower=0, upper=1> mean_gte_y_cens = (mean_y_cens_rep >= mean_y_cens);
    
    int<lower=0, upper=1> sd_gte_y_obs = (sd_y_obs_rep >= sd_y_obs);
    int<lower=0, upper=1> sd_gte_y_cens = (sd_y_cens_rep >= sd_y_cens);
  }