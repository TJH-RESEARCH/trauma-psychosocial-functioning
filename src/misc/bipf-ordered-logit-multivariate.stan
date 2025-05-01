data {
    int < lower = 2 > n_bipf_category;                                   // Factor levels in outcome variable 
    int < lower = 1 > n;                                                 // Sample size
    int < lower = 1 > n_predictors;                                      // Number of predictors
    array[n] int < lower = 1, upper = n_bipf_category > bipf_category;   // Outcome
    matrix[n, n_predictors] x;                                           // Design Matrix
  }
  

  parameters {
    vector[n_predictors] beta;   // Regression coefficient
    ordered[n_bipf_category - 1] cutpoints; 
  }
  
  
  transformed parameters{
  vector[n] log_prob = x * beta;
}


  model {
  
    // Priors
    beta ~ cauchy(0, 2.5);
    
    // Logit Model
      bipf_category ~ ordered_logistic(log_prob, cutpoints);
  
  }
 
 
  generated quantities{
    vector[n] log_lik;        
    real y_pred[n];
    
    for (i in 1:n){
  
      //log-likelihood
      log_lik[i] = ordered_logistic_lpmf(bipf_category[i] | log_prob[i], cutpoints);
    
       //predictions
       y_pred[i] = ordered_logistic_rng(log_prob[i], cutpoints);
    }
}
