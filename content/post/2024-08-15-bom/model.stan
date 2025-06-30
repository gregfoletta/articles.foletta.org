data {
  //Training data
  int<lower=1> n;
  vector[n] jaggedness;
  array[n] int <lower=1, upper=6> duration;
  vector[n] mean_forecast_error;
  
  //Out of sample test set
  vector[n] jaggedness_t;
  array[n] int <lower=1, upper=6> duration_t;
  vector[n] mean_forecast_error_t;
    }

parameters {
  vector[6] alpha;
  vector[6] beta_jaggedness;
  real<lower=0> sigma;
    }

model {
  // Weakly informative priors
  beta_jaggedness ~ normal(0, 5);
  alpha ~ normal(0, 5);
  sigma ~ exponential(1);

  // Likelihood
  for (i in 1:n) {
    mean_forecast_error[i] ~ normal(alpha[duration[i]] + beta_jaggedness[duration[i]] * jaggedness[i], sigma);
  }
}

generated quantities {
  //Posterior predictive check
  vector[n] y_rep;
  //Out of sample test set
  vector[n] y_test;

  for (i in 1:n) {
    //Posterior predictive checks
    y_rep[i] = normal_rng(
      alpha[duration[i]] + beta_jaggedness[duration[i]] * jaggedness[i],
      sigma
    );
   
    //Out of sample checks 
    y_test[i] = normal_rng(
      alpha[duration_t[i]] + beta_jaggedness[duration_t[i]] * jaggedness_t[i],
      sigma
    );
  }
}
