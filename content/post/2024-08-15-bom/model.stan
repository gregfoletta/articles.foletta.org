data {
  int<lower=1> n;                          // number of observations
  vector[n] jaggedness;                   // continuous predictor
  array[n] int <lower=1, upper=6> duration;      // categorical predictor (indexed)
  vector[n] mean_forecast_error;               // outcome variable
}

parameters {
  vector[6] alpha;                        // intercepts for each category
  vector[6] beta_jaggedness;                   // slope for jaggedness
  real<lower=0> sigma;                    // residual SD
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
  vector[n] y_rep;                        // replicated forecast_error

  for (i in 1:n) {
    y_rep[i] = normal_rng(
      alpha[duration[i]] + beta_jaggedness[duration[i]] * jaggedness[i],
      sigma
    );
  }
}
