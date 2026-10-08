data {
  int<lower=1> n;
  array[n] int<lower=1, upper=6> roll;
}
parameters {
  real<lower=0> sigma;
  sum_to_zero_vector[6] eta_raw;
}
transformed parameters {
  vector[6] eta = sigma * sqrt(6.0 / 5.0) * eta_raw;
  simplex[6] theta = softmax(eta);
}
model {
  sigma ~ normal(0, 0.5);
  eta_raw   ~ std_normal();
  roll  ~ categorical_logit(theta);
}

