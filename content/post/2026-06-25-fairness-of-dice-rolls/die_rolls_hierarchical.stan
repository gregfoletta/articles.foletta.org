data {
  int<lower=1> n;
  array[n] int<lower=1, upper=6> roll;
}
parameters {
  real<lower=0> sigma;
  vector[6] eta_raw;
}
transformed parameters {
  vector[6] eta = sigma * eta_raw;
  simplex[6] theta = softmax(eta);
}
model {
  sigma   ~ normal(0, 0.5);
  eta_raw ~ std_normal();
  roll    ~ categorical(theta);
}
