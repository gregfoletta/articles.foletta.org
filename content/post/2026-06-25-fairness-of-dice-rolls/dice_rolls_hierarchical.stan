data {
  int<lower=1> n;
  array[n] int<lower=1, upper=6> roll;
}
parameters {
  real<lower=0> sigma;
  vector[6] eta;
}
transformed parameters {
  simplex[6] theta = softmax(eta);
}
model {
  sigma ~ normal(0, 0.5);
  eta   ~ normal(0, sigma);
  roll  ~ categorical(theta);
}

