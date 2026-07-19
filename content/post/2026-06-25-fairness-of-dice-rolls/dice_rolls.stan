data {
  int<lower=1> n;
  array[n] int<lower=1, upper=6> roll;
  real<lower=0> alpha;            // prior concentration; >1 = belief in fairness
}

parameters {
  simplex[6] theta;
}

model {
  theta ~ dirichlet(rep_vector(alpha, 6));
  roll  ~ categorical(theta);     // vectorised over the array
}
