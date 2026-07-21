data {
  int<lower=1> n;
  array[n] int<lower=1, upper=6> roll;
}

parameters {
  simplex[6] theta;
}

model {
  theta ~ dirichlet(rep_vector(1, 6));
  roll  ~ categorical(theta);     // vectorised over the array
}
