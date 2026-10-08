data {
  int<lower=1> n;
  array[n] int<lower=1, upper=6> roll;
  vector[6] alphas; 
}

parameters {
  simplex[6] theta;
}

model {
  theta ~ dirichlet(alphas);
  roll  ~ categorical(theta);
}
