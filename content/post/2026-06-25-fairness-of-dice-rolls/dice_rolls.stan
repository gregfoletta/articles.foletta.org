data {
  int<lower=1> n;
  array[n] int<lower=1, upper=6> roll;
  real alpha; 
}

parameters {
  simplex[6] theta;
}

model {
  theta ~ dirichlet(rep_vector(alpha, 6));
  roll  ~ categorical(theta);
}
