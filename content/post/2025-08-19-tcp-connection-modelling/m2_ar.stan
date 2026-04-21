data {
  int<lower=1> t;
  int<lower=1> n_p;
  array[t] int<lower=1, upper=n_p> protocol;
  array[t] int<lower=0> new_cps;
  array[t] int<lower=0> complete_cps;
  int<lower=0> c0;
}

parameters {
  vector<lower=0>[n_p] sigma_proc;
  vector<lower=0, upper=1>[n_p] rho;
  vector[n_p] alpha;
  matrix[t, n_p] latent_c;
}

model {
  rho ~ beta(2,2);
  sigma_proc ~ normal(0, 5);
  alpha ~ normal(0, 10);

  // Initial condition
  for (p in 1:n_p)
    latent_c[1, p] ~ normal(c0, 10);

  // AR(1) state evolution
  for (i in 2:t) {
    int p = protocol[i];
    real net = new_cps[i] - complete_cps[i];
    latent_c[i, p] ~ normal(alpha[p] + rho[p] * latent_c[i-1, p] + net,
                            sigma_proc[p]);
  }
}