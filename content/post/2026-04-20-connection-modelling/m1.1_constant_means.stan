data {
    int<lower=1> t;
    array[t] int<lower=0> new_cps;
    array[t] int<lower=0> complete_cps;
    real<lower=0> c0;
}

parameters {
  real<lower=0> mu_n;
  real<lower=0> mu_c;

  real<lower=0> phi_n;
  real<lower=0> phi_c;
}

model {
    mu_n ~ lognormal(0, 0.5);
    mu_c ~ lognormal(0, 0.5);
    phi_n ~ exponential(1);
    phi_c ~ exponential(1);

    new_cps ~ neg_binomial_2(mu_n, phi_n);
    complete_cps ~ neg_binomial_2(mu_c, phi_c);
}

generated quantities {
    array[t] int new_cps_rep;
    array[t] int complete_cps_rep;
    vector[t] c_pred;
    
    for (i in 1:t) {
        new_cps_rep[i] = neg_binomial_2_rng(mu_n, phi_n);
        complete_cps_rep[i] = neg_binomial_2_rng(mu_c, phi_c);
    }
    
    c_pred[1] = c0;
    for (i in 2:t) {
        c_pred[i] = c_pred[i - 1] + new_cps_rep[i] - complete_cps_rep[i];
    }
}
