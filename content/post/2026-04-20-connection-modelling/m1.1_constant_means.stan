data {
    int<lower=1> t;
    array[t] int<lower=0> tcp_new;
    array[t] int<lower=0> tcp_end;
    real<lower=0> c0;
}

parameters {
  real<lower=0> mu_new;
  real<lower=0> mu_end;

  real<lower=0> phi_new;
  real<lower=0> phi_end;
}

model {
    mu_new ~ lognormal(0, 0.5);
    mu_end ~ lognormal(0, 0.5);
    phi_new ~ exponential(1);
    phi_end ~ exponential(1);

    tcp_new ~ neg_binomial_2(mu_new, phi_new);
    tcp_end ~ neg_binomial_2(mu_end, phi_end);
}

generated quantities {
    array[t] int tcp_new_rep;
    array[t] int tcp_end_rep;
    vector[t] total_pred;
    
    for (i in 1:t) {
        tcp_new_rep[i] = neg_binomial_2_rng(mu_new, phi_new);
        tcp_end_rep[i] = neg_binomial_2_rng(mu_end, phi_end);
    }
}
