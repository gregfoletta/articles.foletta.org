data {
    int<lower=1> t;
    array[t] int<lower=0> new_cps;
    array[t] int<lower=0> complete_cps;
    int<lower=0> c0;
}

parameters {
  real<lower=0> mu_n;
  real<lower=0> phi_n;
  real<lower=0, upper=1> prev_complete; 
}

model {
    mu_n ~ lognormal(0, 1.5);
    phi_n ~ exponential(1);

    prev_complete ~ beta(1,1);

    new_cps ~ neg_binomial_2(mu_n, phi_n);
     
    int c_prev = new_cps[1]; 
    for (i in 2:t) {
        //Need to include the current second new connections, as 
        //they may also finish within the second
        c_prev = c_prev + new_cps[i];
        complete_cps[i] ~ binomial(c_prev, prev_complete);
        c_prev = c_prev - complete_cps[i];
    }
}

generated quantities {
    array[t] int new_cps_rep;
    array[t] int complete_cps_rep;
    vector[t] c_pred;
    
    for (i in 1:t) {
        new_cps_rep[i] = neg_binomial_2_rng(mu_n, phi_n);
    }
    
    c_pred[1] = c0;
    int c_prev = c0;
    for (i in 2:t) {
        c_prev = c_prev + new_cps_rep[i];
        complete_cps_rep[i] = binomial_rng(c_prev, prev_complete);
        c_prev = c_prev - complete_cps_rep[i];
        c_pred[i] = c_prev;
    }
}
