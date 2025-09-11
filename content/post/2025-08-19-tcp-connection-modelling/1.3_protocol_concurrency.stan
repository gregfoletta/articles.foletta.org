data {
    int<lower=1> t;
    int<lower=1> n_p;
    int<lower=1, upper=2> p_index[t];
    int <lower=0> new_cps[n_p, t];
    int <lower=0> complete_cps[n_p, t];
    int<lower=0> c0;
}

parameters {
  real<lower=0> mu_n[n_p];
  real<lower=0> phi_n[n_p];
  real<lower=0, upper=1> prev_complete[n_p]; 
}

model {
    mu_n ~ lognormal(0, 1.5);
    phi_n ~ exponential(1);

    prev_complete ~ beta(1,1);
   
    for (i in 1:t) {
        new_cps[i] ~ neg_binomial_2(mu_n[ p_index[i] ], phi_n[ p_index[i] ]);
    }
    
    int c_prev;
    for (p in 1:n_p) {
        c_prev = c0;
        for (i in 2:t) {
            //Need to include the current second new connections, as 
            //they may also finish within the second
            c_prev = c_prev + new_cps[p, i];
            complete_cps[p, i] ~ binomial(c_prev, prev_complete[p]);
            c_prev = c_prev - complete_cps[p, i];
        }
    }
}

generated quantities {
//    array[t] int new_cps_rep;
//    array[t] int complete_cps_rep;
//    vector[t] c_pred;
//    
//    for (i in 1:t) {
//        new_cps_rep[i] = neg_binomial_2_rng(mu_n, phi_n);
//    }
//    
//    c_pred[1] = c0;
//    int c_prev = c0;
//    for (i in 2:t) {
//        c_prev = c_prev + new_cps_rep[i];
//        complete_cps_rep[i] = binomial_rng(c_prev, prev_complete);
//        c_prev = c_prev - complete_cps_rep[i];
//        c_pred[i] = c_prev;
//    }
}
