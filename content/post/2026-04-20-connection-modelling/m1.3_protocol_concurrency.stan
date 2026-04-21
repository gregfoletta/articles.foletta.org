data {
    int<lower=1> t;
    int<lower=1> n_p;
    array[t] int<lower=1, upper=2> protocol;
    array[t] int<lower=0> new_cps;
    array[t] int<lower=0> complete_cps;
    int<lower=0> c0;
}

parameters {
  vector<lower = 0>[n_p] mu_n;
  vector<lower = 0>[n_p] phi_n;
  vector<lower=0, upper=1>[n_p] prev_complete;
}


model {
    mu_n ~ lognormal(0, 1.5);
    phi_n ~ exponential(1);
    prev_complete ~ beta(1,1);

    new_cps ~ neg_binomial_2(mu_n[ protocol ], phi_n[ protocol ]);
    
    
    array[n_p] int c_prev;
    for (n in 1:n_p) {
        c_prev[n] = c0; 
    }
    
    for (i in 1:t) {
        int p = protocol[i];
        
        c_prev[ p ] = c_prev[ p ] + new_cps[i];
        complete_cps[i] ~ binomial(c_prev[ p ], prev_complete[ protocol[i] ]);
        c_prev[ p ] = c_prev[ p ] - complete_cps[ i ];
    }
}

generated quantities {
    array[n_p, t] int c_pred;
    
    {
        array[t] int new_cps_rep;
        array[t] int complete_cps_rep;
        int c_prev;
    
        for (p in 1:n_p) {
            c_prev = c0;
    
            for (i in 1:600) {
                new_cps_rep[i] = neg_binomial_2_rng(mu_n[p], phi_n[p]);
                
                c_prev = c_prev + new_cps_rep[i];
                complete_cps_rep[i] = binomial_rng(c_prev, prev_complete[p]);
                c_prev = c_prev - complete_cps_rep[i];
                c_pred[p, i] = c_prev;
            }
        }
    }
}
