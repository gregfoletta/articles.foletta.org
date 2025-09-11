data {
    int<lower=1> t;
    int<lower=1> n_p;
    int<lower=1, upper=2> p_index[t];
    int <lower=0> new_cps[n_p, t];
    int <lower=0> complete_cps[n_p, t];
    int<lower=0> c0;
}

parameters {
}

model {
}

generated quantities {
}
