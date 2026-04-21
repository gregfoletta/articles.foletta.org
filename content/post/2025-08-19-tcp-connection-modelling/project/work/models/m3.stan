// Model: m3 — Hierarchical NegBin for new connections AND terminations
// Structure: Independent hierarchical NegBin for each of 4 outcomes:
//            tcp_new, udp_new, tcp_end, udp_end.
//            Each outcome has its own (alpha, sigma, z[K], phi).
//            Non-centered parameterization throughout.
// Purpose: Jointly model all four quantities needed for firewall sizing:
//          new connections (session creation), terminations (log volume),
//          and (derived) concurrent connections.

data {
  int<lower=1> T;                    // total seconds across all machines
  int<lower=1> K;                    // number of machines
  array[T] int<lower=1,upper=K> machine;
  array[T] int<lower=0> tcp_new;
  array[T] int<lower=0> udp_new;
  array[T] int<lower=0> tcp_end;
  array[T] int<lower=0> udp_end;
}

parameters {
  // Population-level log-rate means
  real alpha_tcp_new;
  real alpha_udp_new;
  real alpha_tcp_end;
  real alpha_udp_end;

  // Between-machine SD on log-rate
  real<lower=0> sigma_tcp_new;
  real<lower=0> sigma_udp_new;
  real<lower=0> sigma_tcp_end;
  real<lower=0> sigma_udp_end;

  // Non-centered machine effects
  vector[K] z_tcp_new;
  vector[K] z_udp_new;
  vector[K] z_tcp_end;
  vector[K] z_udp_end;

  // Overdispersion (shared across machines, per outcome)
  real<lower=0> phi_tcp_new;
  real<lower=0> phi_udp_new;
  real<lower=0> phi_tcp_end;
  real<lower=0> phi_udp_end;
}

transformed parameters {
  vector<lower=0>[K] mu_tcp_new = exp(alpha_tcp_new + sigma_tcp_new * z_tcp_new);
  vector<lower=0>[K] mu_udp_new = exp(alpha_udp_new + sigma_udp_new * z_udp_new);
  vector<lower=0>[K] mu_tcp_end = exp(alpha_tcp_end + sigma_tcp_end * z_tcp_end);
  vector<lower=0>[K] mu_udp_end = exp(alpha_udp_end + sigma_udp_end * z_udp_end);
}

model {
  // Population-level priors on log-rate means
  alpha_tcp_new ~ normal(-0.9, 1);
  alpha_udp_new ~ normal(-0.9, 1);
  alpha_tcp_end ~ normal(-0.9, 1);  // terminations have similar rates
  alpha_udp_end ~ normal(-0.9, 1);

  // Between-machine SD priors (mildly informative, K=5)
  sigma_tcp_new ~ normal(0, 1);
  sigma_udp_new ~ normal(0, 1);
  sigma_tcp_end ~ normal(0, 1);
  sigma_udp_end ~ normal(0, 1);

  // Non-centered machine effects
  z_tcp_new ~ std_normal();
  z_udp_new ~ std_normal();
  z_tcp_end ~ std_normal();
  z_udp_end ~ std_normal();

  // Overdispersion priors
  phi_tcp_new ~ exponential(0.5);
  phi_udp_new ~ exponential(0.5);
  phi_tcp_end ~ exponential(0.5);
  phi_udp_end ~ exponential(0.5);

  // Likelihood
  for (t in 1:T) {
    int k = machine[t];
    tcp_new[t] ~ neg_binomial_2(mu_tcp_new[k], phi_tcp_new);
    udp_new[t] ~ neg_binomial_2(mu_udp_new[k], phi_udp_new);
    tcp_end[t] ~ neg_binomial_2(mu_tcp_end[k], phi_tcp_end);
    udp_end[t] ~ neg_binomial_2(mu_udp_end[k], phi_udp_end);
  }
}

// No generated quantities — PPC replicates generated in R post-hoc
// to avoid OOM from 4 * T arrays in memory during sampling.
