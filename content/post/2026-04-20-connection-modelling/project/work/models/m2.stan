// Model: m2 — Hierarchical Negative Binomial for TCP and UDP CPS
// Structure: Machine-level random effects on log-rate, non-centered parameterization.
//            Each machine k has its own rate: mu_k = exp(alpha + sigma * z_k)
//            Shared overdispersion phi across machines (per protocol).
// Priors: Weakly informative on population mean and between-machine SD.

data {
  int<lower=1> T;                    // total seconds across all machines
  int<lower=1> K;                    // number of machines
  array[T] int<lower=1,upper=K> machine;  // machine index for each second
  array[T] int<lower=0> tcp_new;     // new outbound TCP connections per second
  array[T] int<lower=0> udp_new;     // new outbound UDP flows per second
  array[T] int<lower=0> tcp_end;     // TCP terminations per second
  array[T] int<lower=0> udp_end;     // UDP flow endings per second
}

parameters {
  // Population-level log-rate means
  real alpha_tcp;
  real alpha_udp;

  // Between-machine SD on log-rate
  real<lower=0> sigma_tcp;
  real<lower=0> sigma_udp;

  // Non-centered machine effects (standard normal)
  vector[K] z_tcp;
  vector[K] z_udp;

  // Overdispersion parameters (shared across machines, per protocol)
  real<lower=0> phi_tcp;
  real<lower=0> phi_udp;
}

transformed parameters {
  // Machine-level rates
  vector<lower=0>[K] mu_tcp = exp(alpha_tcp + sigma_tcp * z_tcp);
  vector<lower=0>[K] mu_udp = exp(alpha_udp + sigma_udp * z_udp);
}

model {
  // Population-level priors
  // alpha ~ normal(log(0.4), 1) centers on observed pooled mean
  alpha_tcp ~ normal(-0.9, 1);
  alpha_udp ~ normal(-0.9, 1);

  // Between-machine SD: half-normal, mildly informative to regularise the
  // funnel geometry with only K=5 groups. Observed log-rate range ~2.8
  // implies SD ~1 is plausible; normal(0,1) keeps 95% mass below 2.
  sigma_tcp ~ normal(0, 1);
  sigma_udp ~ normal(0, 1);

  // Non-centered machine effects
  z_tcp ~ std_normal();
  z_udp ~ std_normal();

  // Overdispersion priors (same as m1)
  phi_tcp ~ exponential(0.5);
  phi_udp ~ exponential(0.5);

  // Likelihood — vectorised per machine
  for (t in 1:T) {
    tcp_new[t] ~ neg_binomial_2(mu_tcp[machine[t]], phi_tcp);
    udp_new[t] ~ neg_binomial_2(mu_udp[machine[t]], phi_udp);
  }
}

generated quantities {
  // Posterior predictive draws for PPC
  array[T] int<lower=0> tcp_new_rep;
  array[T] int<lower=0> udp_new_rep;

  // Population-level mean (for reporting)
  real mu_tcp_pop = exp(alpha_tcp + square(sigma_tcp) / 2);
  real mu_udp_pop = exp(alpha_udp + square(sigma_udp) / 2);

  for (t in 1:T) {
    tcp_new_rep[t] = neg_binomial_2_rng(mu_tcp[machine[t]], phi_tcp);
    udp_new_rep[t] = neg_binomial_2_rng(mu_udp[machine[t]], phi_udp);
  }
}
