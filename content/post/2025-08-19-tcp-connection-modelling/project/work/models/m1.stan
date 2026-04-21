// Model: m1 — Pooled Negative Binomial for TCP and UDP CPS (multi-machine data)
// Structure: Independent NegBin(mu, phi) for tcp_new and udp_new, fully pooled
//            across K machines. Machine index included in data for future use.
// Priors: Weakly informative, scaled to observed data (TCP mean ~0.4, UDP mean ~0.4)
// Notes: Baseline model. Expected to underfit between-machine heterogeneity,
//        especially for TCP where one machine (mb_cap) has 16x higher rate.

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
  // Mean rates (must be positive)
  real<lower=0> mu_tcp;
  real<lower=0> mu_udp;

  // Overdispersion parameters (larger phi = less overdispersion)
  real<lower=0> phi_tcp;
  real<lower=0> phi_udp;
}

model {
  // Prior on mu_tcp: Exponential(1) — mean 1, puts 95% mass below 3.
  // Observed pooled TCP mean is 0.42; this prior is weakly informative,
  // allowing rates up to ~3 while gently regularising toward lower values.
  mu_tcp ~ exponential(1);

  // Prior on mu_udp: same rationale; observed pooled UDP mean is 0.39.
  mu_udp ~ exponential(1);

  // Prior on phi_tcp: Exponential(0.5) — mean 2. Observed VMR ~5 implies
  // phi ~ mu/(VMR-1) ~ 0.1, but we want a weakly informative prior that
  // allows both small phi (high overdispersion) and moderate phi.
  phi_tcp ~ exponential(0.5);

  // Prior on phi_udp: same rationale; observed VMR ~8.5 implies phi ~ 0.05.
  phi_udp ~ exponential(0.5);

  // Likelihood
  tcp_new ~ neg_binomial_2(mu_tcp, phi_tcp);
  udp_new ~ neg_binomial_2(mu_udp, phi_udp);
}

generated quantities {
  // Posterior predictive draws for PPC
  array[T] int<lower=0> tcp_new_rep;
  array[T] int<lower=0> udp_new_rep;

  for (t in 1:T) {
    tcp_new_rep[t] = neg_binomial_2_rng(mu_tcp, phi_tcp);
    udp_new_rep[t] = neg_binomial_2_rng(mu_udp, phi_udp);
  }
}
