// Model: m1 — Pooled Negative Binomial for TCP and UDP CPS (multi-machine data)
//
// Structure:
//   Four independent Negative Binomial likelihoods for the four CPS series:
//   tcp_new, tcp_end, udp_new, udp_end. Each series is fully pooled across
//   machines (single (mu, phi) pair per series). The data block carries
//   K and machine[] so the next iteration can introduce machine-level
//   parameters without changing the data pipeline.
//
// Priors:
//   log_mu_*  ~ normal(-1, 2)    — weakly informative; rate centered on
//                                  ~exp(-1)=0.37 cps, 95% prior on log_mu
//                                  covers [-5, 3] -> rate in [0.007, 20].
//                                  Span comfortably covers all per-machine
//                                  empirical means (0.06–0.98 cps).
//   phi_*     ~ gamma(2, 0.1)    — weakly informative on NB dispersion;
//                                  mean=20, supports both Poisson-like
//                                  (large phi) and strongly overdispersed
//                                  (small phi). The prior mode (~10) admits
//                                  the var/mean ratios (2–11) seen in data.
//
// Notes:
//   - Pooling across machines is deliberately simplistic for this bootstrap
//     iteration. Empirical between-machine rate differences are ~16x for TCP
//     and ~2x for UDP; we expect the pooled fit to fail to capture per-
//     machine rate heterogeneity, which the PPC will reveal.
//   - Generated quantities reproduce all four series for full PPC.
//   - neg_binomial_2 parameterisation: mean=mu, variance=mu + mu^2 / phi.

data {
  int<lower=1> T;                       // total seconds across all machines
  int<lower=1> K;                       // number of machines
  array[T] int<lower=1, upper=K> machine; // machine index per second (1..K)
  array[T] int<lower=0> tcp_new;
  array[T] int<lower=0> tcp_end;
  array[T] int<lower=0> udp_new;
  array[T] int<lower=0> udp_end;
}

parameters {
  // Log-rates (one per series, fully pooled)
  real log_mu_tcp_new;
  real log_mu_tcp_end;
  real log_mu_udp_new;
  real log_mu_udp_end;

  // NB dispersion parameters (one per series)
  real<lower=0> phi_tcp_new;
  real<lower=0> phi_tcp_end;
  real<lower=0> phi_udp_new;
  real<lower=0> phi_udp_end;
}

transformed parameters {
  real<lower=0> mu_tcp_new = exp(log_mu_tcp_new);
  real<lower=0> mu_tcp_end = exp(log_mu_tcp_end);
  real<lower=0> mu_udp_new = exp(log_mu_udp_new);
  real<lower=0> mu_udp_end = exp(log_mu_udp_end);
}

model {
  // Priors on log-rates: weakly informative around log(rate) = -1
  log_mu_tcp_new ~ normal(-1, 2);
  log_mu_tcp_end ~ normal(-1, 2);
  log_mu_udp_new ~ normal(-1, 2);
  log_mu_udp_end ~ normal(-1, 2);

  // Priors on dispersions: gamma(2, 0.1) — mean=20, mode=10
  phi_tcp_new ~ gamma(2, 0.1);
  phi_tcp_end ~ gamma(2, 0.1);
  phi_udp_new ~ gamma(2, 0.1);
  phi_udp_end ~ gamma(2, 0.1);

  // Likelihoods
  tcp_new ~ neg_binomial_2(mu_tcp_new, phi_tcp_new);
  tcp_end ~ neg_binomial_2(mu_tcp_end, phi_tcp_end);
  udp_new ~ neg_binomial_2(mu_udp_new, phi_udp_new);
  udp_end ~ neg_binomial_2(mu_udp_end, phi_udp_end);
}

generated quantities {
  array[T] int tcp_new_rep;
  array[T] int tcp_end_rep;
  array[T] int udp_new_rep;
  array[T] int udp_end_rep;
  for (t in 1:T) {
    tcp_new_rep[t] = neg_binomial_2_rng(mu_tcp_new, phi_tcp_new);
    tcp_end_rep[t] = neg_binomial_2_rng(mu_tcp_end, phi_tcp_end);
    udp_new_rep[t] = neg_binomial_2_rng(mu_udp_new, phi_udp_new);
    udp_end_rep[t] = neg_binomial_2_rng(mu_udp_end, phi_udp_end);
  }
}
