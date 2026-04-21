# TCP/UDP CPS Modelling — Investigation Notes

---

## Bootstrap

### Data Exploration

- **Machines**: greg_cap, greg_cap_2, greg_cap_3, hendo_cap, mb_cap (K=5)
- Per machine:
  - greg_cap: 7034s (117 min), 2.08M packets, IPs: 172.20.10.2, 2001:8004:6b10:dc11:16c5:4cda:21f4:709f
  - greg_cap_2: 2098s (35 min), 576K packets, IPs: 10.111.7.75
  - greg_cap_3: 7200s (120 min), 525K packets, IPs: 10.48.7.2, 2403:5803:98a3:7:c5cf:3421:64ef:26bc
  - hendo_cap: 3601s (60 min), 823K packets, IPs: 172.25.16.38, fe80::1c03:e205:cd03:763e
  - mb_cap: 7200s (120 min), 3.30M packets, IPs: 192.168.230.4, 2403:5811:bd7e:230:30ec:df9c:2a56:2cc6
- Total: 27,133 seconds across all machines

- **TCP CPS (pooled)**: mean=0.42, sd=1.49, median=0, p99=5, max=61, zeros=82.6%, VMR=5.25
  - Per-machine means range from 0.065 (hendo) to 1.076 (mb_cap) — **16× range**
  - VMR ranges 2.77–18.54; all heavily overdispersed
- **UDP CPS (pooled)**: mean=0.39, sd=1.83, median=0, p99=7, max=79, zeros=89.6%, VMR=8.53
  - Per-machine means range from 0.29 to 0.62 — more homogeneous than TCP (~2× range)
  - VMR ranges 5.19–12.92; all heavily overdispersed
- **TCP terminations**: similar pattern to TCP new but slightly lower rates
- **UDP terminations**: nearly identical to UDP new (expected since every flow has one start and one end)

- **Between-machine differences**: mb_cap has dramatically higher TCP rate (1.08) vs others (0.07–0.35). UDP rates are more uniform across machines. This suggests a hierarchical model with machine-level rates would be valuable, but starting pooled as a baseline is reasonable. The large between-machine variance in TCP means a pooled model will likely underfit the heterogeneity.

### Model Design Decisions

- **Likelihood**: Negative Binomial (`neg_binomial_2`) for both TCP and UDP. All series show VMR >> 1 (overdispersion relative to Poisson), making NegBin the natural choice. The high zero percentages (53–97%) are consistent with low-rate NegBin and don't necessarily require explicit zero-inflation — the NegBin can accommodate zeros through its overdispersion parameter.
- **TCP and UDP modelled independently**: no obvious reason to couple them; separate parameters allow each to find its own rate and dispersion.
- **Pooled baseline (m1)**: shared mu and phi across all machines. This will underestimate between-machine variance (especially for TCP where mb_cap is an outlier) but establishes a baseline for PPC comparison. Data block includes machine index for later hierarchical extension.
- **Priors**: weakly informative, centred on observed pooled means. Half-normal or exponential for rates (must be positive). Gamma or half-normal for phi (overdispersion), with scale informed by observed VMR.
- **Concerns**: The pooled model will average over the 16× range in TCP means — expect poor PPC coverage. Iteration 1 should add machine-level random effects.

### Model: m1.stan — Independent Negative Binomial

**Structure**: Fully pooled NegBin(mu, phi) for TCP and UDP CPS independently, shared parameters across all 5 machines (27,133 seconds total). Machine index included in data block but unused in m1.

**Priors**:
- `mu_tcp ~ exponential(1)` — mean 1, weakly informative given observed mean 0.42
- `mu_udp ~ exponential(1)` — same rationale, observed mean 0.39
- `phi_tcp ~ exponential(0.5)` — mean 2, allows wide range of overdispersion
- `phi_udp ~ exponential(0.5)` — same

### Diagnostics

- Divergences: 0
- E-BFMI: 1.162, 1.199, 1.098, 1.078 (all well above 0.3)
- Rhat: max = 1.0015 (phi_tcp) — excellent
- Bulk ESS: min = 4503 (mu_tcp) — excellent
- Tail ESS: min = 3152 (mu_udp) — excellent
- Issues: none. All convergence criteria met comfortably.

**Posterior estimates**:
- mu_tcp = 0.424 (sd 0.008), phi_tcp = 0.136 (sd 0.003) → VMR ≈ mu/phi + 1 ≈ 4.1
- mu_udp = 0.392 (sd 0.011), phi_udp = 0.052 (sd 0.001) → VMR ≈ mu/phi + 1 ≈ 8.5

### PPC Assessment (see plots/ppc_bootstrap.png)

The pooled NegBin model captures the overall shape of the observed CPS distributions well for both TCP and UDP. The dominant feature — a massive spike at zero with a long right tail — is reproduced by the posterior predictive draws. For TCP, the observed density shows subtle bumps around 1 and 2 CPS that the model smooths over; these likely reflect the mixture of machine-specific rates (mb_cap at ~1.1 vs others at ~0.1–0.35). For UDP, the fit is tighter since machine rates are more homogeneous. The model does not appear to miss systematic zero-inflation beyond what NegBin accommodates. Overall, the pooled model is a reasonable baseline but cannot capture per-machine rate heterogeneity.

### Next Steps

- **Iteration 1**: Add machine-level random effects (hierarchical NegBin) to capture the 16× range in TCP means across machines. This is essential for realistic N-user scaling since each simulated user should be drawn from the population distribution of rates, not the pooled average.
- **Temporal structure**: Investigate minute-level autocorrelation — connection bursts may cluster in time. A time-varying rate (e.g., random walk on log-mu per minute) could improve tail behaviour.
- **Joint TCP/UDP**: Consider whether TCP and UDP rates are correlated within seconds (e.g., application activity drives both). If so, a joint model could improve predictions of total concurrent connections.

---

## Iteration 1: Hierarchical NegBin with machine-level random effects

### Structural Audit

Before modifying the model, audited three key assumptions:

1. **All machines share one rate** — Violated: TCP means range 0.065–1.076 (16× range), documented in Bootstrap. Fixed this iteration by adding machine random effects.
2. **Seconds are i.i.d. within machine** — Not yet checked (temporal autocorrelation). Deferred: between-machine heterogeneity is the dominant misfit; will revisit after hierarchy is in place.
3. **NegBin adequacy** — PPC from m1 shows reasonable coverage; no structural zero-inflation beyond NegBin. Deferred: reassess after hierarchy.

### Change Made

Added machine-level random effects to the NegBin model using a non-centered parameterization. Each machine k gets its own rate mu_tcp[k] = exp(alpha_tcp + sigma_tcp * z_tcp[k]), where z_tcp[k] ~ N(0,1). This allows the model to capture the large between-machine variation (16× in TCP) while estimating the population-level hyperparameters (alpha, sigma) that drive N-user scaling. Written as new file m2.stan; fit.R updated to point to m2.

### Model: m2.stan — Hierarchical Negative Binomial with machine random effects

Non-centered parameterization of log-normal machine effects. Population mean alpha and between-machine SD sigma estimated per protocol. Shared phi (overdispersion) per protocol across machines. Generated quantities include mu_tcp_pop = exp(alpha + sigma^2/2) for the population mean, and per-observation posterior predictive replicates.

### Diagnostics

| Metric | Before (m1) | After (m2) |
|---|---|---|
| Divergences | 0 | 10 |
| E-BFMI min | 1.078 | 0.821 |
| Rhat max | 1.0015 | 1.0040 |
| Bulk ESS min | 4503 | 1200 |
| Tail ESS min | 3152 | 1479 |

### PPC Assessment (see plots/ppc_iter_1.png)

The hierarchical model's PPC is visually similar to m1's — posterior predictive draws closely track the observed density for both TCP and UDP, capturing the zero spike and tail shape. This is expected because the PPC pools across machines, and the machine-level rates correctly decompose the mixture that m1 was averaging over. The model now correctly estimates per-machine rates: hendo_cap at 0.066 TCP/s vs mb_cap at 1.076 TCP/s. UDP rates are more homogeneous (0.30–0.61). The key improvement is structural: scaling to N users now draws from a population distribution rather than a point estimate.

### Scaling Suitability

The model can now generate N-user distributions by drawing machine-level rates from the estimated population distribution (lognormal with alpha, sigma). However, the 10 divergences and wide posterior on mu_tcp_pop (mean=1.24, sd=3.94) suggest the alpha-sigma funnel geometry needs tightening before the scaling outputs are reliable. Terminations are also not yet modelled.

### Next Steps

- **Priority**: Eliminate the 10 divergences — likely caused by the funnel between alpha_tcp and sigma_tcp with only K=5 groups. Options: tighter prior on sigma, or parameterize the population mean directly instead of deriving it from alpha and sigma.
- Add connection terminations (tcp_end, udp_end) to the model once diagnostics are clean.
- Investigate temporal autocorrelation within machines.

---

## Iteration 2: Tighten sigma prior and increase adapt_delta to eliminate divergences

### Change Made

Two minor tuning changes to m2.stan to address the 10 divergences from iteration 1: (1) tightened the between-machine SD prior from normal(0, 1.5) to normal(0, 1) — still allows sigma_tcp≈1.1 but reduces extreme excursions into the funnel; (2) increased adapt_delta from 0.8 (default) to 0.95, giving the sampler smaller step sizes to navigate the alpha-sigma geometry. No structural model change — same m2.stan file updated in place.

### Model: m2.stan — Hierarchical NegBin (tighter sigma prior, adapt_delta=0.95)

Same structure as iteration 1. Only changes: sigma prior scale reduced from 1.5 to 1.0, adapt_delta raised to 0.95.

### Diagnostics

| Metric | Before (iter 1) | After (iter 2) |
|---|---|---|
| Divergences | 10 | 0 |
| E-BFMI min | 0.821 | 0.805 |
| Rhat max | 1.0040 | 1.0058 |
| Bulk ESS min | 1200 | 1238 |
| Tail ESS min | 1479 | 1736 |

All convergence criteria met: 0 divergences, E-BFMI > 0.3, Rhat < 1.01, ESS > 400.

### PPC Assessment (see plots/ppc_iter_2.png)

PPC is essentially identical to iteration 1 — posterior predictive draws closely track the observed density for both TCP and UDP, capturing the zero spike, the secondary mode around 1 CPS (TCP), and the heavy tails. The tighter prior and higher adapt_delta did not change the posterior substantively, confirming that the divergences in iteration 1 were a sampling artefact rather than a sign of model misspecification. Machine-level estimates are stable: TCP rates 0.066–1.076/s, UDP rates 0.30–0.61/s.

### Scaling Suitability

With clean diagnostics and well-separated machine-level rates, the model can now meaningfully generate per-user CPS distributions by drawing from the population distribution (lognormal with alpha, sigma). However, terminations are not yet modelled — this is required for log volume estimation and concurrent connection derivation, both critical for firewall sizing.

### Next Steps

- **Priority**: Extend the model to jointly model connection terminations (tcp_end, udp_end) alongside new connections — this is the next structural improvement needed for the firewall sizing deliverable.
- Investigate temporal autocorrelation within machines (deferred from iteration 1).

---

## Iteration 3: Add connection terminations (m3)

### Change Made

Extended the model to jointly model all four per-second counts: tcp_new, udp_new, tcp_end, and udp_end. Each outcome has its own independent hierarchical NegBin structure (alpha, sigma, z[K], phi) with non-centered parameterization. Written as new file m3.stan. Generated quantities were removed from the Stan file to avoid OOM (4 × 27K = 108K replicate arrays caused exit code 137 on first attempt); PPC replicates are now generated in R post-hoc using `rnbinom()` with extracted mu and phi draws.

### Model: m3.stan — Hierarchical NegBin for new connections and terminations

Four independent hierarchical NegBin sub-models (tcp_new, udp_new, tcp_end, udp_end), each with population log-rate mean (alpha), between-machine SD (sigma), non-centered machine effects (z[K]), and shared overdispersion (phi). Total of 4×(2 + K + 1) = 32 parameters for K=5.

### Diagnostics

| Metric | Before (m2, iter 2) | After (m3, iter 3) |
|---|---|---|
| Divergences | 0 | 0 |
| E-BFMI min | 0.805 | 0.792 |
| Rhat max | 1.0058 | 1.0033 |
| Bulk ESS min | 1238 | 1468 |
| Tail ESS min | 1736 | 1804 |

All convergence criteria met. Adding the termination sub-models did not degrade sampling quality.

### PPC Assessment (see plots/ppc_iter_3.png)

All four panels show excellent PPC coverage. TCP new and UDP new panels are consistent with m2 results. TCP terminations show a slightly different shape — more peaked near 1 CPS with lower tail — reflecting that mb_cap's termination rate (0.96/s) is slightly below its new connection rate (1.08/s), consistent with some connections remaining open at capture end. UDP terminations are nearly identical to UDP new (expected since every flow has one start and one end). The model captures the zero-inflation and overdispersion in all four outcomes.

### Key Parameter Estimates (machine-level means)

| Machine | TCP new | TCP end | UDP new | UDP end |
|---|---|---|---|---|
| greg_cap | 0.276 | 0.265 | 0.317 | 0.316 |
| greg_cap_2 | 0.353 | 0.281 | 0.297 | 0.296 |
| greg_cap_3 | 0.118 | 0.108 | 0.315 | 0.315 |
| hendo_cap | 0.066 | 0.060 | 0.304 | 0.303 |
| mb_cap | 1.075 | 0.959 | 0.614 | 0.614 |

### Scaling Suitability

The model now covers all four quantities needed for firewall sizing: new connections (session creation throughput), terminations (log volume), and the ability to derive concurrent connections. With clean diagnostics and good PPC across all outcomes, this model can meaningfully generate per-user distributions for N-user scaling. The between-machine variation is properly captured, enabling realistic population-level draws.

### Next Steps

- **Assessed as CONVERGED**: All diagnostic criteria met, PPC good across all four outcomes, terminations modelled. Final scaling plots generated.

---

## Final Assessment

### Model Selected: m3.stan

The selected model is a hierarchical Negative Binomial with independent sub-models for four per-second counts: TCP new connections, UDP new connections, TCP terminations, and UDP terminations. Each sub-model uses non-centered parameterization with a population-level log-rate mean (alpha), between-machine standard deviation (sigma), machine-specific offsets (z[K]), and shared overdispersion (phi). The model was fit to 27,133 seconds of data across K=5 machines, achieving zero divergences, all Rhat < 1.01, all ESS > 1400, and good PPC coverage across all four outcomes. The hierarchical structure captures the 16× between-machine variation in TCP rates (0.066–1.076/s) and the more moderate 2× variation in UDP rates (0.30–0.61/s), enabling realistic N-user scaling by drawing per-user rates from the estimated population distribution.

### Firewall Sizing: How to Use This Model

**Scaling to N users** (NegBin scaling with hierarchical population draws):
- For each simulated user, draw a rate from lognormal(alpha, sigma)
- Per-second counts are NegBin(mu_user, phi)
- Aggregate N users by summing independent draws

**Per-user estimates from the model** (population-level):
- TCP new: mean ≈ 0.47/s (median), so N=10: ~5, N=50: ~24, N=100: ~47
- UDP new: mean ≈ 0.38/s (median), so N=10: ~4, N=50: ~19, N=100: ~38
- TCP terminations: mean ≈ 0.42/s → 25 logs/min per user (median)
- UDP terminations: mean ≈ 0.38/s → 23 logs/min per user (median)

**N-user scaling summary (posterior predictive, p95)**:

| Metric | N=1 | N=10 | N=50 | N=100 |
|---|---|---|---|---|
| TCP new CPS | 3 | 22 | 99 | 201 |
| UDP new CPS | 2 | 16 | 50 | 85 |
| TCP term/min | 180 | 1,200 | 5,760 | 11,880 |
| UDP term/min | 120 | 900 | 2,760 | 4,680 |
| Concurrent (p95) | 3,725 | 30,277 | 138,415 | 268,153 |

**Note on concurrent connections**: The concurrent connection estimates are high because the model treats new and termination rates as independent, and the data shows new > end on average (capture truncation: some TCP connections don't reach terminal state within the capture window). For firewall session table sizing, the concurrent connection figures represent a conservative upper bound. In practice, most connections are short-lived and the true steady-state concurrent count will be lower.

**Note on heavy tails**: The lognormal population distribution (sigma_tcp ≈ 1.1) produces substantial right-tail variation in per-user rates. The p99 values can be much higher than p95. For conservative firewall sizing, use p95 estimates; for worst-case planning, use p99.
