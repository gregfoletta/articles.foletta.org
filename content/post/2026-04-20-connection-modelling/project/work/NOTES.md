# TCP/UDP CPS Modelling — Investigation Notes

---

## Bootstrap

### Data Exploration

- **Machines** (3): `greg_cap_1`, `hendo_cap`, `mb_cap`
- **Per machine**:

  | machine_id   | duration (s) | duration (min) | total packets (raw) | local IPv4       | local IPv6                                 |
  |--------------|--------------:|----------------:|---------------------:|------------------|---------------------------------------------|
  | greg_cap_1   | 7 195         | 119.9           | 720 954              | 10.48.7.2        | 2403:5803:98a3:7:2389:50ba:ca75:b2e7        |
  | hendo_cap    | 3 600         |  60.0           | (see /data)          | 172.25.16.38     | fe80::1c03:e205:cd03:763e                   |
  | mb_cap       | 7 200         | 120.0           | (see /data)          | 192.168.230.4    | 2403:5811:bd7e:230:30ec:df9c:2a56:2cc6      |

  Total seconds across machines: T = **17 997**.

- **TCP CPS** (new outbound, three-way completed connections per second):

  | scope        | mean   | sd    | median | p99 | max | %zeros | var/mean |
  |--------------|-------:|------:|-------:|----:|----:|------:|---------:|
  | greg_cap_1   | 0.205  | 1.13  | 0      | 5   | 33  | 91.8% | **6.18** |
  | hendo_cap    | 0.060  | 0.48  | 0      | 2   |  8  | 97.7% | **3.86** |
  | mb_cap       | 0.980  | 1.49  | 0      | 6   | 23  | 53.9% | **2.27** |
  | POOLED       | 0.486  | 1.27  | 0      | 5   | 33  | 77.8% | **3.31** |

- **UDP CPS** (new outbound flow per second):

  | scope        | mean   | sd    | median | p99 | max | %zeros | var/mean |
  |--------------|-------:|------:|-------:|----:|----:|------:|---------:|
  | greg_cap_1   | 0.576  | 2.53  | 0      | 11  | 66  | 88.5% | **11.12** |
  | hendo_cap    | 0.300  | 1.77  | 0      |  6  | 77  | 91.9% |  **10.49** |
  | mb_cap       | 0.602  | 1.90  | 0      |  9  | 30  | 82.7% |   **6.01** |
  | POOLED       | 0.531  | 2.16  | 0      |  9  | 77  | 86.9% |   **8.76** |

  (`tcp_end` and `udp_end` distributions are very similar to their `*_new`
  counterparts — see `diagnostics/cps_stats.csv` for full numbers.)

  All series are **strongly overdispersed** (variance/mean far above 1) → a
  Poisson likelihood is clearly inadequate.

- **Between-machine differences**:
  - **Mean rates**: TCP CPS spans a ~16× range (0.06 hendo_cap → 0.98
    mb_cap); UDP CPS spans ~2× (0.30 hendo_cap → 0.60 mb_cap).
  - **Overdispersion**: TCP var/mean spans 2.3 (mb_cap) → 6.2 (greg_cap_1);
    UDP var/mean spans 6.0 (mb_cap) → 11.1 (greg_cap_1).
  - **Zero proportions**: TCP zeros vary 54%–98%; UDP zeros 83%–92%.
  - These differences are large and structural (different machines, network
    environments, user behaviour). Treating seconds across machines as
    fully exchangeable will underfit the heavy-rate machine and overfit
    the light-rate machine. Machine-level partial pooling is warranted in
    the next iteration.

### Model Design Decisions

- **Likelihood**: Negative Binomial (NB2 parameterisation `mu`, `phi`).
  Variance = `mu + mu^2 / phi`, so `phi → ∞` recovers Poisson; smaller
  `phi` admits the high overdispersion seen empirically (var/mean up to
  11). Empirical var/mean ratios of 2–11 imply `phi` likely in the
  0.05–0.5 range — well outside what a Poisson can express, but routinely
  handled by NB.
- **TCP and UDP modelled separately**: Their event semantics differ
  (three-way handshake vs first-packet) and their dispersions differ
  (UDP much more bursty). No domain reason to share parameters at this
  stage; joint structure (e.g. shared overdispersion) can be reintroduced
  later if profitable.
- **Four series, four likelihoods**: `tcp_new`, `tcp_end`, `udp_new`,
  `udp_end` are each given their own (mu, phi). End rates should track
  new rates closely in steady-state (because every connection that opens
  must eventually close), so we expect mu_*_end ≈ mu_*_new in the fit;
  this is also a simple sanity check.
- **Pooled across machines for bootstrap**: Start with the simplest
  defensible model. The data block already carries `K` and `machine[t]`
  so iteration 1 can introduce machine-level rates without changing the
  pipeline.
- **Priors** — weakly informative:
  - `log_mu_* ~ Normal(-1, 2)`: rate centred on ~0.37 cps, 95% prior
    interval on rate ≈ [0.007, 20] — covers all per-machine empirical
    means (0.06–0.98) with room to spare.
  - `phi_* ~ Gamma(2, 0.1)`: mean 20, mode 10. Supports both
    Poisson-like (large phi) and strongly overdispersed (small phi) data;
    prior median ≈ 17 is roughly Poisson-ish so it does not pre-commit
    to strong overdispersion.
- **Zero-inflation**: not modelled explicitly in the bootstrap. NB's
  excess-zeros at high overdispersion may already absorb most of the
  observed zero mass; if PPC shows residual zero-inflation we will add
  a hurdle/ZINB layer in a later iteration.
- **Temporal structure**: not modelled. Seconds are treated as iid given
  rate. Auto-correlation, time-of-day, and burstiness on sub-second
  timescales are deferred — they may become important once point-mass
  CPS is fitted.

---

### Model: m1.stan — Independent Negative Binomial

**Structure**: Four independent fully-pooled NB likelihoods —
`tcp_new ~ NB(mu_tcp_new, phi_tcp_new)` and analogous for `tcp_end`,
`udp_new`, `udp_end`. Each series has one rate parameter and one
dispersion parameter, shared across all 17 997 seconds and all 3
machines. Stan data block already carries `K = 3` and
`machine[1..T] ∈ {1..K}` for downstream iterations.

**Priors**:

| parameter      | prior           | rationale                                                |
|----------------|-----------------|----------------------------------------------------------|
| `log_mu_*`     | Normal(-1, 2)   | log-rate centred on log(0.37); 95% prior on rate ≈ [0.007, 20] — covers empirical 0.06–0.98 with margin. |
| `phi_*`        | Gamma(2, 0.1)   | NB dispersion; mean 20, mode 10 — does not pre-bias toward over- or under-dispersion. |

### Diagnostics

- Divergences: **0**
- E-BFMI: 1.574, 1.740, 1.758, 1.690 (all ≫ 0.3 — fine)
- Max-treedepth hits: 0
- Rhat: max = **1.0019** (`phi_tcp_new`)
- Bulk ESS: min = **1602** (`phi_udp_end`)
- Tail ESS: min = **1553** (`phi_udp_end`)
- Issues: **none** — all convergence criteria comfortably met.

Posterior estimates (means with 90% CI):
- `mu_tcp_new` = 0.486 [0.471, 0.500] — exact match to pooled empirical mean.
- `mu_tcp_end` = 0.486 [0.473, 0.501] — matches `mu_tcp_new` (good steady-state sanity check).
- `mu_udp_new` = 0.531 [0.504, 0.559]
- `mu_udp_end` = 0.531 [0.506, 0.556]
- `phi_tcp_new` = 0.216 [0.207, 0.226] — strong overdispersion (var ≈ 1.6 × mu²).
- `phi_tcp_end` = 0.217 [0.208, 0.227]
- `phi_udp_new` = 0.064 [0.061, 0.067] — very strong overdispersion (var ≈ 16 × mu²).
- `phi_udp_end` = 0.078 [0.075, 0.082]

### PPC Assessment (see plots/ppc_bootstrap.png)

The pooled NB matches the marginal shape of both TCP and UDP CPS densities
closely: zero spike, fast decay, and a long tail are all reproduced. Means
are recovered exactly (by construction — the model is essentially a single
NB MLE per series). However, this PPC is showing the **marginal** density;
it cannot reveal that a single (mu, phi) is being asked to absorb three
machines whose underlying rates differ by up to 16×. Heterogeneity is
hidden inside the inflated phi (UDP phi ≈ 0.06 is in geometric-like
territory). The bootstrap fit is therefore a usable starting point but
will not generalise correctly to the "N identical workstations" scaling
question — it would simulate N copies of a hypothetical "average machine"
and over- or under-predict any specific machine's traffic.

### Next Steps

- **Add machine-level rates with partial pooling**: replace `mu_*` with
  `mu_*[k] = exp(mu_log_bar_* + sigma_* · z_k)` (non-centred), letting
  each machine have its own rate but sharing strength across them.
- **Per-machine PPC overlay**: re-plot the bootstrap PPC faceted by
  machine so the heterogeneity that the pooled model is hiding becomes
  visible.
- **Investigate `phi`**: the very small UDP phi (~0.06) suggests either
  genuine extreme burstiness or unmodelled heterogeneity. A
  hierarchical model should reveal which by partially absorbing
  between-machine variance into the rate hierarchy and (likely)
  yielding larger phi.
