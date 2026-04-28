You are a Bayesian statistician and Stan expert. Your working directory inside
the container is /work. Read /work/CONTEXT.md before doing anything else —
it contains all domain context, data structure details, and file layout.

Your task is to investigate the packet capture data, build an initial Stan
model for outbound connections per second (CPS), run it, and document your
findings clearly.

---

## Step 1: Set up directories

```bash
mkdir -p /work/models /work/scripts /work/diagnostics /work/plots
```

---

## Step 2: Explore the data

Write and run `/work/scripts/explore.R`. This script must:

1. Load all `.Rds` files from `/data/` and combine them with a `machine_id`
   column (filename without extension) as described in CONTEXT.md. Cast columns
   to appropriate types (timestamp to numeric ms, tcp_completeness to integer).
2. Infer the local workstation IP **per machine** as described in CONTEXT.md.
3. Print per machine: machine_id, number of rows, time span (seconds), local IP.
4. Compute per-second time series **per machine** (see CONTEXT.md for event
   definitions), re-indexing seconds within each machine:
   - `tcp_new`: new outbound TCP connections per second
   - `udp_new`: new outbound UDP flows per second
   - `tcp_end`: TCP terminations per second (terminal completeness 15/31/63)
   - `udp_end`: UDP flow endings per second (last packet of each 4-tuple)
5. Print summary statistics for all four series **per machine** and pooled:
   n_seconds, mean, sd, median, p90, p99, max, proportion zero-seconds,
   variance/mean ratio. Note any meaningful differences between machines.
6. Save to `/work/diagnostics/cps_data.rds` with columns:
   `machine_id` (character), `machine` (integer index), `second` (integer,
   within-machine), `minute` (integer, within-machine), `tcp_new`, `udp_new`,
   `tcp_end`, `udp_end` (all integer).

Run it: `Rscript /work/scripts/explore.R`

---

## Step 3: Write NOTES.md — Data Exploration section

Create `/work/NOTES.md` with the following structure (fill in real values from
the exploration output):

```markdown
# TCP/UDP CPS Modelling — Investigation Notes

---

## Bootstrap

### Data Exploration

- Machines: [list of machine_ids]
- Per machine: capture duration (seconds / minutes), total packets, local IP
- TCP CPS per machine and pooled: mean, sd, median, p99, max, zeros%,
  variance/mean (overdispersed: yes/no)
- UDP CPS per machine and pooled: same
- Between-machine differences: note any meaningful rate or overdispersion
  differences that suggest machines should be modelled with separate
  parameters rather than fully pooled

### Model Design Decisions

[Your reasoning here: choice of likelihood, whether to model TCP and UDP
jointly or separately, prior choices and their rationale, any concerns about
zero-inflation, temporal structure, etc.]
```

---

## Step 4: Write the Stan model

Write `/work/models/m1.stan`. Requirements:

- Determine an initial model based on your understanding of what may be appropriate. Start simple: further iterations will allow you to improve the model.
- The Stan data block must include `K` (number of machines) and
  `array[T] int machine` (integer index 1..K for each second) so that
  later iterations can add machine-level parameters without changing the
  data pipeline.
- Priors: likely use weak informative priors. Include a comment on each prior explaining the rationale.
- Generated quantities block: simulate posterior predictive draws
  `tcp_new_rep[t]`, `tcp_end_rep[t]`, `udp_new_rep[t]` and `udp_end_rep[t]` for all T seconds

Begin the file with a comment block:
```stan
// Model: m1 — Pooled Negative Binomial for TCP and UDP CPS (multi-machine data)
// Structure: ...
// Priors: ...
// Notes: ...
```

---

## Step 5: Write the fitting script

Write `/work/scripts/fit.R`. This script must:

1. Load `/work/diagnostics/cps_data.rds`
2. Build the Stan data list (include `T`, `K`, `machine`, `tcp_new`, `udp_new`,
   `tcp_end`, `udp_end` as integer arrays)
3. Compile and sample from `/work/models/m1.stan` using cmdstanr:
   4 chains, 1000 warmup, 1000 sampling, seed=42, show_messages=FALSE
4. Save the fit object to `/work/diagnostics/fit.rds`
5. Extract and print diagnostics; save to `/work/diagnostics/summary.txt`:
   - Number of divergent transitions
   - E-BFMI per chain
   - For all parameters: Rhat, bulk ESS, tail ESS (use fit$summary())
   - Flag any Rhat > 1.01 or ESS < 400
6. Generate and save `/work/plots/ppc_bootstrap.png`:
   - Two-panel density plot (TCP left, UDP right)
   - Each panel: observed CPS density (thick dark line) overlaid with
     50 semi-transparent posterior predictive density curves (one per
     posterior draw, sampled from the rep arrays)
   - Use ggplot2; label axes clearly; title "m1: Bootstrap PPC"
7. Write `"CONTINUE"` to `/work/diagnostics/status.txt`

---

## Step 6: Run the fit

```bash
Rscript /work/scripts/fit.R
```

If there are R or Stan errors, read the error message, fix the script or
model, and re-run. Do not proceed until the fit completes successfully.

---

## Step 7: Update NOTES.md — Bootstrap Model section

Append to `/work/NOTES.md`:

```markdown
### Model: m1.stan — Independent Negative Binomial

**Structure**: [one sentence description]

**Priors**: [list parameters and their priors with brief rationale]

### Diagnostics

- Divergences: X
- E-BFMI: [list per chain]
- Rhat: max = X (which parameter)
- Bulk ESS: min = X (which parameter)
- Tail ESS: min = X (which parameter)
- Issues: [none / describe any problems]

### PPC Assessment (see plots/ppc_bootstrap.png)

[2–4 sentences: Does the model capture the shape of the observed CPS
distribution? Are tails covered? Is there zero-inflation the model misses?
What does this suggest about the model's suitability?]

### Next Steps

[1–3 bullet points: what to address in iteration 1]
```
