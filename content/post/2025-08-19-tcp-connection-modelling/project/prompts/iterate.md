You are iteratively improving a Bayesian Stan model for outbound TCP and UDP
connections per second (CPS) from a single workstation. Your working directory
is /work. Read /work/CONTEXT.md for domain context.

---

## Step 1: Read current state

Read the following files to orient yourself:

- `/work/NOTES.md` — full history of what has been tried and why
- `/work/diagnostics/summary.txt` — diagnostics from the last fit
- `/work/diagnostics/status.txt` — CONTINUE or CONVERGED
- The current Stan model files in `/work/models/`

Determine the current iteration number N by counting the "## Iteration"
sections already in NOTES.md. You are working on iteration N+1.

---

## Step 1b: Structural audit

Before looking at convergence metrics, audit the assumptions the current model
makes. List any that have not yet been empirically verified against the data.
For each assumption:

- State what would need to be true for it to hold
- Note whether it was checked in any previous iteration (search NOTES.md)
- Decide: **verify this iteration** or **defer with explicit justification**

If any unverified assumption would materially change the modelling direction if
violated, verify it now — before proposing a model change. Document the
findings in NOTES.md regardless of outcome.

Note that the data contains captures from multiple machines (`machine_id` in
cps_data.rds). Between-machine variation in baseline rates is a structural
feature of the data that the model should eventually account for — it directly
determines how much uncertainty to carry into the N-user scaling outputs.

---

## Step 2: Diagnose

Assess the last model against these criteria:

| Criterion | Target | Problem if failing |
|---|---|---|
| Divergences | 0 | Geometry issue — reparameterize or tighten priors |
| E-BFMI | > 0.3 per chain | Prior/likelihood mismatch |
| Rhat | < 1.01 all params | Non-convergence |
| Bulk ESS | > 400 all params | Poor mixing |
| Tail ESS | > 400 all params | Poor tail sampling |
| PPC | Covers observed distribution | Model misspecification |

Also consider: does the PPC from the previous iteration (in `/work/plots/`)
show systematic misfit (too narrow, heavy-tailed data not captured, wrong
mean)?

---

## Step 3: Make exactly ONE targeted improvement

Choose one change based on your diagnosis. In priority order:

1. **Divergences present**: non-centered parameterization, constrain priors
2. **Low ESS / high Rhat**: log-scale parameterization, more informative priors
3. **PPC too narrow** (model underestimates spread): more flexible likelihood,
   add overdispersion structure, or relax priors
4. **PPC systematically shifted**: adjust prior on mean, check data prep
5. **PPC shape wrong** (e.g. bimodal observed, unimodal predicted): before
   attempting mixture models, check whether temporal autocorrelation better
   explains the shape misfit — mixtures on count data are frequently
   non-identifiable; temporal structure (AR or hierarchical by time block)
   is usually more robust and often the true cause of apparent bimodality
6. **Model does not yet include terminations**: extend the model to jointly
   model connection terminations (`tcp_end`, `udp_end`) alongside new
   connections. Terminations drive firewall log volume (logs/min ≈
   terminations/s × 60 × N users) and, together with new connections, allow
   concurrent connections to be derived as a generated quantity. See
   CONTEXT.md for how to compute `tcp_end` and `udp_end` from the data.
7. **All diagnostics good, PPC good, terminations modelled**: add temporal
   structure (e.g. AR(1) on log rate), or model TCP/UDP jointly with shared
   hyperpriors

If your change introduces a meaningfully different model structure, write it
as a **new file** (`m2.stan`, `m3.stan`, etc.) rather than overwriting the
previous model. Update `/work/scripts/fit.R` to point to the new model file
and save the fit to `/work/diagnostics/fit.rds` (overwriting is fine).

If the change is a minor prior adjustment or reparameterization of the same
model, update the existing Stan file in place.

---

## Step 4: Run the fit

```bash
Rscript /work/scripts/fit.R
```

If there are errors, fix them and re-run. Do not proceed until the fit
completes successfully.

---

## Step 5: Generate the PPC plot for this iteration

Ensure the fit script saves the PPC plot to:

```
/work/plots/ppc_iter_N.png
```

(where N is the current iteration number from Step 1)

The plot must show:
- Two panels: TCP (left) and UDP (right)
- Observed CPS density: thick dark line
- 50 posterior predictive density curves: semi-transparent, one per draw
- Clear axis labels and title: "mX: [short model description] — Iteration N PPC"

If `fit.R` currently saves the plot with a fixed filename, update it to use
the correct iteration-numbered filename for this run.

---

## Step 6: Update NOTES.md

Append the following section to `/work/NOTES.md`:

```markdown
---

## Iteration N: [Short title describing the change]

### Change Made

[One paragraph: what was changed, which file, and the specific reasoning
based on the diagnostics from the previous iteration.]

### Model: [filename] — [one-line description]

[If a new Stan file was written, briefly describe its structure and how it
differs from the previous model.]

### Diagnostics

| Metric | Before | After |
|---|---|---|
| Divergences | X | X |
| E-BFMI min | X | X |
| Rhat max | X | X |
| Bulk ESS min | X | X |
| Tail ESS min | X | X |

### PPC Assessment (see plots/ppc_iter_N.png)

[2–4 sentences: Is the fit better or worse than before? What aspects of the
observed distribution does the model now capture or still miss? Is the model
appropriate for generating data to simulate N users?]

### Scaling Suitability

[1–2 sentences: Given the current model, can we meaningfully generate CPS
distributions for N = 10, 50, 100 users? What would need to improve first?]

### Next Steps

[1–3 bullet points]
```

---

## Step 7: Assess convergence

If ALL of the following are true:
- Zero divergences
- All E-BFMI > 0.3
- All Rhat < 1.01
- All Bulk ESS > 400
- All Tail ESS > 400
- PPC covers the observed distribution well

Then:
1. Write `"CONVERGED"` to `/work/diagnostics/status.txt`
2. Append a final section to `/work/NOTES.md`:

```markdown
---

## Final Assessment

### Model Selected: [filename]

[One paragraph summary of the chosen model and why it is appropriate.]

### Firewall Sizing: How to Use This Model

**Scaling to N users** (see CONTEXT.md for the NegBin scaling formula):
- New connections/s → session creation throughput required
- Terminations/s × 60 → log lines per minute the firewall will generate
- Concurrent connections → session table size required

**Per-user estimates from the model**:
- TCP new: mean = [value]/s, so N=10: ~[value], N=50: ~[value], N=100: ~[value]
- UDP new: [same]
- TCP terminations: mean = [value]/s → [value] logs/min per user
- UDP terminations: [same]
- Peak concurrent connections (p95): [value] per user → [value] for N=100

Generate and save `/work/plots/scaling_N_users.png` with three panels
(new CPS, terminations/min, concurrent connections) each showing posterior
predictive distributions for N = 1, 10, 50, 100 users overlaid.
```

Otherwise write `"CONTINUE"` to `/work/diagnostics/status.txt`.
