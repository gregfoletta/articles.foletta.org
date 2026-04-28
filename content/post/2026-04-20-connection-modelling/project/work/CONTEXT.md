# TCP/UDP Connection Rate Modelling — Project Context

## Purpose

Build a Bayesian Stan model that characterises three quantities per second from
a single workstation: new outbound connections, connection terminations, and
(derived) concurrent connections. The model is used to simulate what these
quantities look like for N identical workstations (e.g. 10, 50, 100 users),
enabling:

- **Firewall throughput sizing**: driven by new connections per second (session
  creation rate).
- **Firewall session table sizing**: driven by concurrent connections (the
  running integral of new minus terminated).
- **Log volume estimation**: stateful firewalls log at session termination, so
  logs per minute ≈ termination rate × 60 × N users.

Generating realistic posterior predictive distributions of all three quantities
— and showing how they scale with N — is the primary deliverable.

---

## Data

**Files**: `/data/` contains one or more `.Rds` files, each a compressed R
data frame of individual packets captured with tshark on a single
workstation's WiFi interface. Each file is an independent capture from a
different machine. Load all files and tag each row with a `machine_id` derived
from the filename before doing anything else:

```r
files <- list.files("/data", pattern = "\\.Rds$", full.names = TRUE)
data  <- purrr::map_dfr(files, ~ readRDS(.x) |>
           dplyr::mutate(machine_id = tools::file_path_sans_ext(basename(.x))))
```

Treat captures from different machines as independent observations of the
same data-generating process. Do not pool seconds across machines as if they
were exchangeable — each machine has its own local IP, its own capture
duration, and potentially its own connection rate.

**Columns** (all stored as character; cast before use):

| Column             | Type      | Notes |
|--------------------|-----------|-------|
| `id`               | integer   | Per-pack ID |
| `timestamp`        | double    | Milliseconds since Unix epoch; divide by 1000 for POSIXct |
| `protocol`         | factor    | `"tcp"` or `"udp"` |
| `ip_version`       | integer   | `"4"` or `"6"` |
| `src_ip`           | character | Source IP address |
| `dst_ip`           | character | Destination IP address |
| `src_port`         | character | Source TCP or UDP port |
| `dst_port`         | character | Destination TCP or UDP port |
| `time_relative`    | double    | Seconds since the TCP or UDP stream started
| `tcp_completeness` | character | TCP completeness bitmask (TCP only; NA for UDP) |

**Key tcp_completeness values** (after casting to integer):
- The values are cumulative within the stream, e.g.
 - 0 is the first packet (SYN),
 - 1 is the second packet (SYN/ACK)
 - 3 is the ACK from the SYN/ACK
- `0`  — first packet of a new TCP connection (this IS the new-connection event)
- `15`, `31`, `63` — completed connections
- Other values — mid-stream packets, ignore for CPS counting

---

## Identifying the Workstation IP

The data has no explicit "local machine" flag. A workstation may have both an
IPv4 and an IPv6 address; both must be found and treated as the same machine's
traffic. Infer the dominant source IP **per machine per IP version**:

```r
local_ips <- data |>
  filter(protocol == "tcp", as.integer(tcp_completeness) == 0) |>
  group_by(machine_id, ip_version) |>
  count(src_ip, sort = TRUE) |>
  slice_max(n, n = 1) |>
  select(machine_id, ip_version, local_ip = src_ip)
```

This produces up to two rows per machine (one for each address family present).
Join back onto `data` by `(machine_id, ip_version, src_ip)` and retain only
rows where `src_ip` matches the inferred local address for that machine and
address family. All subsequent CPS counting must include connections from both
address families so that no outbound traffic is missed.

---

## Defining Events per Protocol

### New connections
**TCP**: `tcp_completeness == 0` AND `src_ip == local_ip`. 
Note you can use the `stream_id` variable to uniquely identify a bi-directional TCP stream. `stream_id` is only unique WITHIN protocol (tcp or udp).

Only count successful TCP connections as a new-connection event. TCP connections that never reached a full TCP three-way should not be counted.

**UDP**: No completeness field. 
Can use the `stream_id` variable to uniquely identify a UDP stream. The *first* packet of each stream is a new-connection event.
Filter to `src_ip == local_ip` for outbound flows only.

### Terminations
**TCP**: For each unique stream (identified by `stream_id`), the termination
event is the packet where `tcp_completeness` reaches a terminal value
(15, 31, or 63 — indicating FIN/RST completion). Use the timestamp of that
packet as the termination time. Only count streams that reach a terminal
completeness value; incomplete streams are in progress at capture end.

**UDP**: No explicit termination signal exists. Use the *last* packet of each
stream (identified by `stream_id`) as the termination event. This is an approximation —
it underestimates true termination rate because some flows may continue beyond
the capture window.

---

## Time Series

Aggregate events into 1-second bins **per machine**:

```r
second_bin <- floor(as.numeric(timestamp_ms) / 1000)
```

This produces four integer count vectors per machine. Seconds should be
re-indexed within each machine (so each starts at 1), and a `machine_id`
column retained throughout. The CPS data saved to disk should have columns:

- `machine_id` — character identifier from the filename
- `machine`    — integer index (1..K, where K = number of machines)
- `second`     — integer, 1-indexed within each machine's capture
- `minute`     — integer minute block index within each machine's capture
- `tcp_new`, `udp_new`, `tcp_end`, `udp_end` — integer counts
- `tcp_concurrent` and `udp_concurrent` - calculated per-protocol, per-second, per-machine based on `tcp_new` - `tcp_end` and `udp_new` - `udp_end`.

Many seconds will have zero counts in each series, which is expected.

---

## File Layout (inside container at /work)

```
/work/
  CONTEXT.md          <- this file (read-only reference)
  NOTES.md            <- running investigation notes (written by Claude)
  models/             <- Stan model files (m1.stan, m2.stan, ...)
  scripts/            <- R scripts (explore.R, fit.R, ...)
  diagnostics/        <- fit objects, summary text, status flag
  plots/              <- PPC and scaling plots
  loop_logs/          <- JSON logs (written by host runner)
```

Multiple data files from multiple machines are in `/data/*.jsonl.rds` (read-only mount).

---

## Stan / R Environment

- R 4.4, tidyverse, cmdstanr, tidybayes, posterior
- CmdStan path set automatically via Rprofile.site
- Compile: `cmdstan_model("/work/models/mN.stan")`
- Default sampling: 4 chains, 1000 warmup, 1000 sampling, seed = 42

---

## Convergence Criteria

- Zero divergences
- E-BFMI > 0.3 on all chains
- All Rhat < 1.01
- Bulk ESS > 400 and Tail ESS > 400 for all parameters

## Good PPC

The posterior predictive density of simulated per-second CPS should cover the
observed CPS distribution — capturing the mean, spread, and tail behaviour.
Zero-inflation or heavy tails in the observed data that the model misses are
red flags.

You may also look at the dervied concurrent connections from the posterior (new connections minus ended connections) per protocol (UDP and TCP)
over time and compare them against the real (derived) concurrent connections:
- Are the real connections within the credible interval?
- Do the concurrent connections drop into negative territory (which is impossible in the real world).

