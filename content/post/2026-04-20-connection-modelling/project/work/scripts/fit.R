## Fit m1.stan: pooled NB for tcp_new, tcp_end, udp_new, udp_end.
## Saves the fit, summary, and bootstrap PPC plot.
## Memory note: T x #draws x 4 rep series can be ~2GB. We thin by 2 to
## halve the in-memory rep arrays and load reps one series at a time.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cmdstanr)
  library(posterior)
})

set.seed(42)

# ---------- Load CPS data ---------------------------------------------------

cps <- readRDS("/work/diagnostics/cps_data.rds")
stopifnot(all(c("machine_id", "machine", "second", "minute",
                "tcp_new", "udp_new", "tcp_end", "udp_end") %in% names(cps)))

cps <- cps |> arrange(machine, second)

T_  <- nrow(cps)
K   <- length(unique(cps$machine))

stan_data <- list(
  T       = T_,
  K       = K,
  machine = as.integer(cps$machine),
  tcp_new = as.integer(cps$tcp_new),
  tcp_end = as.integer(cps$tcp_end),
  udp_new = as.integer(cps$udp_new),
  udp_end = as.integer(cps$udp_end)
)

cat("Stan data: T =", T_, "K =", K, "\n")

# ---------- Compile and sample ---------------------------------------------

mod <- cmdstan_model("/work/models/m1.stan")

fit <- mod$sample(
  data            = stan_data,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 1000,
  iter_sampling   = 1000,
  thin            = 2,         # halve memory footprint of GQ rep arrays
  seed            = 42,
  show_messages   = FALSE,
  refresh         = 200
)

# ---------- Diagnostics -----------------------------------------------------

diag <- fit$diagnostic_summary()
n_div  <- sum(diag$num_divergent)
ebfmi  <- diag$ebfmi
n_max_treedepth <- sum(diag$num_max_treedepth)

param_summary <- fit$summary(
  variables = c("log_mu_tcp_new", "log_mu_tcp_end", "log_mu_udp_new", "log_mu_udp_end",
                "phi_tcp_new", "phi_tcp_end", "phi_udp_new", "phi_udp_end",
                "mu_tcp_new", "mu_tcp_end", "mu_udp_new", "mu_udp_end")
)

flagged_rhat <- param_summary |> filter(rhat > 1.01)
flagged_ess  <- param_summary |> filter(ess_bulk < 400 | ess_tail < 400)

out_lines <- c(
  "Model: m1.stan (pooled NB for 4 CPS series)",
  paste("T =", T_, " K =", K),
  "Chains = 4, warmup = 1000, sampling = 1000, thin = 2, seed = 42",
  "",
  paste("Divergent transitions (total across chains):", n_div),
  paste("Max-treedepth hits (total):", n_max_treedepth),
  paste("E-BFMI per chain:", paste(sprintf("%.3f", ebfmi), collapse = ", ")),
  "",
  "Parameter summary (model parameters only):",
  capture.output(print(param_summary, n = Inf, width = 200)),
  "",
  paste("Max Rhat:", sprintf("%.4f", max(param_summary$rhat))),
  paste("Min Bulk ESS:", sprintf("%.1f", min(param_summary$ess_bulk)),
        "(", param_summary$variable[which.min(param_summary$ess_bulk)], ")"),
  paste("Min Tail ESS:", sprintf("%.1f", min(param_summary$ess_tail)),
        "(", param_summary$variable[which.min(param_summary$ess_tail)], ")"),
  "",
  if (nrow(flagged_rhat) == 0) "Rhat OK: all <= 1.01" else
    paste("WARN Rhat > 1.01:", paste(flagged_rhat$variable, collapse = ", ")),
  if (nrow(flagged_ess) == 0)  "ESS OK: all bulk/tail >= 400" else
    paste("WARN ESS < 400 :", paste(flagged_ess$variable, collapse = ", "))
)

writeLines(out_lines, "/work/diagnostics/summary.txt")
cat(paste(out_lines, collapse = "\n"), "\n")

gc(verbose = FALSE)

# ---------- Bootstrap PPC plot ---------------------------------------------

# Load each rep series one at a time to keep memory below ~600MB peak.
n_show <- 50

build_long <- function(prefix) {
  dr <- fit$draws(variables = prefix, format = "draws_matrix")
  ndraws <- nrow(dr)
  set.seed(42 + ifelse(prefix == "tcp_new_rep", 0, 1))
  sel <- sample.int(ndraws, n_show)
  m   <- dr[sel, , drop = FALSE]
  rm(dr); gc(verbose = FALSE)
  out <- tibble(
    draw_id = rep(seq_len(n_show), times = ncol(m)),
    value   = as.integer(as.vector(m))
  )
  rm(m); gc(verbose = FALSE)
  out
}

tcp_rep_long <- build_long("tcp_new_rep") |> mutate(series = "TCP new CPS")
udp_rep_long <- build_long("udp_new_rep") |> mutate(series = "UDP new CPS")
rep_long <- bind_rows(tcp_rep_long, udp_rep_long) |>
  mutate(draw_id = paste(series, draw_id, sep = "_"))

obs_long <- bind_rows(
  tibble(value = cps$tcp_new, series = "TCP new CPS"),
  tibble(value = cps$udp_new, series = "UDP new CPS")
)

clip_q <- obs_long |>
  group_by(series) |>
  summarise(qmax = quantile(value, 0.995), .groups = "drop") |>
  mutate(qmax = pmax(qmax, 5))

p <- ggplot() +
  geom_density(data = rep_long,
               aes(x = value, group = draw_id),
               colour = "steelblue", alpha = 0.18, bw = 0.6) +
  geom_density(data = obs_long,
               aes(x = value),
               colour = "black", linewidth = 1.0, bw = 0.6) +
  facet_wrap(~ series, scales = "free", ncol = 2) +
  coord_cartesian(xlim = c(-0.5, max(clip_q$qmax) + 1)) +
  labs(
    title    = "m1: Bootstrap PPC",
    subtitle = "Observed (black) vs 50 posterior predictive draws (blue)",
    x        = "Per-second count",
    y        = "Density"
  ) +
  theme_bw(base_size = 12)

ggsave("/work/plots/ppc_bootstrap.png", p,
       width = 10, height = 4.5, dpi = 130)

rm(rep_long, tcp_rep_long, udp_rep_long, obs_long, p); gc(verbose = FALSE)

# ---------- Save fit object (memory permitting) ----------------------------
# fit$save_object compresses by default; disable to keep peak memory low.

fit$save_object("/work/diagnostics/fit.rds", compress = FALSE)

# ---------- Status flag -----------------------------------------------------

writeLines("CONTINUE", "/work/diagnostics/status.txt")
cat("\nSaved fit, summary, plot, and status flag.\n")
