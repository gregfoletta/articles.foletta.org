library(tidyverse)
library(cmdstanr)
library(posterior)

# ── Load fit ──────────────────────────────────────────────────────────────
fit <- readRDS("/work/diagnostics/fit.rds")
cps <- readRDS("/work/diagnostics/cps_data.rds")
K <- length(unique(cps$machine))

cat("Loaded fit and data. K =", K, "machines\n")

# ── Extract draws ─────────────────────────────────────────────────────────
param_vars <- c(
  "alpha_tcp_new", "alpha_udp_new", "alpha_tcp_end", "alpha_udp_end",
  "sigma_tcp_new", "sigma_udp_new", "sigma_tcp_end", "sigma_udp_end",
  "phi_tcp_new", "phi_udp_new", "phi_tcp_end", "phi_udp_end"
)
draws <- fit$draws(variables = param_vars, format = "draws_matrix")

set.seed(42)
n_draws <- 500
draw_ids <- sample(nrow(draws), n_draws)

N_users <- c(1, 10, 50, 100)
n_seconds <- 1000  # simulate 1000 seconds per draw

# ── Simulate N-user scaling ──────────────────────────────────────────────
# For each posterior draw:
#   1. Draw N user-level rates from lognormal(alpha, sigma)
#   2. For each second, draw NegBin(mu_user, phi) for each user and sum
# This captures both between-user and within-user variation.

outcomes <- list(
  list(alpha = "alpha_tcp_new", sigma = "sigma_tcp_new", phi = "phi_tcp_new", label = "TCP New CPS"),
  list(alpha = "alpha_udp_new", sigma = "sigma_udp_new", phi = "phi_udp_new", label = "UDP New CPS"),
  list(alpha = "alpha_tcp_end", sigma = "sigma_tcp_end", phi = "phi_tcp_end", label = "TCP Terminations/s"),
  list(alpha = "alpha_udp_end", sigma = "sigma_udp_end", phi = "phi_udp_end", label = "UDP Terminations/s")
)

scaling_data <- tibble()

for (oc in outcomes) {
  for (N in N_users) {
    agg_vals <- numeric(n_draws * n_seconds)
    idx <- 1
    for (i in seq_along(draw_ids)) {
      d <- draw_ids[i]
      alpha_val <- draws[d, oc$alpha]
      sigma_val <- draws[d, oc$sigma]
      phi_val   <- draws[d, oc$phi]

      # Draw N user rates from the population distribution
      user_rates <- exp(rnorm(N, mean = alpha_val, sd = sigma_val))

      # For each second, sum N independent NegBin draws
      for (s in 1:n_seconds) {
        total <- sum(rnbinom(N, mu = user_rates, size = phi_val))
        agg_vals[idx] <- total
        idx <- idx + 1
      }
    }
    scaling_data <- bind_rows(scaling_data,
      tibble(value = agg_vals, N = paste0("N=", N), outcome = oc$label))
  }
  cat("Done:", oc$label, "\n")
}

# ── Compute summary stats for the final assessment ───────────────────────
cat("\n=== Per-user summary statistics ===\n")
for (oc in outcomes) {
  vals <- numeric(n_draws)
  for (i in seq_along(draw_ids)) {
    d <- draw_ids[i]
    vals[i] <- exp(draws[d, oc$alpha] + draws[d, oc$sigma]^2 / 2)
  }
  cat(sprintf("%s: population mean = %.3f/s (median %.3f)\n",
              oc$label, mean(vals), median(vals)))
}

# ── Derive concurrent connections ────────────────────────────────────────
# Concurrent = cumsum(new) - cumsum(end) over time.
# In steady state, concurrent ~ (mean_new - mean_end) * avg_duration isn't
# directly available, but we can simulate a time series and take running totals.

cat("\n=== Concurrent connections simulation ===\n")

concurrent_data <- tibble()
n_sim_seconds <- 3600  # simulate 1 hour

for (N in N_users) {
  concurrent_vals <- numeric(n_draws * n_sim_seconds)
  idx <- 1
  for (i in 1:min(n_draws, 200)) {  # use 200 draws for tractability
    d <- draw_ids[i]

    # Draw user rates for all 4 outcomes
    tcp_new_rates <- exp(rnorm(N, draws[d, "alpha_tcp_new"], draws[d, "sigma_tcp_new"]))
    tcp_end_rates <- exp(rnorm(N, draws[d, "alpha_tcp_end"], draws[d, "sigma_tcp_end"]))
    udp_new_rates <- exp(rnorm(N, draws[d, "alpha_udp_new"], draws[d, "sigma_udp_new"]))
    udp_end_rates <- exp(rnorm(N, draws[d, "alpha_udp_end"], draws[d, "sigma_udp_end"]))

    phi_tn <- draws[d, "phi_tcp_new"]
    phi_te <- draws[d, "phi_tcp_end"]
    phi_un <- draws[d, "phi_udp_new"]
    phi_ue <- draws[d, "phi_udp_end"]

    # Simulate per-second new and end counts, accumulate concurrent
    concurrent <- 0
    for (s in 1:n_sim_seconds) {
      new_tcp <- sum(rnbinom(N, mu = tcp_new_rates, size = phi_tn))
      end_tcp <- sum(rnbinom(N, mu = tcp_end_rates, size = phi_te))
      new_udp <- sum(rnbinom(N, mu = udp_new_rates, size = phi_un))
      end_udp <- sum(rnbinom(N, mu = udp_end_rates, size = phi_ue))

      concurrent <- max(0, concurrent + (new_tcp + new_udp) - (end_tcp + end_udp))
      concurrent_vals[idx] <- concurrent
      idx <- idx + 1
    }
  }
  # Use last 1800 seconds (steady state) from each draw
  concurrent_data <- bind_rows(concurrent_data,
    tibble(value = concurrent_vals[1:(idx-1)], N = paste0("N=", N),
           outcome = "Concurrent Connections"))
  cat(sprintf("Concurrent N=%d: p50=%.0f, p95=%.0f, p99=%.0f\n",
              N, quantile(concurrent_vals[1:(idx-1)], 0.5),
              quantile(concurrent_vals[1:(idx-1)], 0.95),
              quantile(concurrent_vals[1:(idx-1)], 0.99)))
}

# ── Plot 1: New CPS scaling ──────────────────────────────────────────────
p1 <- scaling_data |>
  filter(grepl("New", outcome)) |>
  ggplot(aes(x = value, fill = N, colour = N)) +
  geom_density(alpha = 0.3, linewidth = 0.5) +
  facet_wrap(~outcome, scales = "free", ncol = 2) +
  labs(title = "New Connections per Second — N-User Scaling",
       subtitle = "Posterior predictive distributions for N = 1, 10, 50, 100 users",
       x = "New connections/s", y = "Density") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# ── Plot 2: Terminations/min scaling ─────────────────────────────────────
term_data <- scaling_data |>
  filter(grepl("Term", outcome)) |>
  mutate(value = value * 60)  # convert /s to /min

p2 <- term_data |>
  ggplot(aes(x = value, fill = N, colour = N)) +
  geom_density(alpha = 0.3, linewidth = 0.5) +
  facet_wrap(~outcome, scales = "free", ncol = 2,
             labeller = labeller(outcome = c(
               "TCP Terminations/s" = "TCP Terminations/min",
               "UDP Terminations/s" = "UDP Terminations/min"))) +
  labs(title = "Terminations per Minute — N-User Scaling (Log Lines/Min)",
       subtitle = "Posterior predictive distributions for N = 1, 10, 50, 100 users",
       x = "Terminations/min (≈ log lines/min)", y = "Density") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# ── Plot 3: Concurrent connections ───────────────────────────────────────
p3 <- concurrent_data |>
  ggplot(aes(x = value, fill = N, colour = N)) +
  geom_density(alpha = 0.3, linewidth = 0.5) +
  labs(title = "Concurrent Connections (TCP + UDP) — N-User Scaling",
       subtitle = "Simulated steady-state concurrent connections (1-hour simulation)",
       x = "Concurrent connections", y = "Density") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# ── Helper: clip to p99 of the largest N group for readable plots ─────────
clip_to_p99 <- function(df) {
  cap <- quantile(df$value, 0.99)
  df |> filter(value <= cap)
}

# ── Plot 1: New CPS ──────────────────────────────────────────────────────
new_data <- scaling_data |> filter(grepl("New", outcome))
new_clipped <- new_data |> group_by(outcome) |> group_modify(~ clip_to_p99(.x)) |> ungroup()

p1 <- new_clipped |>
  mutate(N = factor(N, levels = c("N=1", "N=10", "N=50", "N=100"))) |>
  ggplot(aes(x = value, fill = N, colour = N)) +
  geom_density(alpha = 0.3, linewidth = 0.5) +
  facet_wrap(~outcome, scales = "free", ncol = 2) +
  labs(title = "New Connections per Second", x = "New connections/s", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# ── Plot 2: Terminations/min ─────────────────────────────────────────────
term_data <- scaling_data |>
  filter(grepl("Term", outcome)) |>
  mutate(value = value * 60,
         outcome = sub("/s$", "/min", outcome))
term_clipped <- term_data |> group_by(outcome) |> group_modify(~ clip_to_p99(.x)) |> ungroup()

p2 <- term_clipped |>
  mutate(N = factor(N, levels = c("N=1", "N=10", "N=50", "N=100"))) |>
  ggplot(aes(x = value, fill = N, colour = N)) +
  geom_density(alpha = 0.3, linewidth = 0.5) +
  facet_wrap(~outcome, scales = "free", ncol = 2) +
  labs(title = "Terminations per Minute (≈ Log Lines/Min)",
       x = "Terminations/min", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# ── Plot 3: Concurrent connections ───────────────────────────────────────
conc_clipped <- concurrent_data |> clip_to_p99()

p3 <- conc_clipped |>
  mutate(N = factor(N, levels = c("N=1", "N=10", "N=50", "N=100"))) |>
  ggplot(aes(x = value, fill = N, colour = N)) +
  geom_density(alpha = 0.3, linewidth = 0.5) +
  labs(title = "Concurrent Connections (TCP + UDP)",
       x = "Concurrent connections", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# ── Save as 3-row combined PNG using grid viewports ──────────────────────
png("/work/plots/scaling_N_users.png", width = 14, height = 16, units = "in", res = 150)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(
  nrow = 4, ncol = 1, heights = grid::unit(c(0.5, 5, 5, 5), c("lines", "null", "null", "null")))))

grid::pushViewport(grid::viewport(layout.pos.row = 1))
grid::grid.text("Firewall Sizing: N-User Scaling — m3 Hierarchical NegBin",
                gp = grid::gpar(fontface = "bold", fontsize = 16))
grid::popViewport()

print(p1, vp = grid::viewport(layout.pos.row = 2))
print(p2, vp = grid::viewport(layout.pos.row = 3))
print(p3, vp = grid::viewport(layout.pos.row = 4))
dev.off()
cat("\nScaling plot saved to /work/plots/scaling_N_users.png\n")

# ── Print scaling summaries ──────────────────────────────────────────────
cat("\n=== Scaling Summary ===\n")
for (oc_label in unique(scaling_data$outcome)) {
  cat(sprintf("\n%s:\n", oc_label))
  scaling_data |>
    filter(outcome == oc_label) |>
    group_by(N) |>
    summarise(
      mean = mean(value),
      p50 = quantile(value, 0.5),
      p95 = quantile(value, 0.95),
      p99 = quantile(value, 0.99),
      .groups = "drop"
    ) |>
    print()
}
