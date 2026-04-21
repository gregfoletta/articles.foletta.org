library(tidyverse)
library(cmdstanr)
library(posterior)

# ── 0. Config ─────────────────────────────────────────────────────────────
model_file   <- "/work/models/m3.stan"
model_label  <- "m3: Hierarchical NegBin (new + terminations)"
iter_num     <- 3
ppc_file     <- sprintf("/work/plots/ppc_iter_%d.png", iter_num)

# ── 1. Load data ───────────────────────────────────────────────────────────
cps <- readRDS("/work/diagnostics/cps_data.rds")
cat("Loaded CPS data:", nrow(cps), "rows,", length(unique(cps$machine_id)), "machines\n")

# ── 2. Build Stan data list ───────────────────────────────────────────────
stan_data <- list(
  T       = nrow(cps),
  K       = length(unique(cps$machine)),
  machine = as.integer(cps$machine),
  tcp_new = as.integer(cps$tcp_new),
  udp_new = as.integer(cps$udp_new),
  tcp_end = as.integer(cps$tcp_end),
  udp_end = as.integer(cps$udp_end)
)

cat(sprintf("Stan data: T=%d, K=%d\n", stan_data$T, stan_data$K))

# ── 3. Compile and sample ─────────────────────────────────────────────────
mod <- cmdstan_model(model_file)

fit <- mod$sample(
  data            = stan_data,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 1000,
  iter_sampling   = 1000,
  seed            = 42,
  adapt_delta     = 0.95,
  show_messages   = FALSE,
  refresh         = 200
)

# ── 4. Save fit ───────────────────────────────────────────────────────────
fit$save_object("/work/diagnostics/fit.rds")
cat("Fit saved to /work/diagnostics/fit.rds\n")

# ── 5. Diagnostics ────────────────────────────────────────────────────────
diag_lines <- character()
add_line <- function(x) diag_lines <<- c(diag_lines, x)

num_div <- sum(fit$diagnostic_summary()$num_divergent)
add_line(sprintf("Divergent transitions: %d", num_div))

ebfmi <- fit$diagnostic_summary()$ebfmi
add_line(sprintf("E-BFMI per chain: %s", paste(round(ebfmi, 3), collapse = ", ")))

K <- stan_data$K
sum_vars <- c(
  "alpha_tcp_new", "alpha_udp_new", "alpha_tcp_end", "alpha_udp_end",
  "sigma_tcp_new", "sigma_udp_new", "sigma_tcp_end", "sigma_udp_end",
  "phi_tcp_new", "phi_udp_new", "phi_tcp_end", "phi_udp_end",
  paste0("mu_tcp_new[", 1:K, "]"), paste0("mu_udp_new[", 1:K, "]"),
  paste0("mu_tcp_end[", 1:K, "]"), paste0("mu_udp_end[", 1:K, "]")
)
summ <- fit$summary(variables = sum_vars)
add_line("")
add_line("Parameter summary:")
add_line(paste(capture.output(print(as.data.frame(summ), digits = 4)), collapse = "\n"))

max_rhat <- max(summ$rhat, na.rm = TRUE)
min_ess_bulk <- min(summ$ess_bulk, na.rm = TRUE)
min_ess_tail <- min(summ$ess_tail, na.rm = TRUE)

add_line("")
add_line(sprintf("Max Rhat: %.4f (%s)", max_rhat,
                 summ$variable[which.max(summ$rhat)]))
add_line(sprintf("Min Bulk ESS: %.0f (%s)", min_ess_bulk,
                 summ$variable[which.min(summ$ess_bulk)]))
add_line(sprintf("Min Tail ESS: %.0f (%s)", min_ess_tail,
                 summ$variable[which.min(summ$ess_tail)]))

if (max_rhat > 1.01) add_line("WARNING: Rhat > 1.01 detected!")
if (min_ess_bulk < 400) add_line("WARNING: Bulk ESS < 400 detected!")
if (min_ess_tail < 400) add_line("WARNING: Tail ESS < 400 detected!")

diag_text <- paste(diag_lines, collapse = "\n")
cat(diag_text, "\n")
writeLines(diag_text, "/work/diagnostics/summary.txt")
cat("Diagnostics saved to /work/diagnostics/summary.txt\n")

# ── 6. PPC plot (generated in R, not Stan) ───────────────────────────────
# Extract machine-level mu and phi draws
mu_draws <- fit$draws(
  variables = c(
    paste0("mu_tcp_new[", 1:K, "]"), paste0("mu_udp_new[", 1:K, "]"),
    paste0("mu_tcp_end[", 1:K, "]"), paste0("mu_udp_end[", 1:K, "]"),
    "phi_tcp_new", "phi_udp_new", "phi_tcp_end", "phi_udp_end"
  ),
  format = "draws_matrix"
)

set.seed(42)
n_ppc <- 50
draw_ids <- sample(nrow(mu_draws), n_ppc)
machine_vec <- stan_data$machine

outcomes <- list(
  list(obs = stan_data$tcp_new, mu_prefix = "mu_tcp_new", phi_name = "phi_tcp_new", label = "TCP New"),
  list(obs = stan_data$udp_new, mu_prefix = "mu_udp_new", phi_name = "phi_udp_new", label = "UDP New"),
  list(obs = stan_data$tcp_end, mu_prefix = "mu_tcp_end", phi_name = "phi_tcp_end", label = "TCP Terminations"),
  list(obs = stan_data$udp_end, mu_prefix = "mu_udp_end", phi_name = "phi_udp_end", label = "UDP Terminations")
)

ppc_data <- tibble()

for (oc in outcomes) {
  cap <- quantile(oc$obs, 0.995)
  ppc_data <- bind_rows(ppc_data,
    tibble(value = pmin(oc$obs, cap), draw = "observed", protocol = oc$label))

  for (i in seq_along(draw_ids)) {
    d <- draw_ids[i]
    # Get machine-level mu for this draw
    mu_k <- numeric(K)
    for (k in 1:K) {
      mu_k[k] <- mu_draws[d, paste0(oc$mu_prefix, "[", k, "]")]
    }
    phi_val <- mu_draws[d, oc$phi_name]

    # Generate replicate data
    mu_vec <- mu_k[machine_vec]
    rep_vals <- rnbinom(length(mu_vec), mu = mu_vec, size = phi_val)
    ppc_data <- bind_rows(ppc_data,
      tibble(value = pmin(rep_vals, cap), draw = paste0("rep_", i), protocol = oc$label))
  }
}

p <- ggplot() +
  geom_density(
    data = ppc_data |> filter(draw != "observed"),
    aes(x = value, group = draw),
    colour = alpha("steelblue", 0.15), linewidth = 0.3
  ) +
  geom_density(
    data = ppc_data |> filter(draw == "observed"),
    aes(x = value),
    colour = "black", linewidth = 1.2
  ) +
  facet_wrap(~protocol, scales = "free", ncol = 2) +
  labs(
    title = sprintf("%s — Iteration %d PPC", model_label, iter_num),
    subtitle = "Black = observed; blue = posterior predictive draws (n=50)",
    x = "Events per second",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

ggsave(ppc_file, p, width = 12, height = 8, dpi = 150)
cat(sprintf("PPC plot saved to %s\n", ppc_file))

# ── 7. Status flag ────────────────────────────────────────────────────────
writeLines("CONTINUE", "/work/diagnostics/status.txt")
cat("Status: CONTINUE\n")
