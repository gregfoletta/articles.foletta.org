library(tidyverse)

# ── 1. Load all .Rds files ─────────────────────────────────────────────────
files <- list.files("/data", pattern = "\\.Rds$", full.names = TRUE)
cat("Found files:", basename(files), "\n\n")

data <- purrr::map_dfr(files, ~ readRDS(.x) |>
  dplyr::mutate(machine_id = tools::file_path_sans_ext(basename(.x))))

# Cast columns
data <- data |>
  mutate(
    id = as.integer(id),
    timestamp = as.numeric(timestamp),
    tcp_completeness = as.integer(tcp_completeness),
    src_port = as.integer(src_port),
    dst_port = as.integer(dst_port)
  )

cat("Total rows:", nrow(data), "\n")
cat("Protocols:", paste(unique(data$protocol), collapse = ", "), "\n")
cat("Machines:", paste(unique(data$machine_id), collapse = ", "), "\n\n")

# ── 2. Infer local workstation IP per machine ──────────────────────────────
local_ips <- data |>
  filter(protocol == "tcp", tcp_completeness == 0L) |>
  group_by(machine_id, ip_version) |>
  count(src_ip, sort = TRUE) |>
  slice_max(n, n = 1) |>
  ungroup() |>
  select(machine_id, ip_version, local_ip = src_ip)

cat("Local IPs per machine:\n")
print(as.data.frame(local_ips))
cat("\n")

# ── 3. Per-machine summary ─────────────────────────────────────────────────
machine_summary <- data |>
  group_by(machine_id) |>
  summarise(
    n_rows = n(),
    time_span_s = (max(timestamp) - min(timestamp)) / 1000,
    .groups = "drop"
  )

# Join local IPs (collapsed per machine)
local_ips_collapsed <- local_ips |>
  group_by(machine_id) |>
  summarise(local_ips = paste(local_ip, collapse = ", "), .groups = "drop")

machine_summary <- machine_summary |>
  left_join(local_ips_collapsed, by = "machine_id")

cat("Per-machine summary:\n")
for (i in seq_len(nrow(machine_summary))) {
  r <- machine_summary[i, ]
  cat(sprintf("  %s: %d rows, %.1f seconds (%.1f minutes), IPs: %s\n",
              r$machine_id, r$n_rows, r$time_span_s,
              r$time_span_s / 60, r$local_ips))
}
cat("\n")

# ── 4. Filter to outbound traffic ──────────────────────────────────────────
outbound <- data |>
  inner_join(local_ips, by = c("machine_id", "ip_version", "src_ip" = "local_ip"))

cat("Outbound rows (src_ip == local_ip):", nrow(outbound), "\n\n")

# ── TCP new connections: tcp_completeness == 0 ─────────────────────────────
tcp_new_events <- outbound |>
  filter(protocol == "tcp", tcp_completeness == 0L) |>
  mutate(second_bin = floor(timestamp / 1000))

# ── TCP terminations: terminal completeness (15, 31, 63) ──────────────────
# For each stream (4-tuple), find the packet where completeness reaches terminal
tcp_term_events <- outbound |>
  filter(protocol == "tcp", tcp_completeness %in% c(15L, 31L, 63L)) |>
  group_by(machine_id, src_ip, dst_ip, src_port, dst_port) |>
  slice_max(timestamp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(second_bin = floor(timestamp / 1000))

# ── UDP new connections: first packet per 4-tuple ──────────────────────────
udp_new_events <- outbound |>
  filter(protocol == "udp") |>
  group_by(machine_id, src_ip, dst_ip, src_port, dst_port) |>
  slice_min(timestamp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(second_bin = floor(timestamp / 1000))

# ── UDP terminations: last packet per 4-tuple ─────────────────────────────
udp_end_events <- outbound |>
  filter(protocol == "udp") |>
  group_by(machine_id, src_ip, dst_ip, src_port, dst_port) |>
  slice_max(timestamp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(second_bin = floor(timestamp / 1000))

# ── 4b. Build per-second time series per machine ──────────────────────────
# Create full second grid per machine
second_grids <- data |>
  group_by(machine_id) |>
  summarise(
    min_sec = floor(min(timestamp) / 1000),
    max_sec = floor(max(timestamp) / 1000),
    .groups = "drop"
  ) |>
  rowwise() |>
  mutate(second_bin = list(seq(min_sec, max_sec))) |>
  ungroup() |>
  select(machine_id, second_bin) |>
  unnest(second_bin)

# Count events per second
tcp_new_counts <- tcp_new_events |>
  count(machine_id, second_bin, name = "tcp_new")

tcp_end_counts <- tcp_term_events |>
  count(machine_id, second_bin, name = "tcp_end")

udp_new_counts <- udp_new_events |>
  count(machine_id, second_bin, name = "udp_new")

udp_end_counts <- udp_end_events |>
  count(machine_id, second_bin, name = "udp_end")

cps <- second_grids |>
  left_join(tcp_new_counts, by = c("machine_id", "second_bin")) |>
  left_join(tcp_end_counts, by = c("machine_id", "second_bin")) |>
  left_join(udp_new_counts, by = c("machine_id", "second_bin")) |>
  left_join(udp_end_counts, by = c("machine_id", "second_bin")) |>
  mutate(across(c(tcp_new, tcp_end, udp_new, udp_end), ~ replace_na(.x, 0L)))

# Re-index seconds and minutes within each machine
cps <- cps |>
  group_by(machine_id) |>
  arrange(second_bin, .by_group = TRUE) |>
  mutate(
    second = row_number(),
    minute = as.integer(ceiling(second / 60))
  ) |>
  ungroup()

# Add integer machine index
machine_map <- cps |>
  distinct(machine_id) |>
  arrange(machine_id) |>
  mutate(machine = row_number())

cps <- cps |>
  left_join(machine_map, by = "machine_id")

# Ensure integer types
cps <- cps |>
  mutate(across(c(tcp_new, tcp_end, udp_new, udp_end, second, minute, machine),
                as.integer))

# ── 5. Summary statistics ─────────────────────────────────────────────────
summarise_series <- function(x, name) {
  tibble(
    series = name,
    n_seconds = length(x),
    mean = mean(x),
    sd = sd(x),
    median = median(x),
    p90 = quantile(x, 0.90),
    p99 = quantile(x, 0.99),
    max = max(x),
    pct_zero = mean(x == 0) * 100,
    var_mean_ratio = var(x) / mean(x)
  )
}

cat("=" |> strrep(80), "\n")
cat("PER-MACHINE SUMMARY STATISTICS\n")
cat("=" |> strrep(80), "\n\n")

for (mid in sort(unique(cps$machine_id))) {
  d <- cps |> filter(machine_id == mid)
  cat(sprintf("── %s (%d seconds, %.1f minutes) ──\n",
              mid, nrow(d), nrow(d) / 60))
  stats <- bind_rows(
    summarise_series(d$tcp_new, "tcp_new"),
    summarise_series(d$udp_new, "udp_new"),
    summarise_series(d$tcp_end, "tcp_end"),
    summarise_series(d$udp_end, "udp_end")
  )
  print(as.data.frame(stats), digits = 3, row.names = FALSE)
  cat("\n")
}

cat("=" |> strrep(80), "\n")
cat("POOLED SUMMARY STATISTICS\n")
cat("=" |> strrep(80), "\n\n")

pooled_stats <- bind_rows(
  summarise_series(cps$tcp_new, "tcp_new"),
  summarise_series(cps$udp_new, "udp_new"),
  summarise_series(cps$tcp_end, "tcp_end"),
  summarise_series(cps$udp_end, "udp_end")
)
print(as.data.frame(pooled_stats), digits = 3, row.names = FALSE)
cat("\n")

# Note differences between machines
cat("── Between-machine comparison (means) ──\n")
machine_means <- cps |>
  group_by(machine_id) |>
  summarise(across(c(tcp_new, udp_new, tcp_end, udp_end), mean), .groups = "drop")
print(as.data.frame(machine_means), digits = 3)
cat("\n")

cat("── Between-machine comparison (var/mean ratio) ──\n")
machine_vmr <- cps |>
  group_by(machine_id) |>
  summarise(across(c(tcp_new, udp_new, tcp_end, udp_end),
                   ~ var(.x) / mean(.x)), .groups = "drop")
print(as.data.frame(machine_vmr), digits = 3)
cat("\n")

# ── 6. Save CPS data ──────────────────────────────────────────────────────
cps_out <- cps |>
  select(machine_id, machine, second, minute, tcp_new, udp_new, tcp_end, udp_end)

saveRDS(cps_out, "/work/diagnostics/cps_data.rds")
cat("Saved CPS data to /work/diagnostics/cps_data.rds\n")
cat("Dimensions:", nrow(cps_out), "rows x", ncol(cps_out), "columns\n")
cat("Machines:", paste(sort(unique(cps_out$machine_id)), collapse = ", "), "\n")
