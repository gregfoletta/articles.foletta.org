## Data Exploration for TCP/UDP CPS Modelling
## Loads packet captures from /data, infers local IPs per machine, and
## builds per-second event counts for the four CPS series.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(readr)
})

set.seed(42)

# ---------- 1. Load all .Rds files and tag with machine_id ------------------

files <- list.files("/data", pattern = "\\.Rds$", full.names = TRUE)
stopifnot(length(files) > 0)

cat("Loading", length(files), "files:\n")
for (f in files) cat("  ", basename(f), "\n")

data_raw <- map_dfr(files, function(f) {
  readRDS(f) |>
    mutate(machine_id = tools::file_path_sans_ext(tools::file_path_sans_ext(basename(f))))
})

# Cast columns to appropriate types
data <- data_raw |>
  mutate(
    timestamp        = as.numeric(timestamp),     # ms since epoch
    ip_version       = as.integer(ip_version),
    tcp_completeness = suppressWarnings(as.integer(tcp_completeness)),
    stream_id        = as.integer(stream_id),
    protocol         = as.character(protocol)
  )

cat("\nTotal rows:", nrow(data),
    "  TCP:", sum(data$protocol == "tcp"),
    "  UDP:", sum(data$protocol == "udp"), "\n")

# ---------- 2. Infer local workstation IP per machine x ip_version ----------

local_ips <- data |>
  filter(protocol == "tcp", tcp_completeness == 0L) |>
  group_by(machine_id, ip_version) |>
  count(src_ip, sort = TRUE) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(machine_id, ip_version, local_ip = src_ip)

cat("\nInferred local IPs:\n")
print(local_ips)

# Filter to only outbound traffic (src_ip == local_ip) for each machine
data_local <- data |>
  inner_join(local_ips, by = c("machine_id", "ip_version")) |>
  filter(src_ip == local_ip)

cat("\nRows after restricting to outbound (src=local):", nrow(data_local), "\n")

# ---------- 3. Per-machine summary ------------------------------------------

per_machine <- data_local |>
  group_by(machine_id) |>
  summarise(
    n_rows   = n(),
    t_min_ms = min(timestamp),
    t_max_ms = max(timestamp),
    .groups  = "drop"
  ) |>
  mutate(duration_s = (t_max_ms - t_min_ms) / 1000)

ip_summary <- local_ips |>
  group_by(machine_id) |>
  summarise(local_ips = paste0("v", ip_version, "=", local_ip, collapse = "; "),
            .groups = "drop")

per_machine <- per_machine |> left_join(ip_summary, by = "machine_id")

cat("\nPer-machine summary:\n")
print(per_machine)

# ---------- 4. Event extraction per machine ---------------------------------

# TCP: a stream's "new" event is its tcp_completeness==0 packet, but only
#      count streams that ever reach a terminal completeness (15/31/63).
# TCP: termination event is the FIRST packet whose completeness is 15/31/63.

tcp_local <- data_local |>
  filter(protocol == "tcp")

tcp_streams_completed <- tcp_local |>
  group_by(machine_id, stream_id) |>
  summarise(
    has_syn      = any(tcp_completeness == 0L, na.rm = TRUE),
    has_terminal = any(tcp_completeness %in% c(15L, 31L, 63L), na.rm = TRUE),
    .groups      = "drop"
  ) |>
  filter(has_syn, has_terminal)

cat("\nTCP streams with SYN+terminal:", nrow(tcp_streams_completed),
    "of", length(unique(paste(tcp_local$machine_id, tcp_local$stream_id))), "\n")

# New TCP events: packet rows where completeness==0 for completed streams
tcp_new_events <- tcp_local |>
  semi_join(tcp_streams_completed, by = c("machine_id", "stream_id")) |>
  filter(tcp_completeness == 0L) |>
  group_by(machine_id, stream_id) |>
  slice_min(timestamp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(machine_id, ts_ms = timestamp)

# Termination events: FIRST packet reaching terminal completeness per stream
tcp_end_events <- tcp_local |>
  filter(tcp_completeness %in% c(15L, 31L, 63L)) |>
  group_by(machine_id, stream_id) |>
  slice_min(timestamp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(machine_id, ts_ms = timestamp)

# UDP new = first packet of each (machine_id, stream_id) outbound
udp_local <- data_local |>
  filter(protocol == "udp")

udp_new_events <- udp_local |>
  group_by(machine_id, stream_id) |>
  slice_min(timestamp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(machine_id, ts_ms = timestamp)

# UDP end = last packet of each stream — but we need to check if the LAST
# packet was outbound or inbound. Per CONTEXT: "use the last packet of each
# stream"; we observed only outbound rows in udp_local already, but actual
# last packet may be inbound. To approximate, use last packet across both
# directions to mark termination time. Pull from full data (not data_local).
udp_end_events <- data |>
  filter(protocol == "udp") |>
  inner_join(local_ips |> select(machine_id, ip_version), by = c("machine_id", "ip_version")) |>
  # keep only streams that had at least one outbound packet
  semi_join(udp_new_events |> distinct(machine_id) |>
              inner_join(udp_local |> distinct(machine_id, stream_id),
                         by = "machine_id"),
            by = c("machine_id", "stream_id")) |>
  group_by(machine_id, stream_id) |>
  slice_max(timestamp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(machine_id, ts_ms = timestamp)

cat("\nEvent counts per machine:\n")
events_summary <- bind_rows(
  tcp_new_events |> mutate(kind = "tcp_new"),
  tcp_end_events |> mutate(kind = "tcp_end"),
  udp_new_events |> mutate(kind = "udp_new"),
  udp_end_events |> mutate(kind = "udp_end")
) |>
  count(machine_id, kind) |>
  pivot_wider(names_from = kind, values_from = n, values_fill = 0L)
print(events_summary)

# ---------- 5. Build per-second time series per machine ---------------------

# Compute machine-level reference second so seconds are 1-indexed within machine
machine_t0 <- data_local |>
  group_by(machine_id) |>
  summarise(t0_s = floor(min(timestamp) / 1000), .groups = "drop")

machine_t1 <- data_local |>
  group_by(machine_id) |>
  summarise(t1_s = floor(max(timestamp) / 1000), .groups = "drop")

machine_seconds <- machine_t0 |>
  inner_join(machine_t1, by = "machine_id") |>
  mutate(n_sec = t1_s - t0_s + 1L)

cat("\nMachine seconds:\n")
print(machine_seconds)

# Build full grid of (machine_id, second) and join event counts
grid <- machine_seconds |>
  rowwise() |>
  do(tibble(machine_id = .$machine_id, second = seq_len(.$n_sec))) |>
  ungroup()

bin_events <- function(events, t0_lookup) {
  events |>
    inner_join(t0_lookup, by = "machine_id") |>
    mutate(second = as.integer(floor(ts_ms / 1000) - t0_s + 1L)) |>
    count(machine_id, second, name = "n")
}

tcp_new_bin <- bin_events(tcp_new_events, machine_t0) |> rename(tcp_new = n)
tcp_end_bin <- bin_events(tcp_end_events, machine_t0) |> rename(tcp_end = n)
udp_new_bin <- bin_events(udp_new_events, machine_t0) |> rename(udp_new = n)
udp_end_bin <- bin_events(udp_end_events, machine_t0) |> rename(udp_end = n)

cps <- grid |>
  left_join(tcp_new_bin, by = c("machine_id", "second")) |>
  left_join(tcp_end_bin, by = c("machine_id", "second")) |>
  left_join(udp_new_bin, by = c("machine_id", "second")) |>
  left_join(udp_end_bin, by = c("machine_id", "second")) |>
  mutate(across(c(tcp_new, tcp_end, udp_new, udp_end),
                ~ as.integer(replace_na(.x, 0L)))) |>
  arrange(machine_id, second) |>
  mutate(minute = ((second - 1L) %/% 60L) + 1L)

# Add integer machine index
machine_levels <- sort(unique(cps$machine_id))
cps <- cps |>
  mutate(machine = as.integer(factor(machine_id, levels = machine_levels))) |>
  select(machine_id, machine, second, minute, tcp_new, udp_new, tcp_end, udp_end)

# ---------- 6. Summary statistics -------------------------------------------

summarise_series <- function(x) {
  vm <- if (mean(x) > 0) var(x) / mean(x) else NA_real_
  tibble(
    n_seconds = length(x),
    mean      = mean(x),
    sd        = sd(x),
    median    = median(x),
    p90       = quantile(x, 0.90, names = FALSE),
    p99       = quantile(x, 0.99, names = FALSE),
    max       = max(x),
    prop_zero = mean(x == 0),
    var_mean  = vm
  )
}

per_machine_stats <- cps |>
  pivot_longer(c(tcp_new, udp_new, tcp_end, udp_end),
               names_to = "series", values_to = "x") |>
  group_by(machine_id, series) |>
  summarise(summarise_series(x), .groups = "drop")

pooled_stats <- cps |>
  pivot_longer(c(tcp_new, udp_new, tcp_end, udp_end),
               names_to = "series", values_to = "x") |>
  group_by(series) |>
  summarise(summarise_series(x), .groups = "drop") |>
  mutate(machine_id = "POOLED", .before = 1)

stats_all <- bind_rows(per_machine_stats, pooled_stats) |>
  arrange(series, machine_id)

cat("\n----- Summary statistics for the four CPS series -----\n")
options(pillar.sigfig = 4)
print(stats_all, n = Inf, width = Inf)

# Persist to disk
saveRDS(cps, "/work/diagnostics/cps_data.rds")
write_csv(stats_all,    "/work/diagnostics/cps_stats.csv")
write_csv(per_machine,  "/work/diagnostics/per_machine.csv")
write_csv(local_ips,    "/work/diagnostics/local_ips.csv")
cat("\nSaved /work/diagnostics/cps_data.rds (", nrow(cps), "rows )\n")
cat("Machine index:\n")
print(distinct(cps, machine, machine_id))
