#!/usr/bin/env Rscript

#### Tshark Command Line
# tshark -i wlp0s20f3 -T ek -a duration:60 -e ip.version -e ip.src -e ip.dst -e ipv6.src -e ipv6.dst -e tcp.srcport -e tcp.dstport -e tcp.stream -e tcp.time_relative -e tcp.completeness -e udp.srcport -e udp.dstport -e udp.stream -e udp.time_relative 2>/dev/null | egrep -v index > file
####

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(jsonlite, quietly = TRUE, warn.conflicts = FALSE)

args <- commandArgs(trailingOnly = TRUE)

print(args)

read_and_clean_ek_packets <- function(jsonl, rds) {
    stream_in(file(jsonl), simplifyVector = TRUE, flatten = FALSE) |>
        as_tibble() |>
        unnest(layers) |>
        unnest(everything()) |>
        mutate(
            id = 1:n(),
            src_ip = coalesce(ip_src, ipv6_src),
            dst_ip = coalesce(ip_dst, ipv6_dst),
            src_port = coalesce(tcp_srcport, udp_srcport),
            dst_port = coalesce(tcp_dstport, udp_dstport),
        ) |>
        select(-c(contains('_src'), contains('_dst'))) |>
        pivot_longer(c(udp_stream, tcp_stream), names_to = 'protocol', values_drop_na = TRUE,names_transform = ~{ str_remove(.x, "_stream") }) |>
        select(id, timestamp, protocol, ip_version, src_ip, dst_ip, src_port, dst_port, tcp_time_relative, tcp_completeness) |>
        write_rds(rds, compress = 'gz')
}


read_and_clean_ek_packets(args[1], args[2])