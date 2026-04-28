#!/usr/bin/env Rscript

#### Tshark Command Line
# tshark -i wlp0s20f3 -T ek -a duration:60 -e ip.version -e ip.src -e ip.dst -e ipv6.src -e ipv6.dst -e tcp.srcport -e tcp.dstport -e tcp.stream -e tcp.time_relative -e tcp.completeness -e udp.srcport -e udp.dstport -e udp.stream -e udp.time_relative 2>/dev/null | egrep -v index > file
####

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(jsonlite, quietly = TRUE, warn.conflicts = FALSE)
library(archive, quietly = TRUE, warn.conflicts = FALSE)

args <- commandArgs(trailingOnly = TRUE)

print(args)

read_and_clean_ek_packets <- function(archive, rds) {
    archive_read(archive) |>  
    stream_in(simplifyVector = TRUE, flatten = FALSE) |>
        as_tibble() |>
        unnest(layers) |>
        unnest(everything()) |>
        mutate(
            packet_id = 1:n(),
            src_ip = coalesce(ip_src, ipv6_src),
            dst_ip = coalesce(ip_dst, ipv6_dst),
            src_port = coalesce(tcp_srcport, udp_srcport),
            dst_port = coalesce(tcp_dstport, udp_dstport),
            time_relative = coalesce(tcp_time_relative, udp_time_relative),
        ) |>
        select(-c(contains('_src'), contains('_dst'), 'tcp_time_relative', 'udp_time_relative')) |>
        pivot_longer(c(udp_stream, tcp_stream), names_to = 'protocol', values_to = 'stream_id', values_drop_na = TRUE, names_transform = ~{ str_remove(.x, "_stream") }) |> 
	mutate(
	    timestamp = as.double(timestamp),
	    ip_version = as.integer(ip_version),
	    time_relative = as.double(time_relative),
	    protocol = as.factor(protocol),
	    stream_id = as.integer(stream_id)
	) |>
        write_rds(rds, compress = 'gz')
}


read_and_clean_ek_packets(args[1], args[2])
