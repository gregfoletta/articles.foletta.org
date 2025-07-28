---
title: CLT
author: Greg Foletta
date: '2025-07-14'
slug: []
categories: []
tags: []
---

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tictoc)
library(gt)
```

``` r
random_data <- tibble(
    uniform = runif(10000, min = -20, max = 20),
    normal = rnorm(10000, mean = 0, sd = 4),
    binomial = rbinom(10000, size = 1, prob = .5),
    beta = rbeta(10000, shape1 = .9, shape2 = .5),
    exponential = rexp(10000, .4),
    chisquare = rchisq(10000, df = 2),
)
```

``` r
random_data_l <-
    random_data |>
    pivot_longer(cols = everything(), names_to = 'distribution') 
```

``` r
random_data_l |>
    ggplot() + 
    geom_histogram(aes(value), binwidth = .3) +
    facet_wrap(vars(distribution), scales = 'free')
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

``` r
sample_size <- 60

random_means <- 
    map(1:20000, 
        \(x) slice_sample(random_data, n = sample_size) |>
            summarise(across(everything(), ~mean(.x) ))
    ) |> 
    list_rbind()
```

``` r
random_means |>
    pivot_longer(everything(), names_to = 'distribution') |>
    ggplot() +
    geom_histogram(aes(value), binwidth = .01) +
    facet_wrap(vars(distribution), scales = 'free_x')
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

``` r
random_data_stats <-
    random_data_l|>
    group_by(distribution) |>
    summarise(
        population_mean = mean(value),
        population_sd = sd(value),
        n = n()
    )

random_data_stats |>
    gt()
```

<div id="wggluullpj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#wggluullpj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#wggluullpj thead, #wggluullpj tbody, #wggluullpj tfoot, #wggluullpj tr, #wggluullpj td, #wggluullpj th {
  border-style: none;
}
&#10;#wggluullpj p {
  margin: 0;
  padding: 0;
}
&#10;#wggluullpj .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#wggluullpj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#wggluullpj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#wggluullpj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#wggluullpj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wggluullpj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wggluullpj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wggluullpj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#wggluullpj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#wggluullpj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#wggluullpj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#wggluullpj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#wggluullpj .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#wggluullpj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#wggluullpj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#wggluullpj .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#wggluullpj .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#wggluullpj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#wggluullpj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wggluullpj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#wggluullpj .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#wggluullpj .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#wggluullpj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wggluullpj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#wggluullpj .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#wggluullpj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wggluullpj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wggluullpj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#wggluullpj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#wggluullpj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#wggluullpj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wggluullpj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wggluullpj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wggluullpj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wggluullpj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wggluullpj .gt_left {
  text-align: left;
}
&#10;#wggluullpj .gt_center {
  text-align: center;
}
&#10;#wggluullpj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#wggluullpj .gt_font_normal {
  font-weight: normal;
}
&#10;#wggluullpj .gt_font_bold {
  font-weight: bold;
}
&#10;#wggluullpj .gt_font_italic {
  font-style: italic;
}
&#10;#wggluullpj .gt_super {
  font-size: 65%;
}
&#10;#wggluullpj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#wggluullpj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#wggluullpj .gt_indent_1 {
  text-indent: 5px;
}
&#10;#wggluullpj .gt_indent_2 {
  text-indent: 10px;
}
&#10;#wggluullpj .gt_indent_3 {
  text-indent: 15px;
}
&#10;#wggluullpj .gt_indent_4 {
  text-indent: 20px;
}
&#10;#wggluullpj .gt_indent_5 {
  text-indent: 25px;
}
&#10;#wggluullpj .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#wggluullpj div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="distribution">distribution</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="population_mean">population_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="population_sd">population_sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">n</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="population_mean" class="gt_row gt_right">0.6389556695</td>
<td headers="population_sd" class="gt_row gt_right">0.3101294</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="population_mean" class="gt_row gt_right">0.5034000000</td>
<td headers="population_sd" class="gt_row gt_right">0.5000134</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="population_mean" class="gt_row gt_right">1.9938687769</td>
<td headers="population_sd" class="gt_row gt_right">1.9914316</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="population_mean" class="gt_row gt_right">2.4903542043</td>
<td headers="population_sd" class="gt_row gt_right">2.4556840</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="population_mean" class="gt_row gt_right">0.0003608079</td>
<td headers="population_sd" class="gt_row gt_right">3.9982925</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="population_mean" class="gt_row gt_right">0.0555525539</td>
<td headers="population_sd" class="gt_row gt_right">11.5996417</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
# CLT Calculation
clt <-
    random_means |>
    pivot_longer(
        cols = everything(),
        names_to = 'distribution',
        values_to = 'sample_mean'    
    ) |>
    left_join(random_data_stats, by = 'distribution') |>
    mutate(clt = (sample_mean - population_mean) / (population_sd / sqrt(n) )) 

clt |> 
    ggplot() +
    geom_histogram(aes(clt), binwidth = 1) +
    facet_wrap(vars(distribution), scales = 'free')
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

``` r
clt |>
    ggplot(aes(sample = clt, colour = distribution)) +
    geom_qq_line() +
    geom_qq(size = .3) +
    facet_wrap(vars(distribution)) +
    theme(legend.position = 'none')
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

# In Practice

``` r
take_random_sample <- function(data, sample_size) {
    slice_sample(.data = data, n = sample_size) |>
    summarise(across(everything(), list(sample_mean = mean, sample_sd = sd)))
}

small_sample_size = 10

repeated_samples <-
    map(1:10000, ~take_random_sample(random_data, sample_size = small_sample_size)) |>
    list_rbind() |> 
    pivot_longer(
        everything(),
        names_to = c('distribution', '.value'),
        names_pattern = '(\\w+)_(\\w+_\\w+)'
    ) |>
    left_join(random_data_stats, by = 'distribution') |>
    select(-c(n, population_sd))
```

``` r
repeated_samples |> slice_head(n = 6) |> gt()
```

<div id="ktjaxghfxn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ktjaxghfxn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ktjaxghfxn thead, #ktjaxghfxn tbody, #ktjaxghfxn tfoot, #ktjaxghfxn tr, #ktjaxghfxn td, #ktjaxghfxn th {
  border-style: none;
}
&#10;#ktjaxghfxn p {
  margin: 0;
  padding: 0;
}
&#10;#ktjaxghfxn .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ktjaxghfxn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ktjaxghfxn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ktjaxghfxn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ktjaxghfxn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ktjaxghfxn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ktjaxghfxn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ktjaxghfxn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ktjaxghfxn .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ktjaxghfxn .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ktjaxghfxn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ktjaxghfxn .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ktjaxghfxn .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ktjaxghfxn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ktjaxghfxn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ktjaxghfxn .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ktjaxghfxn .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ktjaxghfxn .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ktjaxghfxn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ktjaxghfxn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ktjaxghfxn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ktjaxghfxn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ktjaxghfxn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ktjaxghfxn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ktjaxghfxn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ktjaxghfxn .gt_left {
  text-align: left;
}
&#10;#ktjaxghfxn .gt_center {
  text-align: center;
}
&#10;#ktjaxghfxn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ktjaxghfxn .gt_font_normal {
  font-weight: normal;
}
&#10;#ktjaxghfxn .gt_font_bold {
  font-weight: bold;
}
&#10;#ktjaxghfxn .gt_font_italic {
  font-style: italic;
}
&#10;#ktjaxghfxn .gt_super {
  font-size: 65%;
}
&#10;#ktjaxghfxn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ktjaxghfxn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ktjaxghfxn .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ktjaxghfxn .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ktjaxghfxn .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ktjaxghfxn .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ktjaxghfxn .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ktjaxghfxn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ktjaxghfxn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="distribution">distribution</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sample_mean">sample_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sample_sd">sample_sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="population_mean">population_mean</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="sample_mean" class="gt_row gt_right">-0.7274557</td>
<td headers="sample_sd" class="gt_row gt_right">10.8145829</td>
<td headers="population_mean" class="gt_row gt_right">0.0555525539</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="sample_mean" class="gt_row gt_right">0.4796856</td>
<td headers="sample_sd" class="gt_row gt_right">5.6266564</td>
<td headers="population_mean" class="gt_row gt_right">0.0003608079</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="sample_mean" class="gt_row gt_right">0.5000000</td>
<td headers="sample_sd" class="gt_row gt_right">0.5270463</td>
<td headers="population_mean" class="gt_row gt_right">0.5034000000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="sample_mean" class="gt_row gt_right">0.6616597</td>
<td headers="sample_sd" class="gt_row gt_right">0.2597492</td>
<td headers="population_mean" class="gt_row gt_right">0.6389556695</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="sample_mean" class="gt_row gt_right">3.9356742</td>
<td headers="sample_sd" class="gt_row gt_right">3.0614015</td>
<td headers="population_mean" class="gt_row gt_right">2.4903542043</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="sample_mean" class="gt_row gt_right">0.9213979</td>
<td headers="sample_sd" class="gt_row gt_right">0.9298744</td>
<td headers="population_mean" class="gt_row gt_right">1.9938687769</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
conf_intervals <-
    repeated_samples |> 
    mutate(
        upper_ci = sample_mean + qnorm(0.975) * sample_sd / sqrt(small_sample_size),
        lower_ci = sample_mean - qnorm(0.975) * sample_sd / sqrt(small_sample_size),
        within_ci = population_mean < upper_ci & population_mean > lower_ci
    )
```

``` r
conf_intervals |>
    group_by(distribution) |> 
    summarise(
        percent_within_ci = mean(within_ci))
```

    ## # A tibble: 6 × 2
    ##   distribution percent_within_ci
    ##   <chr>                    <dbl>
    ## 1 beta                     0.909
    ## 2 binomial                 0.894
    ## 3 chisquare                0.862
    ## 4 exponential              0.875
    ## 5 normal                   0.915
    ## 6 uniform                  0.916

# Using the t-distribution

``` r
repeated_samples |> 
    mutate(
        upper_ci = sample_mean + qt(p = 0.975, df = small_sample_size - 1) * (sample_sd / sqrt(small_sample_size)),
        lower_ci = sample_mean - qt(p = 0.975, df = small_sample_size - 1) * (sample_sd / sqrt(small_sample_size)),
        within_ci = population_mean < upper_ci & population_mean > lower_ci
    ) |> 
    group_by(distribution) |> 
    summarise(percent = mean(within_ci))
```

    ## # A tibble: 6 × 2
    ##   distribution percent
    ##   <chr>          <dbl>
    ## 1 beta           0.939
    ## 2 binomial       0.938
    ## 3 chisquare      0.897
    ## 4 exponential    0.906
    ## 5 normal         0.947
    ## 6 uniform        0.944

# Larger Sample Size

``` r
large_sample_size <- 60

map(1:10000, ~take_random_sample(random_data, sample_size = large_sample_size)) |>
    list_rbind() |> 
    pivot_longer(
        everything(),
        names_to = c('distribution', '.value'),
        names_pattern = '(\\w+)_(\\w+_\\w+)'
    ) |>
    left_join(random_data_stats, by = 'distribution') |>
    select(-c(n, population_sd)) |> 
    mutate(
        upper_ci = sample_mean + qnorm(0.975) * (sample_sd / sqrt(large_sample_size)),
        lower_ci = sample_mean - qnorm(0.975) * (sample_sd / sqrt(large_sample_size)),
        within_ci = population_mean < upper_ci & population_mean > lower_ci
    ) |> 
    group_by(distribution) |> 
    summarise(percent = mean(within_ci))
```

    ## # A tibble: 6 × 2
    ##   distribution percent
    ##   <chr>          <dbl>
    ## 1 beta           0.946
    ## 2 binomial       0.950
    ## 3 chisquare      0.933
    ## 4 exponential    0.934
    ## 5 normal         0.945
    ## 6 uniform        0.948
