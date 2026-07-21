---
title: Rolling a Die and the Clustering Illusion 
author: Greg Foletta
date: '2026-07-19'
slug: []
categories: []
tags: []
---

One of the first posts I wrote on this blog was about simulating ‘Snakes and Ladders’. Simple games of chance like this are great to think about and

The question that I asked myself while playing a game with my son is “could this I detect if this dice was biased?”. To answer this quesion, I needed data.

# LLM Disclosure

# Yes I Rolled It 1000 Times

Rolling a die one-thousand times didn’t take as long as I previously thought, only about 35 minutes. For what it’s worth, I swapped back and forth between my left and right hands to reduce the predictability my my rolls, and put a decent amount of momentum into each roll. It was actually quite meditiative. After those thirty five minutes I had my data, and here are the results of the counts:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" alt="" width="672" />
After one-thousand rolls, face 6 peeks its head out above the crowd. Is this evidence of a loaded die? Or is this within the realms of natural variability?

# A Trip to Monte Carlo

Here’s our Stan model for our die rolls. Astute readers will notice that the dirchlet/categorical are conjugates and thus there’s an analytical solution, negating the need for a Monte Carlo simulation. But I need Stan practice, so we’ll continue on.

``` stan
data {
  int<lower=1> n;
  array[n] int<lower=1, upper=6> roll;
}

parameters {
  simplex[6] theta;
}

model {
  theta ~ dirichlet( rep_vector(1, 6) );
  roll  ~ categorical(theta); 
}
```

The Stan program takes the die roll data as an array of **rolls** of length **n**. These rolls are drawn from a **categorical** distribution with with a simplex (a non-negative vector that sums to 1) parameter **theta**. The posterior distribution of **theta** we get by running our Stan sampler is the probability distribution over the six die face probabilities. This represents our uncertainty about the probability given our rolls data.

We’re using a **dirichlet** prior on theta, with all of the *alpha* values equal to one. What that says is that we expect that all combinations of probabilities have an equal probability density. Each face could have a probability of 1/6, but there’s an equal amount of probability density at 5/6, while the remaining faces have a probability of 1/30. I have the die in my hand and can see and feel that it looks pretty unbiased, so this prior is not a good choice. But let’s roll with it (hah!) for the moment.

Let’s take a look at some of the summary statistics of the theta posteriors:

``` r
dice_roll_fit$summary(
    'median', 'sd',
    variables = 'theta'
)
```

    ## # A tibble: 6 × 3
    ##   variable median     sd
    ##   <chr>     <dbl>  <dbl>
    ## 1 theta[1]  0.165 0.0116
    ## 2 theta[2]  0.166 0.0117
    ## 3 theta[3]  0.164 0.0116
    ## 4 theta[4]  0.157 0.0114
    ## 5 theta[5]  0.161 0.0114
    ## 6 theta[6]  0.185 0.0124

The median of the posterior distribution of faces 1 thru 5 all sit close to 1/6 (.166), but we see **theta\[6\]** is deviated. How much of the posterior distribution is above 1/6?

``` r
dice_roll_draws <-
dice_roll_fit |>
    spread_draws(theta[face])

dice_roll_draws |>
    filter(face == 6) |> 
    summarise(
        percent_side = mean(theta > 1/6) 
    )
```

    ## # A tibble: 1 × 2
    ##    face percent_side
    ##   <int>        <dbl>
    ## 1     6        0.937

Around 95.5% of it. Rather than single statistics, let’s look a a visualisation of the theta distriubions of each face:
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" alt="" width="672" />
All of the other faces are well inside our 90% credible interval, but face 6 sits outside of it. Surely this means that there’s only a 1:20 chance of this occurring, and we can confidently say that this dice is biased towards 6?

# A Failure on I

``` r
alpha <- 1
n <- 1000
side6_obs <- 0.9477

mass_above <- function(count) { 1 - pbeta(1/6, alpha + count, 5 * alpha + n - count) }

rmultinom(10000, n, rep(1/6, 6)) |>
    as_tibble() |>
    mutate(face = 1:n()) |>
    pivot_longer(cols = starts_with('V'), names_to = 'sim', names_prefix = 'V', values_to = 'count') |>
    mutate(q = mass_above(count)) |>
    group_by(sim) |> 
    summarise(
        q_fixed = q[face == '3'],
        q_max = max(q)
    ) |>
    summarise(
        p_fixed = mean(q_fixed >= side6_obs),
        p_any = mean(q_max >= side6_obs)
    ) |>
    gt() |>
    fmt_percent()
```

    ## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
    ## `.name_repair` is omitted as of tibble 2.0.0.
    ## ℹ Using compatibility `.name_repair`.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

<div id="dohoftwnfb" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dohoftwnfb table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#dohoftwnfb thead, #dohoftwnfb tbody, #dohoftwnfb tfoot, #dohoftwnfb tr, #dohoftwnfb td, #dohoftwnfb th {
  border-style: none;
}
&#10;#dohoftwnfb p {
  margin: 0;
  padding: 0;
}
&#10;#dohoftwnfb .gt_table {
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
&#10;#dohoftwnfb .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#dohoftwnfb .gt_title {
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
&#10;#dohoftwnfb .gt_subtitle {
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
&#10;#dohoftwnfb .gt_heading {
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
&#10;#dohoftwnfb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dohoftwnfb .gt_col_headings {
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
&#10;#dohoftwnfb .gt_col_heading {
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
&#10;#dohoftwnfb .gt_column_spanner_outer {
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
&#10;#dohoftwnfb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#dohoftwnfb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#dohoftwnfb .gt_column_spanner {
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
&#10;#dohoftwnfb .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#dohoftwnfb .gt_group_heading {
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
&#10;#dohoftwnfb .gt_empty_group_heading {
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
&#10;#dohoftwnfb .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#dohoftwnfb .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#dohoftwnfb .gt_row {
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
&#10;#dohoftwnfb .gt_stub {
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
&#10;#dohoftwnfb .gt_stub_row_group {
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
&#10;#dohoftwnfb .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#dohoftwnfb .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#dohoftwnfb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dohoftwnfb .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#dohoftwnfb .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#dohoftwnfb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dohoftwnfb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dohoftwnfb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#dohoftwnfb .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#dohoftwnfb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#dohoftwnfb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dohoftwnfb .gt_footnotes {
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
&#10;#dohoftwnfb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dohoftwnfb .gt_sourcenotes {
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
&#10;#dohoftwnfb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dohoftwnfb .gt_left {
  text-align: left;
}
&#10;#dohoftwnfb .gt_center {
  text-align: center;
}
&#10;#dohoftwnfb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#dohoftwnfb .gt_font_normal {
  font-weight: normal;
}
&#10;#dohoftwnfb .gt_font_bold {
  font-weight: bold;
}
&#10;#dohoftwnfb .gt_font_italic {
  font-style: italic;
}
&#10;#dohoftwnfb .gt_super {
  font-size: 65%;
}
&#10;#dohoftwnfb .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#dohoftwnfb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#dohoftwnfb .gt_indent_1 {
  text-indent: 5px;
}
&#10;#dohoftwnfb .gt_indent_2 {
  text-indent: 10px;
}
&#10;#dohoftwnfb .gt_indent_3 {
  text-indent: 15px;
}
&#10;#dohoftwnfb .gt_indent_4 {
  text-indent: 20px;
}
&#10;#dohoftwnfb .gt_indent_5 {
  text-indent: 25px;
}
&#10;#dohoftwnfb .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#dohoftwnfb div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p_fixed">p_fixed</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p_any">p_any</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="p_fixed" class="gt_row gt_right">4.64%</td>
<td headers="p_any" class="gt_row gt_right">27.43%</td></tr>
  </tbody>
  &#10;</table>
</div>
