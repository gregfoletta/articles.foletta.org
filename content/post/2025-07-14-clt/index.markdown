---
title: Simulating and Visualising the Central Limit Theorem
author: Greg Foletta
date: '2025-08-13'
categories: [Statistics R]
---

<<<<<<< HEAD
I completed a Computer Science degree at uni, and bundled a lot of maths subjects in as electives: partial differential equations, vector calculus, discrete maths, linear algebra. For some reason however I always avoided statistics subjects. Maybe there’s a story to be told about a young person finding uncertainty uncomfortable, because twenty years later I find statistics, particularly the Bayesian flavour, really interesting.
=======
**Update:** this post was discussed on [Hacker News](https://news.ycombinator.com/item?id=44909133)

I completed a Computer Science degree at uni, and bundled a lot of maths subjects in as electives: partial differential equations, vector calculus, discrete maths, linear algebra. For some reason however I always avoided statistics subjects. Maybe there’s a story to be told about a young person finding uncertainty uncomfortable, because twenty years later I find statistics, particularly the Bayian flavour, really interesting.
>>>>>>> 1dea3dc (HN Link)

One problem with a self-directed journey is that there’s foundational knowledge that has come to me in dribs and drabs, and one of the most foundational is the *Central Limit Theorem* (CLT). In this post I want to interrogate and explore the CLT using simulation and visualisation in an attempt to understand how it works in practice, not in theory. This is predominantly a process to help me better understand the CLT; you’re just here for the ride. Hopefully that ride can help you get where you need to go as well.

It’s been awhile since I’ve included any code in a post, so where it makes sense I’ll show the generating R code, with a liberal sprinking of comments so it’s hopefully not too inscrutable.

# A Brief Recap

I don’t want this to be like an online recipe with pages of back story before you get to the meat and bones, but a brief summary of the CLT before we begin is unavoidable. In plain English the CLT can be described as such:

> “If you take repeated samples of size *n* from a distribution and calculate the sample mean for each, as *n* gets approaches infinity, the distribution of sample means approaches a normal distribution.”

For the classic CLT there’s a couple of assumptions about the source distribution:

- The sample is drawn independently (no autocorrelation like in a time series).
- All the data points are drawn from the same distribution (“independent and identically distributed” or i.i.d).
- The distribution has a finite mean and variance (e.g. no Cauchy or Pareto distributions).

There are other versions of the CLT in which some of these assumptions are relaxed, but we’ll focus on the ‘classic’ version.

Putting it math terms:

$$
 \frac{\bar{X}_n - \mu}{\sigma/\sqrt{n}}  \overset{d}\longrightarrow \mathcal{N}(0, 1)
$$ 

# Simulating

We’re not the kind of people to just accept something because someone threw some fancy Greek symbols at us. We want to simulate it to give ourselves some confidence it works in practice.

Let’s create a tibble of ten-thousand random values from six different distributions, which we’ll call our ‘population’:

``` r
# 10,000 draws from six different distributions
population_data <- tibble(
    uniform = runif(10000, min = -20, max = 20),
    normal = rnorm(10000, mean = 0, sd = 4),
    binomial = rbinom(10000, size = 1, prob = .5),
    beta = rbeta(10000, shape1 = .9, shape2 = .5),
    exponential = rexp(10000, .4),
    chisquare = rchisq(10000, df = 2),
)
```

<div id="zkuvdcewpv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zkuvdcewpv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#zkuvdcewpv thead, #zkuvdcewpv tbody, #zkuvdcewpv tfoot, #zkuvdcewpv tr, #zkuvdcewpv td, #zkuvdcewpv th {
  border-style: none;
}
&#10;#zkuvdcewpv p {
  margin: 0;
  padding: 0;
}
&#10;#zkuvdcewpv .gt_table {
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
&#10;#zkuvdcewpv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#zkuvdcewpv .gt_title {
  color: #333333;
  font-size: 20px;
  font-weight: bolder;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#zkuvdcewpv .gt_subtitle {
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
&#10;#zkuvdcewpv .gt_heading {
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
&#10;#zkuvdcewpv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zkuvdcewpv .gt_col_headings {
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
&#10;#zkuvdcewpv .gt_col_heading {
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
&#10;#zkuvdcewpv .gt_column_spanner_outer {
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
&#10;#zkuvdcewpv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#zkuvdcewpv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#zkuvdcewpv .gt_column_spanner {
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
&#10;#zkuvdcewpv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#zkuvdcewpv .gt_group_heading {
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
&#10;#zkuvdcewpv .gt_empty_group_heading {
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
&#10;#zkuvdcewpv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#zkuvdcewpv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#zkuvdcewpv .gt_row {
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
&#10;#zkuvdcewpv .gt_stub {
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
&#10;#zkuvdcewpv .gt_stub_row_group {
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
&#10;#zkuvdcewpv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#zkuvdcewpv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#zkuvdcewpv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zkuvdcewpv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#zkuvdcewpv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#zkuvdcewpv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zkuvdcewpv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zkuvdcewpv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#zkuvdcewpv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#zkuvdcewpv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#zkuvdcewpv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zkuvdcewpv .gt_footnotes {
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
&#10;#zkuvdcewpv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zkuvdcewpv .gt_sourcenotes {
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
&#10;#zkuvdcewpv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zkuvdcewpv .gt_left {
  text-align: left;
}
&#10;#zkuvdcewpv .gt_center {
  text-align: center;
}
&#10;#zkuvdcewpv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#zkuvdcewpv .gt_font_normal {
  font-weight: normal;
}
&#10;#zkuvdcewpv .gt_font_bold {
  font-weight: bold;
}
&#10;#zkuvdcewpv .gt_font_italic {
  font-style: italic;
}
&#10;#zkuvdcewpv .gt_super {
  font-size: 65%;
}
&#10;#zkuvdcewpv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#zkuvdcewpv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#zkuvdcewpv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#zkuvdcewpv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#zkuvdcewpv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#zkuvdcewpv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#zkuvdcewpv .gt_indent_5 {
  text-indent: 25px;
}
&#10;#zkuvdcewpv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#zkuvdcewpv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Six 'Population' Distributions - Ten-Thousand Values - First Five Rows</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="uniform">uniform</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="normal">normal</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="binomial">binomial</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="beta">beta</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="exponential">exponential</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="chisquare">chisquare</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="uniform" class="gt_row gt_right">-15.243338</td>
<td headers="normal" class="gt_row gt_right">-0.7022216</td>
<td headers="binomial" class="gt_row gt_right">0</td>
<td headers="beta" class="gt_row gt_right">0.9473270</td>
<td headers="exponential" class="gt_row gt_right">2.34764246</td>
<td headers="chisquare" class="gt_row gt_right">2.8571390</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">-9.788476</td>
<td headers="normal" class="gt_row gt_right">-3.2383413</td>
<td headers="binomial" class="gt_row gt_right">1</td>
<td headers="beta" class="gt_row gt_right">0.0730441</td>
<td headers="exponential" class="gt_row gt_right">6.00295709</td>
<td headers="chisquare" class="gt_row gt_right">0.9272733</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">-11.432831</td>
<td headers="normal" class="gt_row gt_right">-2.0977844</td>
<td headers="binomial" class="gt_row gt_right">0</td>
<td headers="beta" class="gt_row gt_right">0.5160286</td>
<td headers="exponential" class="gt_row gt_right">0.91033272</td>
<td headers="chisquare" class="gt_row gt_right">3.1607328</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">18.142428</td>
<td headers="normal" class="gt_row gt_right">2.5360707</td>
<td headers="binomial" class="gt_row gt_right">1</td>
<td headers="beta" class="gt_row gt_right">0.9428468</td>
<td headers="exponential" class="gt_row gt_right">3.08170008</td>
<td headers="chisquare" class="gt_row gt_right">1.9489974</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">-7.078033</td>
<td headers="normal" class="gt_row gt_right">-1.7224620</td>
<td headers="binomial" class="gt_row gt_right">0</td>
<td headers="beta" class="gt_row gt_right">0.6134417</td>
<td headers="exponential" class="gt_row gt_right">0.08687766</td>
<td headers="chisquare" class="gt_row gt_right">2.5317882</td></tr>
  </tbody>
  &#10;  
</table>
</div>

This ‘wide’ tibble is good for sampling from, but we’ll also transform it into a long version which will have other uses (note the ’\_l’ suffix).

``` r
# Long version of random data
population_data_l <-
    population_data |>
    pivot_longer(cols = everything(), names_to = 'distribution') 
```

<div id="fuujtlarme" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#fuujtlarme table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#fuujtlarme thead, #fuujtlarme tbody, #fuujtlarme tfoot, #fuujtlarme tr, #fuujtlarme td, #fuujtlarme th {
  border-style: none;
}
&#10;#fuujtlarme p {
  margin: 0;
  padding: 0;
}
&#10;#fuujtlarme .gt_table {
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
&#10;#fuujtlarme .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#fuujtlarme .gt_title {
  color: #333333;
  font-size: 20px;
  font-weight: bolder;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#fuujtlarme .gt_subtitle {
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
&#10;#fuujtlarme .gt_heading {
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
&#10;#fuujtlarme .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fuujtlarme .gt_col_headings {
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
&#10;#fuujtlarme .gt_col_heading {
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
&#10;#fuujtlarme .gt_column_spanner_outer {
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
&#10;#fuujtlarme .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#fuujtlarme .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#fuujtlarme .gt_column_spanner {
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
&#10;#fuujtlarme .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#fuujtlarme .gt_group_heading {
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
&#10;#fuujtlarme .gt_empty_group_heading {
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
&#10;#fuujtlarme .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#fuujtlarme .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#fuujtlarme .gt_row {
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
&#10;#fuujtlarme .gt_stub {
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
&#10;#fuujtlarme .gt_stub_row_group {
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
&#10;#fuujtlarme .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#fuujtlarme .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#fuujtlarme .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fuujtlarme .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#fuujtlarme .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#fuujtlarme .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fuujtlarme .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fuujtlarme .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#fuujtlarme .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#fuujtlarme .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#fuujtlarme .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fuujtlarme .gt_footnotes {
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
&#10;#fuujtlarme .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fuujtlarme .gt_sourcenotes {
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
&#10;#fuujtlarme .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fuujtlarme .gt_left {
  text-align: left;
}
&#10;#fuujtlarme .gt_center {
  text-align: center;
}
&#10;#fuujtlarme .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#fuujtlarme .gt_font_normal {
  font-weight: normal;
}
&#10;#fuujtlarme .gt_font_bold {
  font-weight: bold;
}
&#10;#fuujtlarme .gt_font_italic {
  font-style: italic;
}
&#10;#fuujtlarme .gt_super {
  font-size: 65%;
}
&#10;#fuujtlarme .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#fuujtlarme .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#fuujtlarme .gt_indent_1 {
  text-indent: 5px;
}
&#10;#fuujtlarme .gt_indent_2 {
  text-indent: 10px;
}
&#10;#fuujtlarme .gt_indent_3 {
  text-indent: 15px;
}
&#10;#fuujtlarme .gt_indent_4 {
  text-indent: 20px;
}
&#10;#fuujtlarme .gt_indent_5 {
  text-indent: 25px;
}
&#10;#fuujtlarme .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#fuujtlarme div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Six Distributions - Post 'pivot_longer() - First Value of Each</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="distribution">distribution</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="value" class="gt_row gt_right">0.9473270</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="value" class="gt_row gt_right">0.0000000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="value" class="gt_row gt_right">2.8571390</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="value" class="gt_row gt_right">2.3476425</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="value" class="gt_row gt_right">-0.7022216</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="value" class="gt_row gt_right">-15.2433376</td></tr>
  </tbody>
  &#10;  
</table>
</div>

Here’s a histogram of each of the population distributions:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

Let’s define a function `take_random_sample_mean()` which takes a sample from all of the population distributions and calculates the mean. If we use this function repeatedly, we should end up with a data set that demonstrates the central limit theorem.

Let’s take 20,000 sample means of size 60, bind it all together into a single data frame, and shape it into a long version.

``` r
# Define a function to take a random sample from our data
take_random_sample_mean <- function(data, sample_size) {
    slice_sample(.data = data, n = sample_size) |>
    summarise(across(everything(), list(sample_mean = mean, sample_sd = sd)))
}

# Draw 20,000 means of size 60 from our random data
sample_size <- 60
sample_means <- 
    map(1:20000, ~take_random_sample_mean(population_data, sample_size = sample_size)) |> 
    # Bind the sample means into a single tibble
    list_rbind() |> 
    # Move to a long version of the data
    pivot_longer(
        everything(),
        names_to = c('distribution', '.value'),
        names_pattern = '(\\w+)_(\\w+_\\w+)'
    ) 
```

Here’s the resulting histograms of the samples with only the x-axis free to change scale. The beta and binomial look pretty normal, and the others might be as well, but the differences in variance make it difficult to tell.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

If you recall the formula at the start, to get to a standard normal we also need to subtract the population mean and divide by the population standard deviation over the square-root of n. Using the long version of the population data we can calculate the population mean and SD statistics:

``` r
population_data_stats <-
    population_data_l |>
    group_by(distribution) |>
    summarise(
        population_mean = mean(value),
        population_sd = sd(value),
        n = n()
    )
```

<div id="lpeuntixij" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lpeuntixij table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lpeuntixij thead, #lpeuntixij tbody, #lpeuntixij tfoot, #lpeuntixij tr, #lpeuntixij td, #lpeuntixij th {
  border-style: none;
}
&#10;#lpeuntixij p {
  margin: 0;
  padding: 0;
}
&#10;#lpeuntixij .gt_table {
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
&#10;#lpeuntixij .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lpeuntixij .gt_title {
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
&#10;#lpeuntixij .gt_subtitle {
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
&#10;#lpeuntixij .gt_heading {
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
&#10;#lpeuntixij .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lpeuntixij .gt_col_headings {
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
&#10;#lpeuntixij .gt_col_heading {
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
&#10;#lpeuntixij .gt_column_spanner_outer {
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
&#10;#lpeuntixij .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lpeuntixij .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lpeuntixij .gt_column_spanner {
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
&#10;#lpeuntixij .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lpeuntixij .gt_group_heading {
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
&#10;#lpeuntixij .gt_empty_group_heading {
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
&#10;#lpeuntixij .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lpeuntixij .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lpeuntixij .gt_row {
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
&#10;#lpeuntixij .gt_stub {
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
&#10;#lpeuntixij .gt_stub_row_group {
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
&#10;#lpeuntixij .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lpeuntixij .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lpeuntixij .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lpeuntixij .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lpeuntixij .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lpeuntixij .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lpeuntixij .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lpeuntixij .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lpeuntixij .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lpeuntixij .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lpeuntixij .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lpeuntixij .gt_footnotes {
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
&#10;#lpeuntixij .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lpeuntixij .gt_sourcenotes {
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
&#10;#lpeuntixij .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lpeuntixij .gt_left {
  text-align: left;
}
&#10;#lpeuntixij .gt_center {
  text-align: center;
}
&#10;#lpeuntixij .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lpeuntixij .gt_font_normal {
  font-weight: normal;
}
&#10;#lpeuntixij .gt_font_bold {
  font-weight: bold;
}
&#10;#lpeuntixij .gt_font_italic {
  font-style: italic;
}
&#10;#lpeuntixij .gt_super {
  font-size: 65%;
}
&#10;#lpeuntixij .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lpeuntixij .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lpeuntixij .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lpeuntixij .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lpeuntixij .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lpeuntixij .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lpeuntixij .gt_indent_5 {
  text-indent: 25px;
}
&#10;#lpeuntixij .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#lpeuntixij div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Random Means - Statistics</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="distribution">distribution</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="population_mean">population_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="population_sd">population_sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">n</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="population_mean" class="gt_row gt_right">0.64352237</td>
<td headers="population_sd" class="gt_row gt_right">0.3105243</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="population_mean" class="gt_row gt_right">0.50260000</td>
<td headers="population_sd" class="gt_row gt_right">0.5000182</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="population_mean" class="gt_row gt_right">2.03125146</td>
<td headers="population_sd" class="gt_row gt_right">2.0189741</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="population_mean" class="gt_row gt_right">2.48144359</td>
<td headers="population_sd" class="gt_row gt_right">2.5049145</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="population_mean" class="gt_row gt_right">-0.03205698</td>
<td headers="population_sd" class="gt_row gt_right">3.9813502</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="population_mean" class="gt_row gt_right">-0.09148261</td>
<td headers="population_sd" class="gt_row gt_right">11.6405771</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
  </tbody>
  &#10;  
</table>
</div>

Joining each of those statistics into the sample means by their distribution allows us to nicely standardise.

``` r
# CLT Calculation
clt <-
    sample_means |>
    # Join in the popultion mean, sd, and n
    left_join(population_data_stats, by = 'distribution') |>
    # Scale to the standard normal
    mutate(
        clt = (sample_mean - population_mean) / (population_sd / sqrt(sample_size) )
    )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

That’s better: they now at least all look the same, except for the binomial which ends up having higher counts because of its discrete rather than continuous values. While you can *kind of* guess that they’re normal from a histogram, we can get a better sense quantile-quantile plot. The standard normal quantiles are on the x-axis, and our sample mean quantiles are on the y-axis.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />
They all track pretty closely to a standard normal, however the chi-square and exponential do tend to diverge slightly at the ends. We’ll dive a bit deeper into that later in the post.

Let’s summarise: we took twenty-thousand sample means of size sixty from six wildly different distributions, and we were able to see that the distributions of these sample means approximately followed a normal distribution. This is the essence of the central limit theorem, which allows us to use statistical methods for normal distributions on problems that involve population distributions that aren’t normal.

# In Practice, With Mistakes

That’s all well and good if you’ve got the population mean and standard deviation, but in most cases you’re not going to have that. You’re also likely not going to have the resources to take twenty-thousand different samples.

Let’s take a a pretty classic example of interviewing six people about about something (weight, height, voting intentions, etc) and take the average: your sample mean. What the CLT can give you is an interval around your sample mean that would give you the a confidence interval (we’ll use the classic 95%) that the *true mean* of the population is somewhere in the interval.

So you use a bit of algebra and move some terms around in the CLT formula and you get this:

$$
 \bar{X} \pm z_.025 \cdot \ \frac{s}{\sqrt{n}}  
$$

where \\(\\bar{X}\\) is your sample mean, \\(z_.025\\) is the critical value (aka `qnorm()` in R) and \\(s\\) is the sample standard deviation. The sample standard deviation over the square root n is more commonly known as the standard error.

More simply, we take the sample mean and add/subtract the 97.5 quantile from a normal distribution times the standard error to get our 95% interval. Some may have noted a small mistake I’ve made here, which I’’ll leave in but will soon come to light.

We’re in frequentist territory here, so while it’s tempting to say “there’s a 95% probability that the true mean is in the interval”, we shouldn’t. Why? Because to a frequentist, the true mean \\(\\mu\\) is a fixed value. We can’t assign a probability to the population mean, it's either in the interval or it's not. What we should say is that if we were to repeat the sampling process many times, in the long run we should see the true mean within this confidence interval 95% of the time. We can simulate this to see if that is true.

We use a sample size of 6, taking the mean of these 6 samples from each of our population distributions 10,000 times.

``` r
# Ten-thousand random sample means of size six
small_sample_size <- 6 

repeated_samples <-
    map(1:10000, ~take_random_sample_mean(population_data, sample_size = small_sample_size)) |>
    # Bind the samples into a single tibble
    list_rbind() |> 
    # Wide to long tibble
    pivot_longer(
        everything(),
        names_to = c('distribution', '.value'),
        names_pattern = '(\\w+)_(\\w+_\\w+)'
    ) |>
    # Join with our population mean 
    left_join(population_data_stats, by = 'distribution')
```

For each of the samples from the distributions we calculate the 95% confidence interval. Because we know the true population mean, we can determine whether it is or is not within the interval. Then we calculate the percentage of samples for which the population mean fell within the 95% interval.

``` r
conf_intervals <-
    repeated_samples |>
    # Calculate CIs and whether CI contains the true population mean
    mutate(
        upper_ci = sample_mean + qnorm(0.975) * sample_sd / sqrt(small_sample_size),
        lower_ci = sample_mean - qnorm(0.975) * sample_sd / sqrt(small_sample_size),
        within_ci = population_mean < upper_ci & population_mean > lower_ci
    ) |> 
    # Determine percentage of CI that contain the true mean
    group_by(distribution) |> 
    summarise(percent_within_ci = mean(within_ci))
```

<div id="mzhuiwavnp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mzhuiwavnp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#mzhuiwavnp thead, #mzhuiwavnp tbody, #mzhuiwavnp tfoot, #mzhuiwavnp tr, #mzhuiwavnp td, #mzhuiwavnp th {
  border-style: none;
}
&#10;#mzhuiwavnp p {
  margin: 0;
  padding: 0;
}
&#10;#mzhuiwavnp .gt_table {
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
&#10;#mzhuiwavnp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#mzhuiwavnp .gt_title {
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
&#10;#mzhuiwavnp .gt_subtitle {
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
&#10;#mzhuiwavnp .gt_heading {
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
&#10;#mzhuiwavnp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mzhuiwavnp .gt_col_headings {
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
&#10;#mzhuiwavnp .gt_col_heading {
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
&#10;#mzhuiwavnp .gt_column_spanner_outer {
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
&#10;#mzhuiwavnp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#mzhuiwavnp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#mzhuiwavnp .gt_column_spanner {
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
&#10;#mzhuiwavnp .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#mzhuiwavnp .gt_group_heading {
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
&#10;#mzhuiwavnp .gt_empty_group_heading {
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
&#10;#mzhuiwavnp .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#mzhuiwavnp .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#mzhuiwavnp .gt_row {
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
&#10;#mzhuiwavnp .gt_stub {
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
&#10;#mzhuiwavnp .gt_stub_row_group {
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
&#10;#mzhuiwavnp .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#mzhuiwavnp .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#mzhuiwavnp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mzhuiwavnp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#mzhuiwavnp .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#mzhuiwavnp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mzhuiwavnp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mzhuiwavnp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#mzhuiwavnp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#mzhuiwavnp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#mzhuiwavnp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mzhuiwavnp .gt_footnotes {
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
&#10;#mzhuiwavnp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mzhuiwavnp .gt_sourcenotes {
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
&#10;#mzhuiwavnp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mzhuiwavnp .gt_left {
  text-align: left;
}
&#10;#mzhuiwavnp .gt_center {
  text-align: center;
}
&#10;#mzhuiwavnp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#mzhuiwavnp .gt_font_normal {
  font-weight: normal;
}
&#10;#mzhuiwavnp .gt_font_bold {
  font-weight: bold;
}
&#10;#mzhuiwavnp .gt_font_italic {
  font-style: italic;
}
&#10;#mzhuiwavnp .gt_super {
  font-size: 65%;
}
&#10;#mzhuiwavnp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#mzhuiwavnp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#mzhuiwavnp .gt_indent_1 {
  text-indent: 5px;
}
&#10;#mzhuiwavnp .gt_indent_2 {
  text-indent: 10px;
}
&#10;#mzhuiwavnp .gt_indent_3 {
  text-indent: 15px;
}
&#10;#mzhuiwavnp .gt_indent_4 {
  text-indent: 20px;
}
&#10;#mzhuiwavnp .gt_indent_5 {
  text-indent: 25px;
}
&#10;#mzhuiwavnp .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#mzhuiwavnp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="distribution">Distribution</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="percent_within_ci">% Within CI</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="percent_within_ci" class="gt_row gt_right">89.36%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="percent_within_ci" class="gt_row gt_right">88.95%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="percent_within_ci" class="gt_row gt_right">87.87%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="percent_within_ci" class="gt_row gt_right">83.32%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="percent_within_ci" class="gt_row gt_right">82.87%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="percent_within_ci" class="gt_row gt_right">77.90%</td></tr>
  </tbody>
  &#10;  
</table>
</div>

Uh oh! We’re way off our 95% here, what happened?

This is the mistake I was talking about earlier: we used a normal to calculate the CIs. We estimated the population standard deviation \\(\\sigma\\) using our sample standard deviation \\(s\\). With our small sample size of 6, our CLT formula follows a t-distribution, not a normal.

We’ll re-run our confidence intervals, but this time we use `qt()`, the t-distribution quantile function instead of `qnorm()`.

``` r
conf_intervals <-
    repeated_samples |> 
    mutate(
        upper_ci = sample_mean + qt(p = 0.975, df = small_sample_size - 1) * (sample_sd / sqrt(small_sample_size)),
        lower_ci = sample_mean - qt(p = 0.975, df = small_sample_size - 1) * (sample_sd / sqrt(small_sample_size)),
        within_ci = population_mean < upper_ci & population_mean > lower_ci
    ) |> 
    group_by(distribution) |> 
    summarise(percent_within_ci = mean(within_ci))
```

<div id="gbaweccfdi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gbaweccfdi table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#gbaweccfdi thead, #gbaweccfdi tbody, #gbaweccfdi tfoot, #gbaweccfdi tr, #gbaweccfdi td, #gbaweccfdi th {
  border-style: none;
}
&#10;#gbaweccfdi p {
  margin: 0;
  padding: 0;
}
&#10;#gbaweccfdi .gt_table {
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
&#10;#gbaweccfdi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#gbaweccfdi .gt_title {
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
&#10;#gbaweccfdi .gt_subtitle {
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
&#10;#gbaweccfdi .gt_heading {
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
&#10;#gbaweccfdi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gbaweccfdi .gt_col_headings {
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
&#10;#gbaweccfdi .gt_col_heading {
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
&#10;#gbaweccfdi .gt_column_spanner_outer {
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
&#10;#gbaweccfdi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#gbaweccfdi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#gbaweccfdi .gt_column_spanner {
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
&#10;#gbaweccfdi .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#gbaweccfdi .gt_group_heading {
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
&#10;#gbaweccfdi .gt_empty_group_heading {
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
&#10;#gbaweccfdi .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#gbaweccfdi .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#gbaweccfdi .gt_row {
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
&#10;#gbaweccfdi .gt_stub {
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
&#10;#gbaweccfdi .gt_stub_row_group {
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
&#10;#gbaweccfdi .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#gbaweccfdi .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#gbaweccfdi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gbaweccfdi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#gbaweccfdi .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#gbaweccfdi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gbaweccfdi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gbaweccfdi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#gbaweccfdi .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#gbaweccfdi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#gbaweccfdi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gbaweccfdi .gt_footnotes {
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
&#10;#gbaweccfdi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gbaweccfdi .gt_sourcenotes {
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
&#10;#gbaweccfdi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gbaweccfdi .gt_left {
  text-align: left;
}
&#10;#gbaweccfdi .gt_center {
  text-align: center;
}
&#10;#gbaweccfdi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#gbaweccfdi .gt_font_normal {
  font-weight: normal;
}
&#10;#gbaweccfdi .gt_font_bold {
  font-weight: bold;
}
&#10;#gbaweccfdi .gt_font_italic {
  font-style: italic;
}
&#10;#gbaweccfdi .gt_super {
  font-size: 65%;
}
&#10;#gbaweccfdi .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#gbaweccfdi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#gbaweccfdi .gt_indent_1 {
  text-indent: 5px;
}
&#10;#gbaweccfdi .gt_indent_2 {
  text-indent: 10px;
}
&#10;#gbaweccfdi .gt_indent_3 {
  text-indent: 15px;
}
&#10;#gbaweccfdi .gt_indent_4 {
  text-indent: 20px;
}
&#10;#gbaweccfdi .gt_indent_5 {
  text-indent: 25px;
}
&#10;#gbaweccfdi .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#gbaweccfdi div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="distribution">Distribution</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="percent_within_ci">% Within CI</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="percent_within_ci" class="gt_row gt_right">96.83%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="percent_within_ci" class="gt_row gt_right">95.05%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="percent_within_ci" class="gt_row gt_right">93.88%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="percent_within_ci" class="gt_row gt_right">92.59%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="percent_within_ci" class="gt_row gt_right">88.91%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="percent_within_ci" class="gt_row gt_right">88.47%</td></tr>
  </tbody>
  &#10;  
</table>
</div>

That looks a bit better! The binomial is higher than 95% due to its discrete nature (can’t smoothly hit all possible values), the normal and uniform distributions are close to our 95% value, but our heavily skewed beta, exponential a chi-square still aren’t up to scratch. With those heavily skewed population distributions, we need more samples before the central limit theorem ‘kicks in’.

Let’s re-run using the dataset from the start of the post, which used s sample size of 60.

``` r
conf_intervals <-
    # Using our original sample size of 60
    sample_means |> 
    left_join(population_data_stats, by = 'distribution') |>
    mutate(
        upper_ci = sample_mean + qnorm(0.975) * (sample_sd / sqrt(sample_size)),
        lower_ci = sample_mean - qnorm(0.975) * (sample_sd / sqrt(sample_size)),
        within_ci = population_mean < upper_ci & population_mean > lower_ci
    ) |> 
    group_by(distribution) |> 
    summarise(percent_within_ci = mean(within_ci))
```

<div id="aihartbnfg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#aihartbnfg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#aihartbnfg thead, #aihartbnfg tbody, #aihartbnfg tfoot, #aihartbnfg tr, #aihartbnfg td, #aihartbnfg th {
  border-style: none;
}
&#10;#aihartbnfg p {
  margin: 0;
  padding: 0;
}
&#10;#aihartbnfg .gt_table {
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
&#10;#aihartbnfg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#aihartbnfg .gt_title {
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
&#10;#aihartbnfg .gt_subtitle {
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
&#10;#aihartbnfg .gt_heading {
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
&#10;#aihartbnfg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#aihartbnfg .gt_col_headings {
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
&#10;#aihartbnfg .gt_col_heading {
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
&#10;#aihartbnfg .gt_column_spanner_outer {
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
&#10;#aihartbnfg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#aihartbnfg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#aihartbnfg .gt_column_spanner {
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
&#10;#aihartbnfg .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#aihartbnfg .gt_group_heading {
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
&#10;#aihartbnfg .gt_empty_group_heading {
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
&#10;#aihartbnfg .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#aihartbnfg .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#aihartbnfg .gt_row {
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
&#10;#aihartbnfg .gt_stub {
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
&#10;#aihartbnfg .gt_stub_row_group {
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
&#10;#aihartbnfg .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#aihartbnfg .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#aihartbnfg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aihartbnfg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#aihartbnfg .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#aihartbnfg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#aihartbnfg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aihartbnfg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#aihartbnfg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#aihartbnfg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#aihartbnfg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#aihartbnfg .gt_footnotes {
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
&#10;#aihartbnfg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aihartbnfg .gt_sourcenotes {
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
&#10;#aihartbnfg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aihartbnfg .gt_left {
  text-align: left;
}
&#10;#aihartbnfg .gt_center {
  text-align: center;
}
&#10;#aihartbnfg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#aihartbnfg .gt_font_normal {
  font-weight: normal;
}
&#10;#aihartbnfg .gt_font_bold {
  font-weight: bold;
}
&#10;#aihartbnfg .gt_font_italic {
  font-style: italic;
}
&#10;#aihartbnfg .gt_super {
  font-size: 65%;
}
&#10;#aihartbnfg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#aihartbnfg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#aihartbnfg .gt_indent_1 {
  text-indent: 5px;
}
&#10;#aihartbnfg .gt_indent_2 {
  text-indent: 10px;
}
&#10;#aihartbnfg .gt_indent_3 {
  text-indent: 15px;
}
&#10;#aihartbnfg .gt_indent_4 {
  text-indent: 20px;
}
&#10;#aihartbnfg .gt_indent_5 {
  text-indent: 25px;
}
&#10;#aihartbnfg .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#aihartbnfg div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="distribution">Distribution</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="percent_within_ci">% Within CI</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.86%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.52%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.40%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.36%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="percent_within_ci" class="gt_row gt_right">93.60%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="percent_within_ci" class="gt_row gt_right">92.97%</td></tr>
  </tbody>
  &#10;  
</table>
</div>

Better, but even with a sample size of 60, the skewed distirbutions are still slightly under the mark.

This begs the question: how do the sample means of these skewed distributions behave as the sample size increased? What we can do is repeatedly sample, but increase the sample size each time. We’ll go up in powers of two from 1 to 1024 samples.

``` r
increasing_sample_size <-
    # Sample sizes increasing in powers of 2 (1, 2, 4, 8, ...)
    map(
        2^(0:10),
        # Anonymous function
        \(y) {
            # 1000 sample means
            map(1:1000, ~take_random_sample_mean(population_data, sample_size = y)) |>
            # Bind them all together
            list_rbind() |> 
            # Wide to long per distribution
            pivot_longer(
                everything(),
                names_to = c('distribution', '.value'),
                names_pattern = '(\\w+)_(\\w+_\\w+)'
            ) |>
            # Add in our population means and SDs
            left_join(population_data_stats, by = 'distribution') |>
            # Add sample size as a column
            mutate(sample_size = y) |> 
            # Normalise to -> N(0,1)
            mutate(clt = (sample_mean - population_mean) / (population_sd / sqrt(sample_size) )) 
        }
    ) |>
    list_rbind() 
```

With this data we can create an animation of a Q-Q plot for the uniform versus exponential distributions, showing how the distribution of sample means changes as the sample size increases. You’ll see the distribution approaching the standard normal distribution.

![](index_files/figure-html/unnamed-chunk-22-1.gif)<!-- -->
You’ll see the distribution of sample means from the uniform distribution approaches a normal much faster than the expontential. It’s very subjective, but I think the uniform starts looking reasonably good at a sample size of 8. The exponential however takes much longer to converge to a normal.

# Summary

The central limit theorem is something I’ve read about many times and had a reasonable grasp on, but I was always wondering about how it behaved with different population distributions. Running simulations and visualising how it behaved in different scenarios has given me (and hopefully yourselves) a much clearer view on how it works, and probably more importantly where it doesn’t work well.
