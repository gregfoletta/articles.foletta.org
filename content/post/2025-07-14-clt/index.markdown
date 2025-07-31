---
title: CLT
author: Greg Foletta
date: '2025-07-14'
slug: []
categories: []
tags: []
---

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

<div id="uinsgxfdwt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#uinsgxfdwt table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#uinsgxfdwt thead, #uinsgxfdwt tbody, #uinsgxfdwt tfoot, #uinsgxfdwt tr, #uinsgxfdwt td, #uinsgxfdwt th {
  border-style: none;
}
&#10;#uinsgxfdwt p {
  margin: 0;
  padding: 0;
}
&#10;#uinsgxfdwt .gt_table {
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
&#10;#uinsgxfdwt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#uinsgxfdwt .gt_title {
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
&#10;#uinsgxfdwt .gt_subtitle {
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
&#10;#uinsgxfdwt .gt_heading {
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
&#10;#uinsgxfdwt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uinsgxfdwt .gt_col_headings {
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
&#10;#uinsgxfdwt .gt_col_heading {
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
&#10;#uinsgxfdwt .gt_column_spanner_outer {
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
&#10;#uinsgxfdwt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#uinsgxfdwt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#uinsgxfdwt .gt_column_spanner {
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
&#10;#uinsgxfdwt .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#uinsgxfdwt .gt_group_heading {
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
&#10;#uinsgxfdwt .gt_empty_group_heading {
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
&#10;#uinsgxfdwt .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#uinsgxfdwt .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#uinsgxfdwt .gt_row {
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
&#10;#uinsgxfdwt .gt_stub {
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
&#10;#uinsgxfdwt .gt_stub_row_group {
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
&#10;#uinsgxfdwt .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#uinsgxfdwt .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#uinsgxfdwt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uinsgxfdwt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#uinsgxfdwt .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#uinsgxfdwt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uinsgxfdwt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uinsgxfdwt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#uinsgxfdwt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#uinsgxfdwt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#uinsgxfdwt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uinsgxfdwt .gt_footnotes {
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
&#10;#uinsgxfdwt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uinsgxfdwt .gt_sourcenotes {
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
&#10;#uinsgxfdwt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uinsgxfdwt .gt_left {
  text-align: left;
}
&#10;#uinsgxfdwt .gt_center {
  text-align: center;
}
&#10;#uinsgxfdwt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#uinsgxfdwt .gt_font_normal {
  font-weight: normal;
}
&#10;#uinsgxfdwt .gt_font_bold {
  font-weight: bold;
}
&#10;#uinsgxfdwt .gt_font_italic {
  font-style: italic;
}
&#10;#uinsgxfdwt .gt_super {
  font-size: 65%;
}
&#10;#uinsgxfdwt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#uinsgxfdwt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#uinsgxfdwt .gt_indent_1 {
  text-indent: 5px;
}
&#10;#uinsgxfdwt .gt_indent_2 {
  text-indent: 10px;
}
&#10;#uinsgxfdwt .gt_indent_3 {
  text-indent: 15px;
}
&#10;#uinsgxfdwt .gt_indent_4 {
  text-indent: 20px;
}
&#10;#uinsgxfdwt .gt_indent_5 {
  text-indent: 25px;
}
&#10;#uinsgxfdwt .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#uinsgxfdwt div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Six Distributions - First Five Rows</td>
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
    <tr><td headers="uniform" class="gt_row gt_right">-16.124848</td>
<td headers="normal" class="gt_row gt_right">-0.33695433</td>
<td headers="binomial" class="gt_row gt_right">0</td>
<td headers="beta" class="gt_row gt_right">0.5596841</td>
<td headers="exponential" class="gt_row gt_right">3.23141455</td>
<td headers="chisquare" class="gt_row gt_right">1.4684123</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">-6.371476</td>
<td headers="normal" class="gt_row gt_right">3.06663097</td>
<td headers="binomial" class="gt_row gt_right">1</td>
<td headers="beta" class="gt_row gt_right">0.1749123</td>
<td headers="exponential" class="gt_row gt_right">0.56521981</td>
<td headers="chisquare" class="gt_row gt_right">1.7506236</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">-10.494407</td>
<td headers="normal" class="gt_row gt_right">6.18782837</td>
<td headers="binomial" class="gt_row gt_right">0</td>
<td headers="beta" class="gt_row gt_right">0.4137960</td>
<td headers="exponential" class="gt_row gt_right">1.17021534</td>
<td headers="chisquare" class="gt_row gt_right">1.5448832</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">-6.975028</td>
<td headers="normal" class="gt_row gt_right">0.35258747</td>
<td headers="binomial" class="gt_row gt_right">0</td>
<td headers="beta" class="gt_row gt_right">0.8978034</td>
<td headers="exponential" class="gt_row gt_right">1.78066949</td>
<td headers="chisquare" class="gt_row gt_right">2.1061675</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">-12.629873</td>
<td headers="normal" class="gt_row gt_right">-0.04232554</td>
<td headers="binomial" class="gt_row gt_right">1</td>
<td headers="beta" class="gt_row gt_right">0.4690491</td>
<td headers="exponential" class="gt_row gt_right">0.06609136</td>
<td headers="chisquare" class="gt_row gt_right">0.8896643</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
# Long version of random data
population_data_l <-
    population_data |>
    pivot_longer(cols = everything(), names_to = 'distribution') 
```

<div id="jshekphyqb" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jshekphyqb table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jshekphyqb thead, #jshekphyqb tbody, #jshekphyqb tfoot, #jshekphyqb tr, #jshekphyqb td, #jshekphyqb th {
  border-style: none;
}
&#10;#jshekphyqb p {
  margin: 0;
  padding: 0;
}
&#10;#jshekphyqb .gt_table {
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
&#10;#jshekphyqb .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jshekphyqb .gt_title {
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
&#10;#jshekphyqb .gt_subtitle {
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
&#10;#jshekphyqb .gt_heading {
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
&#10;#jshekphyqb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jshekphyqb .gt_col_headings {
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
&#10;#jshekphyqb .gt_col_heading {
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
&#10;#jshekphyqb .gt_column_spanner_outer {
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
&#10;#jshekphyqb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jshekphyqb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jshekphyqb .gt_column_spanner {
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
&#10;#jshekphyqb .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jshekphyqb .gt_group_heading {
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
&#10;#jshekphyqb .gt_empty_group_heading {
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
&#10;#jshekphyqb .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jshekphyqb .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jshekphyqb .gt_row {
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
&#10;#jshekphyqb .gt_stub {
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
&#10;#jshekphyqb .gt_stub_row_group {
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
&#10;#jshekphyqb .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jshekphyqb .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jshekphyqb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jshekphyqb .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jshekphyqb .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jshekphyqb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jshekphyqb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jshekphyqb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jshekphyqb .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jshekphyqb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jshekphyqb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jshekphyqb .gt_footnotes {
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
&#10;#jshekphyqb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jshekphyqb .gt_sourcenotes {
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
&#10;#jshekphyqb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jshekphyqb .gt_left {
  text-align: left;
}
&#10;#jshekphyqb .gt_center {
  text-align: center;
}
&#10;#jshekphyqb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jshekphyqb .gt_font_normal {
  font-weight: normal;
}
&#10;#jshekphyqb .gt_font_bold {
  font-weight: bold;
}
&#10;#jshekphyqb .gt_font_italic {
  font-style: italic;
}
&#10;#jshekphyqb .gt_super {
  font-size: 65%;
}
&#10;#jshekphyqb .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jshekphyqb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jshekphyqb .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jshekphyqb .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jshekphyqb .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jshekphyqb .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jshekphyqb .gt_indent_5 {
  text-indent: 25px;
}
&#10;#jshekphyqb .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#jshekphyqb div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Six Distributions - Post 'pivot_longer() - First Value</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="distribution">distribution</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="value" class="gt_row gt_right">0.5596841</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="value" class="gt_row gt_right">0.0000000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="value" class="gt_row gt_right">1.4684123</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="value" class="gt_row gt_right">3.2314146</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="value" class="gt_row gt_right">-0.3369543</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="value" class="gt_row gt_right">-16.1248483</td></tr>
  </tbody>
  &#10;  
</table>
</div>

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

``` r
# Define a function to take a random sample from our data
take_random_sample <- function(data, sample_size) {
    slice_sample(.data = data, n = sample_size) |>
    summarise(across(everything(), list(sample_mean = mean, sample_sd = sd)))
}

sample_size <- 60

# Draw 20,000 means of size 60 from our random data
random_means <- 
    map(1:20000, ~take_random_sample(population_data, sample_size = sample_size)) |> 
    list_rbind() |> 
    pivot_longer(
        everything(),
        names_to = c('distribution', '.value'),
        names_pattern = '(\\w+)_(\\w+_\\w+)'
    ) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

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

<div id="ioeoviqbap" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ioeoviqbap table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ioeoviqbap thead, #ioeoviqbap tbody, #ioeoviqbap tfoot, #ioeoviqbap tr, #ioeoviqbap td, #ioeoviqbap th {
  border-style: none;
}
&#10;#ioeoviqbap p {
  margin: 0;
  padding: 0;
}
&#10;#ioeoviqbap .gt_table {
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
&#10;#ioeoviqbap .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ioeoviqbap .gt_title {
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
&#10;#ioeoviqbap .gt_subtitle {
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
&#10;#ioeoviqbap .gt_heading {
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
&#10;#ioeoviqbap .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ioeoviqbap .gt_col_headings {
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
&#10;#ioeoviqbap .gt_col_heading {
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
&#10;#ioeoviqbap .gt_column_spanner_outer {
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
&#10;#ioeoviqbap .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ioeoviqbap .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ioeoviqbap .gt_column_spanner {
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
&#10;#ioeoviqbap .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ioeoviqbap .gt_group_heading {
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
&#10;#ioeoviqbap .gt_empty_group_heading {
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
&#10;#ioeoviqbap .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ioeoviqbap .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ioeoviqbap .gt_row {
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
&#10;#ioeoviqbap .gt_stub {
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
&#10;#ioeoviqbap .gt_stub_row_group {
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
&#10;#ioeoviqbap .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ioeoviqbap .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ioeoviqbap .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ioeoviqbap .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ioeoviqbap .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ioeoviqbap .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ioeoviqbap .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ioeoviqbap .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ioeoviqbap .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ioeoviqbap .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ioeoviqbap .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ioeoviqbap .gt_footnotes {
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
&#10;#ioeoviqbap .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ioeoviqbap .gt_sourcenotes {
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
&#10;#ioeoviqbap .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ioeoviqbap .gt_left {
  text-align: left;
}
&#10;#ioeoviqbap .gt_center {
  text-align: center;
}
&#10;#ioeoviqbap .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ioeoviqbap .gt_font_normal {
  font-weight: normal;
}
&#10;#ioeoviqbap .gt_font_bold {
  font-weight: bold;
}
&#10;#ioeoviqbap .gt_font_italic {
  font-style: italic;
}
&#10;#ioeoviqbap .gt_super {
  font-size: 65%;
}
&#10;#ioeoviqbap .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ioeoviqbap .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ioeoviqbap .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ioeoviqbap .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ioeoviqbap .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ioeoviqbap .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ioeoviqbap .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ioeoviqbap .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ioeoviqbap div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="population_mean" class="gt_row gt_right">0.63792575</td>
<td headers="population_sd" class="gt_row gt_right">0.3092906</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="population_mean" class="gt_row gt_right">0.49770000</td>
<td headers="population_sd" class="gt_row gt_right">0.5000197</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="population_mean" class="gt_row gt_right">1.99390440</td>
<td headers="population_sd" class="gt_row gt_right">2.0091079</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="population_mean" class="gt_row gt_right">2.53042602</td>
<td headers="population_sd" class="gt_row gt_right">2.5386757</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="population_mean" class="gt_row gt_right">-0.01604072</td>
<td headers="population_sd" class="gt_row gt_right">4.0658841</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="population_mean" class="gt_row gt_right">-0.16039852</td>
<td headers="population_sd" class="gt_row gt_right">11.6496352</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
# CLT Calculation
clt <-
    random_means |>
    left_join(population_data_stats, by = 'distribution') |>
    mutate(clt = (sample_mean - population_mean) / (population_sd / sqrt(n) ))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

# In Practice With Mistakes

``` r
small_sample_size <- 10

repeated_samples <-
    map(1:10000, ~take_random_sample(population_data, sample_size = small_sample_size)) |>
    list_rbind() |> 
    pivot_longer(
        everything(),
        names_to = c('distribution', '.value'),
        names_pattern = '(\\w+)_(\\w+_\\w+)'
    ) |>
    left_join(population_data_stats, by = 'distribution')
```

``` r
conf_intervals <-
    repeated_samples |> 
    mutate(
        upper_ci = sample_mean + qnorm(0.975) * sample_sd / sqrt(small_sample_size),
        lower_ci = sample_mean - qnorm(0.975) * sample_sd / sqrt(small_sample_size),
        within_ci = population_mean < upper_ci & population_mean > lower_ci
    ) |> 
    group_by(distribution) |> 
    summarise(percent_within_ci = mean(within_ci))
```

<div id="inkygfscij" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#inkygfscij table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#inkygfscij thead, #inkygfscij tbody, #inkygfscij tfoot, #inkygfscij tr, #inkygfscij td, #inkygfscij th {
  border-style: none;
}
&#10;#inkygfscij p {
  margin: 0;
  padding: 0;
}
&#10;#inkygfscij .gt_table {
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
&#10;#inkygfscij .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#inkygfscij .gt_title {
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
&#10;#inkygfscij .gt_subtitle {
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
&#10;#inkygfscij .gt_heading {
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
&#10;#inkygfscij .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#inkygfscij .gt_col_headings {
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
&#10;#inkygfscij .gt_col_heading {
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
&#10;#inkygfscij .gt_column_spanner_outer {
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
&#10;#inkygfscij .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#inkygfscij .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#inkygfscij .gt_column_spanner {
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
&#10;#inkygfscij .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#inkygfscij .gt_group_heading {
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
&#10;#inkygfscij .gt_empty_group_heading {
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
&#10;#inkygfscij .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#inkygfscij .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#inkygfscij .gt_row {
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
&#10;#inkygfscij .gt_stub {
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
&#10;#inkygfscij .gt_stub_row_group {
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
&#10;#inkygfscij .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#inkygfscij .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#inkygfscij .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#inkygfscij .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#inkygfscij .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#inkygfscij .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#inkygfscij .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#inkygfscij .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#inkygfscij .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#inkygfscij .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#inkygfscij .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#inkygfscij .gt_footnotes {
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
&#10;#inkygfscij .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#inkygfscij .gt_sourcenotes {
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
&#10;#inkygfscij .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#inkygfscij .gt_left {
  text-align: left;
}
&#10;#inkygfscij .gt_center {
  text-align: center;
}
&#10;#inkygfscij .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#inkygfscij .gt_font_normal {
  font-weight: normal;
}
&#10;#inkygfscij .gt_font_bold {
  font-weight: bold;
}
&#10;#inkygfscij .gt_font_italic {
  font-style: italic;
}
&#10;#inkygfscij .gt_super {
  font-size: 65%;
}
&#10;#inkygfscij .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#inkygfscij .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#inkygfscij .gt_indent_1 {
  text-indent: 5px;
}
&#10;#inkygfscij .gt_indent_2 {
  text-indent: 10px;
}
&#10;#inkygfscij .gt_indent_3 {
  text-indent: 15px;
}
&#10;#inkygfscij .gt_indent_4 {
  text-indent: 20px;
}
&#10;#inkygfscij .gt_indent_5 {
  text-indent: 25px;
}
&#10;#inkygfscij .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#inkygfscij div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="percent_within_ci" class="gt_row gt_right">90.96%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="percent_within_ci" class="gt_row gt_right">88.87%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="percent_within_ci" class="gt_row gt_right">87.25%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="percent_within_ci" class="gt_row gt_right">87.58%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="percent_within_ci" class="gt_row gt_right">91.76%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="percent_within_ci" class="gt_row gt_right">91.89%</td></tr>
  </tbody>
  &#10;  
</table>
</div>

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

``` r
conf_intervals |> 
    gt() |> 
    fmt_percent(percent_within_ci) |>
    cols_label(
        'distribution' = 'Distribution',
        'percent_within_ci' = '% Within CI'
    )
```

<div id="wvthulyouf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#wvthulyouf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#wvthulyouf thead, #wvthulyouf tbody, #wvthulyouf tfoot, #wvthulyouf tr, #wvthulyouf td, #wvthulyouf th {
  border-style: none;
}
&#10;#wvthulyouf p {
  margin: 0;
  padding: 0;
}
&#10;#wvthulyouf .gt_table {
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
&#10;#wvthulyouf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#wvthulyouf .gt_title {
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
&#10;#wvthulyouf .gt_subtitle {
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
&#10;#wvthulyouf .gt_heading {
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
&#10;#wvthulyouf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wvthulyouf .gt_col_headings {
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
&#10;#wvthulyouf .gt_col_heading {
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
&#10;#wvthulyouf .gt_column_spanner_outer {
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
&#10;#wvthulyouf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#wvthulyouf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#wvthulyouf .gt_column_spanner {
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
&#10;#wvthulyouf .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#wvthulyouf .gt_group_heading {
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
&#10;#wvthulyouf .gt_empty_group_heading {
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
&#10;#wvthulyouf .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#wvthulyouf .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#wvthulyouf .gt_row {
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
&#10;#wvthulyouf .gt_stub {
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
&#10;#wvthulyouf .gt_stub_row_group {
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
&#10;#wvthulyouf .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#wvthulyouf .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#wvthulyouf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wvthulyouf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#wvthulyouf .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#wvthulyouf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wvthulyouf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wvthulyouf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#wvthulyouf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#wvthulyouf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#wvthulyouf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wvthulyouf .gt_footnotes {
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
&#10;#wvthulyouf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wvthulyouf .gt_sourcenotes {
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
&#10;#wvthulyouf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wvthulyouf .gt_left {
  text-align: left;
}
&#10;#wvthulyouf .gt_center {
  text-align: center;
}
&#10;#wvthulyouf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#wvthulyouf .gt_font_normal {
  font-weight: normal;
}
&#10;#wvthulyouf .gt_font_bold {
  font-weight: bold;
}
&#10;#wvthulyouf .gt_font_italic {
  font-style: italic;
}
&#10;#wvthulyouf .gt_super {
  font-size: 65%;
}
&#10;#wvthulyouf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#wvthulyouf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#wvthulyouf .gt_indent_1 {
  text-indent: 5px;
}
&#10;#wvthulyouf .gt_indent_2 {
  text-indent: 10px;
}
&#10;#wvthulyouf .gt_indent_3 {
  text-indent: 15px;
}
&#10;#wvthulyouf .gt_indent_4 {
  text-indent: 20px;
}
&#10;#wvthulyouf .gt_indent_5 {
  text-indent: 25px;
}
&#10;#wvthulyouf .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#wvthulyouf div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="percent_within_ci" class="gt_row gt_right">93.99%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="percent_within_ci" class="gt_row gt_right">93.87%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="percent_within_ci" class="gt_row gt_right">90.53%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="percent_within_ci" class="gt_row gt_right">90.47%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.98%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.71%</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
conf_intervals <-
    random_means |> 
    left_join(population_data_stats, by = 'distribution') |>
    mutate(
        upper_ci = sample_mean + qnorm(0.975) * (sample_sd / sqrt(sample_size)),
        lower_ci = sample_mean - qnorm(0.975) * (sample_sd / sqrt(sample_size)),
        within_ci = population_mean <= upper_ci & population_mean > lower_ci
    ) |> 
    group_by(distribution) |> 
    summarise(percent_within_ci = mean(within_ci))
```

<div id="lelspdkrje" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lelspdkrje table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lelspdkrje thead, #lelspdkrje tbody, #lelspdkrje tfoot, #lelspdkrje tr, #lelspdkrje td, #lelspdkrje th {
  border-style: none;
}
&#10;#lelspdkrje p {
  margin: 0;
  padding: 0;
}
&#10;#lelspdkrje .gt_table {
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
&#10;#lelspdkrje .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lelspdkrje .gt_title {
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
&#10;#lelspdkrje .gt_subtitle {
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
&#10;#lelspdkrje .gt_heading {
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
&#10;#lelspdkrje .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lelspdkrje .gt_col_headings {
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
&#10;#lelspdkrje .gt_col_heading {
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
&#10;#lelspdkrje .gt_column_spanner_outer {
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
&#10;#lelspdkrje .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lelspdkrje .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lelspdkrje .gt_column_spanner {
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
&#10;#lelspdkrje .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lelspdkrje .gt_group_heading {
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
&#10;#lelspdkrje .gt_empty_group_heading {
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
&#10;#lelspdkrje .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lelspdkrje .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lelspdkrje .gt_row {
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
&#10;#lelspdkrje .gt_stub {
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
&#10;#lelspdkrje .gt_stub_row_group {
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
&#10;#lelspdkrje .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lelspdkrje .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lelspdkrje .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lelspdkrje .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lelspdkrje .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lelspdkrje .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lelspdkrje .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lelspdkrje .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lelspdkrje .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lelspdkrje .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lelspdkrje .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lelspdkrje .gt_footnotes {
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
&#10;#lelspdkrje .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lelspdkrje .gt_sourcenotes {
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
&#10;#lelspdkrje .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lelspdkrje .gt_left {
  text-align: left;
}
&#10;#lelspdkrje .gt_center {
  text-align: center;
}
&#10;#lelspdkrje .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lelspdkrje .gt_font_normal {
  font-weight: normal;
}
&#10;#lelspdkrje .gt_font_bold {
  font-weight: bold;
}
&#10;#lelspdkrje .gt_font_italic {
  font-style: italic;
}
&#10;#lelspdkrje .gt_super {
  font-size: 65%;
}
&#10;#lelspdkrje .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lelspdkrje .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lelspdkrje .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lelspdkrje .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lelspdkrje .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lelspdkrje .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lelspdkrje .gt_indent_5 {
  text-indent: 25px;
}
&#10;#lelspdkrje .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#lelspdkrje div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.53%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.70%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="percent_within_ci" class="gt_row gt_right">93.78%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="percent_within_ci" class="gt_row gt_right">93.36%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.56%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.45%</td></tr>
  </tbody>
  &#10;  
</table>
</div>
