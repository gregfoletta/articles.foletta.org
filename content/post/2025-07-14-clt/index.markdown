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

<div id="vfnotxsyjc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vfnotxsyjc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#vfnotxsyjc thead, #vfnotxsyjc tbody, #vfnotxsyjc tfoot, #vfnotxsyjc tr, #vfnotxsyjc td, #vfnotxsyjc th {
  border-style: none;
}
&#10;#vfnotxsyjc p {
  margin: 0;
  padding: 0;
}
&#10;#vfnotxsyjc .gt_table {
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
&#10;#vfnotxsyjc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#vfnotxsyjc .gt_title {
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
&#10;#vfnotxsyjc .gt_subtitle {
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
&#10;#vfnotxsyjc .gt_heading {
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
&#10;#vfnotxsyjc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vfnotxsyjc .gt_col_headings {
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
&#10;#vfnotxsyjc .gt_col_heading {
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
&#10;#vfnotxsyjc .gt_column_spanner_outer {
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
&#10;#vfnotxsyjc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#vfnotxsyjc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#vfnotxsyjc .gt_column_spanner {
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
&#10;#vfnotxsyjc .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#vfnotxsyjc .gt_group_heading {
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
&#10;#vfnotxsyjc .gt_empty_group_heading {
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
&#10;#vfnotxsyjc .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#vfnotxsyjc .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#vfnotxsyjc .gt_row {
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
&#10;#vfnotxsyjc .gt_stub {
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
&#10;#vfnotxsyjc .gt_stub_row_group {
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
&#10;#vfnotxsyjc .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#vfnotxsyjc .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#vfnotxsyjc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vfnotxsyjc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#vfnotxsyjc .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#vfnotxsyjc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vfnotxsyjc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vfnotxsyjc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#vfnotxsyjc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#vfnotxsyjc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#vfnotxsyjc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vfnotxsyjc .gt_footnotes {
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
&#10;#vfnotxsyjc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vfnotxsyjc .gt_sourcenotes {
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
&#10;#vfnotxsyjc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vfnotxsyjc .gt_left {
  text-align: left;
}
&#10;#vfnotxsyjc .gt_center {
  text-align: center;
}
&#10;#vfnotxsyjc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#vfnotxsyjc .gt_font_normal {
  font-weight: normal;
}
&#10;#vfnotxsyjc .gt_font_bold {
  font-weight: bold;
}
&#10;#vfnotxsyjc .gt_font_italic {
  font-style: italic;
}
&#10;#vfnotxsyjc .gt_super {
  font-size: 65%;
}
&#10;#vfnotxsyjc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#vfnotxsyjc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#vfnotxsyjc .gt_indent_1 {
  text-indent: 5px;
}
&#10;#vfnotxsyjc .gt_indent_2 {
  text-indent: 10px;
}
&#10;#vfnotxsyjc .gt_indent_3 {
  text-indent: 15px;
}
&#10;#vfnotxsyjc .gt_indent_4 {
  text-indent: 20px;
}
&#10;#vfnotxsyjc .gt_indent_5 {
  text-indent: 25px;
}
&#10;#vfnotxsyjc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#vfnotxsyjc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="uniform" class="gt_row gt_right">11.379704</td>
<td headers="normal" class="gt_row gt_right">6.5844727</td>
<td headers="binomial" class="gt_row gt_right">1</td>
<td headers="beta" class="gt_row gt_right">0.9424624</td>
<td headers="exponential" class="gt_row gt_right">1.8587599</td>
<td headers="chisquare" class="gt_row gt_right">0.6819254</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">1.212929</td>
<td headers="normal" class="gt_row gt_right">-2.8101019</td>
<td headers="binomial" class="gt_row gt_right">1</td>
<td headers="beta" class="gt_row gt_right">0.9544019</td>
<td headers="exponential" class="gt_row gt_right">3.5766877</td>
<td headers="chisquare" class="gt_row gt_right">0.5323494</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">9.516200</td>
<td headers="normal" class="gt_row gt_right">-2.0265289</td>
<td headers="binomial" class="gt_row gt_right">1</td>
<td headers="beta" class="gt_row gt_right">0.9716268</td>
<td headers="exponential" class="gt_row gt_right">0.2137721</td>
<td headers="chisquare" class="gt_row gt_right">1.6421727</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">-12.662431</td>
<td headers="normal" class="gt_row gt_right">-0.2426582</td>
<td headers="binomial" class="gt_row gt_right">1</td>
<td headers="beta" class="gt_row gt_right">0.8255052</td>
<td headers="exponential" class="gt_row gt_right">2.7051339</td>
<td headers="chisquare" class="gt_row gt_right">0.5261084</td></tr>
    <tr><td headers="uniform" class="gt_row gt_right">-15.951850</td>
<td headers="normal" class="gt_row gt_right">-2.6425435</td>
<td headers="binomial" class="gt_row gt_right">0</td>
<td headers="beta" class="gt_row gt_right">0.5061168</td>
<td headers="exponential" class="gt_row gt_right">1.6677345</td>
<td headers="chisquare" class="gt_row gt_right">3.5690578</td></tr>
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

<div id="zondiwtxlp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zondiwtxlp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#zondiwtxlp thead, #zondiwtxlp tbody, #zondiwtxlp tfoot, #zondiwtxlp tr, #zondiwtxlp td, #zondiwtxlp th {
  border-style: none;
}
&#10;#zondiwtxlp p {
  margin: 0;
  padding: 0;
}
&#10;#zondiwtxlp .gt_table {
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
&#10;#zondiwtxlp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#zondiwtxlp .gt_title {
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
&#10;#zondiwtxlp .gt_subtitle {
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
&#10;#zondiwtxlp .gt_heading {
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
&#10;#zondiwtxlp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zondiwtxlp .gt_col_headings {
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
&#10;#zondiwtxlp .gt_col_heading {
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
&#10;#zondiwtxlp .gt_column_spanner_outer {
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
&#10;#zondiwtxlp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#zondiwtxlp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#zondiwtxlp .gt_column_spanner {
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
&#10;#zondiwtxlp .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#zondiwtxlp .gt_group_heading {
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
&#10;#zondiwtxlp .gt_empty_group_heading {
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
&#10;#zondiwtxlp .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#zondiwtxlp .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#zondiwtxlp .gt_row {
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
&#10;#zondiwtxlp .gt_stub {
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
&#10;#zondiwtxlp .gt_stub_row_group {
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
&#10;#zondiwtxlp .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#zondiwtxlp .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#zondiwtxlp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zondiwtxlp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#zondiwtxlp .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#zondiwtxlp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zondiwtxlp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zondiwtxlp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#zondiwtxlp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#zondiwtxlp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#zondiwtxlp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zondiwtxlp .gt_footnotes {
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
&#10;#zondiwtxlp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zondiwtxlp .gt_sourcenotes {
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
&#10;#zondiwtxlp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zondiwtxlp .gt_left {
  text-align: left;
}
&#10;#zondiwtxlp .gt_center {
  text-align: center;
}
&#10;#zondiwtxlp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#zondiwtxlp .gt_font_normal {
  font-weight: normal;
}
&#10;#zondiwtxlp .gt_font_bold {
  font-weight: bold;
}
&#10;#zondiwtxlp .gt_font_italic {
  font-style: italic;
}
&#10;#zondiwtxlp .gt_super {
  font-size: 65%;
}
&#10;#zondiwtxlp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#zondiwtxlp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#zondiwtxlp .gt_indent_1 {
  text-indent: 5px;
}
&#10;#zondiwtxlp .gt_indent_2 {
  text-indent: 10px;
}
&#10;#zondiwtxlp .gt_indent_3 {
  text-indent: 15px;
}
&#10;#zondiwtxlp .gt_indent_4 {
  text-indent: 20px;
}
&#10;#zondiwtxlp .gt_indent_5 {
  text-indent: 25px;
}
&#10;#zondiwtxlp .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#zondiwtxlp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="value" class="gt_row gt_right">0.9424624</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="value" class="gt_row gt_right">1.0000000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="value" class="gt_row gt_right">0.6819254</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="value" class="gt_row gt_right">1.8587599</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="value" class="gt_row gt_right">6.5844727</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="value" class="gt_row gt_right">11.3797044</td></tr>
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

<div id="qpitkgegid" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qpitkgegid table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#qpitkgegid thead, #qpitkgegid tbody, #qpitkgegid tfoot, #qpitkgegid tr, #qpitkgegid td, #qpitkgegid th {
  border-style: none;
}
&#10;#qpitkgegid p {
  margin: 0;
  padding: 0;
}
&#10;#qpitkgegid .gt_table {
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
&#10;#qpitkgegid .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#qpitkgegid .gt_title {
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
&#10;#qpitkgegid .gt_subtitle {
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
&#10;#qpitkgegid .gt_heading {
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
&#10;#qpitkgegid .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qpitkgegid .gt_col_headings {
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
&#10;#qpitkgegid .gt_col_heading {
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
&#10;#qpitkgegid .gt_column_spanner_outer {
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
&#10;#qpitkgegid .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#qpitkgegid .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#qpitkgegid .gt_column_spanner {
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
&#10;#qpitkgegid .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#qpitkgegid .gt_group_heading {
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
&#10;#qpitkgegid .gt_empty_group_heading {
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
&#10;#qpitkgegid .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#qpitkgegid .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#qpitkgegid .gt_row {
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
&#10;#qpitkgegid .gt_stub {
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
&#10;#qpitkgegid .gt_stub_row_group {
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
&#10;#qpitkgegid .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#qpitkgegid .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#qpitkgegid .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qpitkgegid .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#qpitkgegid .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#qpitkgegid .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qpitkgegid .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qpitkgegid .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#qpitkgegid .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#qpitkgegid .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#qpitkgegid .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qpitkgegid .gt_footnotes {
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
&#10;#qpitkgegid .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qpitkgegid .gt_sourcenotes {
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
&#10;#qpitkgegid .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qpitkgegid .gt_left {
  text-align: left;
}
&#10;#qpitkgegid .gt_center {
  text-align: center;
}
&#10;#qpitkgegid .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#qpitkgegid .gt_font_normal {
  font-weight: normal;
}
&#10;#qpitkgegid .gt_font_bold {
  font-weight: bold;
}
&#10;#qpitkgegid .gt_font_italic {
  font-style: italic;
}
&#10;#qpitkgegid .gt_super {
  font-size: 65%;
}
&#10;#qpitkgegid .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#qpitkgegid .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#qpitkgegid .gt_indent_1 {
  text-indent: 5px;
}
&#10;#qpitkgegid .gt_indent_2 {
  text-indent: 10px;
}
&#10;#qpitkgegid .gt_indent_3 {
  text-indent: 15px;
}
&#10;#qpitkgegid .gt_indent_4 {
  text-indent: 20px;
}
&#10;#qpitkgegid .gt_indent_5 {
  text-indent: 25px;
}
&#10;#qpitkgegid .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#qpitkgegid div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="population_mean" class="gt_row gt_right">0.64468198</td>
<td headers="population_sd" class="gt_row gt_right">0.3068144</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="population_mean" class="gt_row gt_right">0.50700000</td>
<td headers="population_sd" class="gt_row gt_right">0.4999760</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="population_mean" class="gt_row gt_right">1.98064768</td>
<td headers="population_sd" class="gt_row gt_right">1.9963599</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="population_mean" class="gt_row gt_right">2.50463331</td>
<td headers="population_sd" class="gt_row gt_right">2.5260892</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="population_mean" class="gt_row gt_right">-0.05659686</td>
<td headers="population_sd" class="gt_row gt_right">3.9888057</td>
<td headers="n" class="gt_row gt_right">10000</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="population_mean" class="gt_row gt_right">0.09015447</td>
<td headers="population_sd" class="gt_row gt_right">11.6105963</td>
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
    mutate(clt = (sample_mean - population_mean) / (population_sd / sqrt(sample_size) ))
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

<div id="xkqkbnqbzn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xkqkbnqbzn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#xkqkbnqbzn thead, #xkqkbnqbzn tbody, #xkqkbnqbzn tfoot, #xkqkbnqbzn tr, #xkqkbnqbzn td, #xkqkbnqbzn th {
  border-style: none;
}
&#10;#xkqkbnqbzn p {
  margin: 0;
  padding: 0;
}
&#10;#xkqkbnqbzn .gt_table {
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
&#10;#xkqkbnqbzn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#xkqkbnqbzn .gt_title {
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
&#10;#xkqkbnqbzn .gt_subtitle {
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
&#10;#xkqkbnqbzn .gt_heading {
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
&#10;#xkqkbnqbzn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xkqkbnqbzn .gt_col_headings {
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
&#10;#xkqkbnqbzn .gt_col_heading {
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
&#10;#xkqkbnqbzn .gt_column_spanner_outer {
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
&#10;#xkqkbnqbzn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#xkqkbnqbzn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#xkqkbnqbzn .gt_column_spanner {
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
&#10;#xkqkbnqbzn .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#xkqkbnqbzn .gt_group_heading {
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
&#10;#xkqkbnqbzn .gt_empty_group_heading {
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
&#10;#xkqkbnqbzn .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#xkqkbnqbzn .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#xkqkbnqbzn .gt_row {
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
&#10;#xkqkbnqbzn .gt_stub {
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
&#10;#xkqkbnqbzn .gt_stub_row_group {
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
&#10;#xkqkbnqbzn .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#xkqkbnqbzn .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#xkqkbnqbzn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xkqkbnqbzn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#xkqkbnqbzn .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#xkqkbnqbzn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xkqkbnqbzn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xkqkbnqbzn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#xkqkbnqbzn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#xkqkbnqbzn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#xkqkbnqbzn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xkqkbnqbzn .gt_footnotes {
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
&#10;#xkqkbnqbzn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xkqkbnqbzn .gt_sourcenotes {
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
&#10;#xkqkbnqbzn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xkqkbnqbzn .gt_left {
  text-align: left;
}
&#10;#xkqkbnqbzn .gt_center {
  text-align: center;
}
&#10;#xkqkbnqbzn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#xkqkbnqbzn .gt_font_normal {
  font-weight: normal;
}
&#10;#xkqkbnqbzn .gt_font_bold {
  font-weight: bold;
}
&#10;#xkqkbnqbzn .gt_font_italic {
  font-style: italic;
}
&#10;#xkqkbnqbzn .gt_super {
  font-size: 65%;
}
&#10;#xkqkbnqbzn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#xkqkbnqbzn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#xkqkbnqbzn .gt_indent_1 {
  text-indent: 5px;
}
&#10;#xkqkbnqbzn .gt_indent_2 {
  text-indent: 10px;
}
&#10;#xkqkbnqbzn .gt_indent_3 {
  text-indent: 15px;
}
&#10;#xkqkbnqbzn .gt_indent_4 {
  text-indent: 20px;
}
&#10;#xkqkbnqbzn .gt_indent_5 {
  text-indent: 25px;
}
&#10;#xkqkbnqbzn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#xkqkbnqbzn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="percent_within_ci" class="gt_row gt_right">91.88%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="percent_within_ci" class="gt_row gt_right">91.62%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="percent_within_ci" class="gt_row gt_right">90.77%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="percent_within_ci" class="gt_row gt_right">88.91%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="percent_within_ci" class="gt_row gt_right">86.91%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="percent_within_ci" class="gt_row gt_right">86.63%</td></tr>
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

<div id="lvelacowji" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lvelacowji table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lvelacowji thead, #lvelacowji tbody, #lvelacowji tfoot, #lvelacowji tr, #lvelacowji td, #lvelacowji th {
  border-style: none;
}
&#10;#lvelacowji p {
  margin: 0;
  padding: 0;
}
&#10;#lvelacowji .gt_table {
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
&#10;#lvelacowji .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lvelacowji .gt_title {
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
&#10;#lvelacowji .gt_subtitle {
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
&#10;#lvelacowji .gt_heading {
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
&#10;#lvelacowji .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lvelacowji .gt_col_headings {
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
&#10;#lvelacowji .gt_col_heading {
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
&#10;#lvelacowji .gt_column_spanner_outer {
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
&#10;#lvelacowji .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lvelacowji .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lvelacowji .gt_column_spanner {
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
&#10;#lvelacowji .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lvelacowji .gt_group_heading {
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
&#10;#lvelacowji .gt_empty_group_heading {
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
&#10;#lvelacowji .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lvelacowji .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lvelacowji .gt_row {
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
&#10;#lvelacowji .gt_stub {
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
&#10;#lvelacowji .gt_stub_row_group {
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
&#10;#lvelacowji .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lvelacowji .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lvelacowji .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lvelacowji .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lvelacowji .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lvelacowji .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lvelacowji .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lvelacowji .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lvelacowji .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lvelacowji .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lvelacowji .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lvelacowji .gt_footnotes {
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
&#10;#lvelacowji .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lvelacowji .gt_sourcenotes {
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
&#10;#lvelacowji .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lvelacowji .gt_left {
  text-align: left;
}
&#10;#lvelacowji .gt_center {
  text-align: center;
}
&#10;#lvelacowji .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lvelacowji .gt_font_normal {
  font-weight: normal;
}
&#10;#lvelacowji .gt_font_bold {
  font-weight: bold;
}
&#10;#lvelacowji .gt_font_italic {
  font-style: italic;
}
&#10;#lvelacowji .gt_super {
  font-size: 65%;
}
&#10;#lvelacowji .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lvelacowji .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lvelacowji .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lvelacowji .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lvelacowji .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lvelacowji .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lvelacowji .gt_indent_5 {
  text-indent: 25px;
}
&#10;#lvelacowji .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#lvelacowji div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="percent_within_ci" class="gt_row gt_right">93.63%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="percent_within_ci" class="gt_row gt_right">93.62%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="percent_within_ci" class="gt_row gt_right">89.56%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="percent_within_ci" class="gt_row gt_right">89.83%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.66%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.69%</td></tr>
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

<div id="hbzgwnlytc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hbzgwnlytc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#hbzgwnlytc thead, #hbzgwnlytc tbody, #hbzgwnlytc tfoot, #hbzgwnlytc tr, #hbzgwnlytc td, #hbzgwnlytc th {
  border-style: none;
}
&#10;#hbzgwnlytc p {
  margin: 0;
  padding: 0;
}
&#10;#hbzgwnlytc .gt_table {
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
&#10;#hbzgwnlytc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#hbzgwnlytc .gt_title {
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
&#10;#hbzgwnlytc .gt_subtitle {
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
&#10;#hbzgwnlytc .gt_heading {
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
&#10;#hbzgwnlytc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hbzgwnlytc .gt_col_headings {
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
&#10;#hbzgwnlytc .gt_col_heading {
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
&#10;#hbzgwnlytc .gt_column_spanner_outer {
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
&#10;#hbzgwnlytc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#hbzgwnlytc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#hbzgwnlytc .gt_column_spanner {
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
&#10;#hbzgwnlytc .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#hbzgwnlytc .gt_group_heading {
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
&#10;#hbzgwnlytc .gt_empty_group_heading {
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
&#10;#hbzgwnlytc .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#hbzgwnlytc .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#hbzgwnlytc .gt_row {
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
&#10;#hbzgwnlytc .gt_stub {
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
&#10;#hbzgwnlytc .gt_stub_row_group {
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
&#10;#hbzgwnlytc .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#hbzgwnlytc .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#hbzgwnlytc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hbzgwnlytc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#hbzgwnlytc .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#hbzgwnlytc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hbzgwnlytc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hbzgwnlytc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#hbzgwnlytc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#hbzgwnlytc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#hbzgwnlytc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hbzgwnlytc .gt_footnotes {
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
&#10;#hbzgwnlytc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hbzgwnlytc .gt_sourcenotes {
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
&#10;#hbzgwnlytc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hbzgwnlytc .gt_left {
  text-align: left;
}
&#10;#hbzgwnlytc .gt_center {
  text-align: center;
}
&#10;#hbzgwnlytc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#hbzgwnlytc .gt_font_normal {
  font-weight: normal;
}
&#10;#hbzgwnlytc .gt_font_bold {
  font-weight: bold;
}
&#10;#hbzgwnlytc .gt_font_italic {
  font-style: italic;
}
&#10;#hbzgwnlytc .gt_super {
  font-size: 65%;
}
&#10;#hbzgwnlytc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#hbzgwnlytc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#hbzgwnlytc .gt_indent_1 {
  text-indent: 5px;
}
&#10;#hbzgwnlytc .gt_indent_2 {
  text-indent: 10px;
}
&#10;#hbzgwnlytc .gt_indent_3 {
  text-indent: 15px;
}
&#10;#hbzgwnlytc .gt_indent_4 {
  text-indent: 20px;
}
&#10;#hbzgwnlytc .gt_indent_5 {
  text-indent: 25px;
}
&#10;#hbzgwnlytc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#hbzgwnlytc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="distribution" class="gt_row gt_left">uniform</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.63%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">normal</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.59%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">binomial</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.54%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">beta</td>
<td headers="percent_within_ci" class="gt_row gt_right">94.36%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">chisquare</td>
<td headers="percent_within_ci" class="gt_row gt_right">93.42%</td></tr>
    <tr><td headers="distribution" class="gt_row gt_left">exponential</td>
<td headers="percent_within_ci" class="gt_row gt_right">93.08%</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
increasing_sample_size <-
    # Sample sizes of 1 to 50
    map(
        1:50,
        \(y) {
            # 1000 sample means
            map(1:1000, ~take_random_sample(population_data, sample_size = y)) |>
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
            # Add sample size
            mutate(sample_size = y) |> 
            # Normalise to -> N(0,1)
            mutate(clt = (sample_mean - population_mean) / (population_sd / sqrt(sample_size) )) 
        }
    ) |>
    list_rbind() 
```

![](index_files/figure-html/unnamed-chunk-22-1.gif)<!-- -->
