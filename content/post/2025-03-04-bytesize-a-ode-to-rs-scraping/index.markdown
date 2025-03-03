---
title: 'Bytesize: A Love Letter to Webscraping with R'
author: ''
date: '2025-03-3'
categories: [R Scraping]
---

I was recently doing some websscraping, building out a data pipeline to generate the end-of-season awards for Little Athletics. I ran into a problem, and as always, R comes to the rescue with a simple, elegant solution. I thought I’d write a little byte-sized ode to web-scraping with R.

# Beauty and Terseness

I’ll get to the challenge I ran into shortly, but before that let’s take a look at what standard web-scraping scenario looks like, and how easy is is to do in R. Suppose you want to get all the headlines from The Age’s website. You look at the source and see that they have an attribute *data-testid* equal to *article-link*. Here’s the pipeline that acheives this:

``` r
request('http://theage.com.au') |>
    req_perform() |>
    resp_body_html() |>
    html_elements(xpath = "//a[@data-testid='article-link']") |>
    html_text() |>
    tibble(.name_repair = ~c('headline')) |>
    filter(headline != "") |>
    slice_head(n = 10) |>
    gt()
```

<div id="rwlhntialg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rwlhntialg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#rwlhntialg thead, #rwlhntialg tbody, #rwlhntialg tfoot, #rwlhntialg tr, #rwlhntialg td, #rwlhntialg th {
  border-style: none;
}
&#10;#rwlhntialg p {
  margin: 0;
  padding: 0;
}
&#10;#rwlhntialg .gt_table {
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
&#10;#rwlhntialg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#rwlhntialg .gt_title {
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
&#10;#rwlhntialg .gt_subtitle {
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
&#10;#rwlhntialg .gt_heading {
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
&#10;#rwlhntialg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rwlhntialg .gt_col_headings {
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
&#10;#rwlhntialg .gt_col_heading {
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
&#10;#rwlhntialg .gt_column_spanner_outer {
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
&#10;#rwlhntialg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#rwlhntialg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#rwlhntialg .gt_column_spanner {
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
&#10;#rwlhntialg .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#rwlhntialg .gt_group_heading {
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
&#10;#rwlhntialg .gt_empty_group_heading {
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
&#10;#rwlhntialg .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#rwlhntialg .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#rwlhntialg .gt_row {
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
&#10;#rwlhntialg .gt_stub {
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
&#10;#rwlhntialg .gt_stub_row_group {
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
&#10;#rwlhntialg .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#rwlhntialg .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#rwlhntialg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rwlhntialg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#rwlhntialg .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#rwlhntialg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rwlhntialg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rwlhntialg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#rwlhntialg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#rwlhntialg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#rwlhntialg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rwlhntialg .gt_footnotes {
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
&#10;#rwlhntialg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rwlhntialg .gt_sourcenotes {
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
&#10;#rwlhntialg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rwlhntialg .gt_left {
  text-align: left;
}
&#10;#rwlhntialg .gt_center {
  text-align: center;
}
&#10;#rwlhntialg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#rwlhntialg .gt_font_normal {
  font-weight: normal;
}
&#10;#rwlhntialg .gt_font_bold {
  font-weight: bold;
}
&#10;#rwlhntialg .gt_font_italic {
  font-style: italic;
}
&#10;#rwlhntialg .gt_super {
  font-size: 65%;
}
&#10;#rwlhntialg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#rwlhntialg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#rwlhntialg .gt_indent_1 {
  text-indent: 5px;
}
&#10;#rwlhntialg .gt_indent_2 {
  text-indent: 10px;
}
&#10;#rwlhntialg .gt_indent_3 {
  text-indent: 15px;
}
&#10;#rwlhntialg .gt_indent_4 {
  text-indent: 20px;
}
&#10;#rwlhntialg .gt_indent_5 {
  text-indent: 25px;
}
&#10;#rwlhntialg .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#rwlhntialg div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="headline">headline</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="headline" class="gt_row gt_left">The Morning Edition podcast</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">Get 2-for-1 Comedy Festival tickets*</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">Thank God it’s Monday newsletter</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">This radical move will keep kids safer during the school run – but it’s unpopular</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">The missing links in Melbourne’s traffic nightmare – and how to fix them</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">PM slams Coalition’s ‘lazy’ work-from-home ban as Trump imitation; Election speculation mounts</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">‘It’s gonna cost me’: Home owner says he had no choice but to ‘illegally’ build seawall on beach</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">John Setka defends Mick Gatto as an ‘absolute gentleman’</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">It’s clear that Trump is an agent of Putin. All US allies should be alarmed</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">Doubling the size of schools: How Victoria can save $1.5b as population booms</td></tr>
  </tbody>
  &#10;  
</table>
</div>

There we go, five lines of R and you’ve got the headlines, plus a couple more to get it into a nicer structure. Why is it so easy? I think it comes down to two things: number one is R’s pipe operator which means you don’t have to pepper your code with temporary variables. Second is R’s vectorisation, which means you don’t need to worry about any loops.

# The Challenge

The challenge I ran into yesterday was that, while the data I needed was structured, it wasn’t in HTML, XML, or even JSON, it was actually JavaScript. Here’s an sample of what was returned in an API call:

``` json
sessions_NMRKeilor = [
  {"SessNbr":"1","SessPtr":"24","SessName":"Sat Morning - Field","SessDay":"1","SessTime":"30600"},
  {"SessNbr":"2","SessPtr":"25","SessName":"Sat Morning - Track","SessDay":"1","SessTime":"32400"},
  {"SessNbr":"3","SessPtr":"46","SessName":"Sat Afternoon - Field","SessDay":"1","SessTime":"46800"},
  ...
]
```

Not sure what to do, I go and fetch thed data:

``` r
js_content <-
    request('https://lavic.resultshub.com.au/php/resultsFileFetch.php?season=2024&series=regions&round=3&venue=undefined') |>
    req_perform() |>
    resp_body_string()
```

The mind initially goes to dark places: can I solve this with a regex? Pausing for a second, I think “this is just JavaScript, is there a way I can simply evaluate it?”. A quick seach shows that there’s an R library that provides API access into Google’s V8 JavaScript implementation, allowing us to evalusate the code we received:

``` r
jscontext <- v8()
jscontext$eval(js_content)
```

From there we can get the variable we need, and we’re done.

``` r
jscontext$get('sessions_NMRKeilor') |>
    as_tibble() |> 
    clean_names() |> 
    gt()
```

<div id="lbkmeihxao" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lbkmeihxao table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lbkmeihxao thead, #lbkmeihxao tbody, #lbkmeihxao tfoot, #lbkmeihxao tr, #lbkmeihxao td, #lbkmeihxao th {
  border-style: none;
}
&#10;#lbkmeihxao p {
  margin: 0;
  padding: 0;
}
&#10;#lbkmeihxao .gt_table {
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
&#10;#lbkmeihxao .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lbkmeihxao .gt_title {
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
&#10;#lbkmeihxao .gt_subtitle {
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
&#10;#lbkmeihxao .gt_heading {
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
&#10;#lbkmeihxao .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lbkmeihxao .gt_col_headings {
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
&#10;#lbkmeihxao .gt_col_heading {
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
&#10;#lbkmeihxao .gt_column_spanner_outer {
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
&#10;#lbkmeihxao .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lbkmeihxao .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lbkmeihxao .gt_column_spanner {
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
&#10;#lbkmeihxao .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lbkmeihxao .gt_group_heading {
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
&#10;#lbkmeihxao .gt_empty_group_heading {
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
&#10;#lbkmeihxao .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lbkmeihxao .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lbkmeihxao .gt_row {
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
&#10;#lbkmeihxao .gt_stub {
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
&#10;#lbkmeihxao .gt_stub_row_group {
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
&#10;#lbkmeihxao .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lbkmeihxao .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lbkmeihxao .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lbkmeihxao .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lbkmeihxao .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lbkmeihxao .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lbkmeihxao .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lbkmeihxao .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lbkmeihxao .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lbkmeihxao .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lbkmeihxao .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lbkmeihxao .gt_footnotes {
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
&#10;#lbkmeihxao .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lbkmeihxao .gt_sourcenotes {
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
&#10;#lbkmeihxao .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lbkmeihxao .gt_left {
  text-align: left;
}
&#10;#lbkmeihxao .gt_center {
  text-align: center;
}
&#10;#lbkmeihxao .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lbkmeihxao .gt_font_normal {
  font-weight: normal;
}
&#10;#lbkmeihxao .gt_font_bold {
  font-weight: bold;
}
&#10;#lbkmeihxao .gt_font_italic {
  font-style: italic;
}
&#10;#lbkmeihxao .gt_super {
  font-size: 65%;
}
&#10;#lbkmeihxao .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lbkmeihxao .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lbkmeihxao .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lbkmeihxao .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lbkmeihxao .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lbkmeihxao .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lbkmeihxao .gt_indent_5 {
  text-indent: 25px;
}
&#10;#lbkmeihxao .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#lbkmeihxao div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sess_nbr">sess_nbr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sess_ptr">sess_ptr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="sess_name">sess_name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sess_day">sess_day</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sess_time">sess_time</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="sess_nbr" class="gt_row gt_right">1</td>
<td headers="sess_ptr" class="gt_row gt_right">24</td>
<td headers="sess_name" class="gt_row gt_left">Sat Morning - Field</td>
<td headers="sess_day" class="gt_row gt_right">1</td>
<td headers="sess_time" class="gt_row gt_right">30600</td></tr>
    <tr><td headers="sess_nbr" class="gt_row gt_right">2</td>
<td headers="sess_ptr" class="gt_row gt_right">25</td>
<td headers="sess_name" class="gt_row gt_left">Sat Morning - Track</td>
<td headers="sess_day" class="gt_row gt_right">1</td>
<td headers="sess_time" class="gt_row gt_right">32400</td></tr>
    <tr><td headers="sess_nbr" class="gt_row gt_right">3</td>
<td headers="sess_ptr" class="gt_row gt_right">46</td>
<td headers="sess_name" class="gt_row gt_left">Sat Afternoon - Field</td>
<td headers="sess_day" class="gt_row gt_right">1</td>
<td headers="sess_time" class="gt_row gt_right">46800</td></tr>
    <tr><td headers="sess_nbr" class="gt_row gt_right">4</td>
<td headers="sess_ptr" class="gt_row gt_right">30</td>
<td headers="sess_name" class="gt_row gt_left">Sat Afternoon - Track</td>
<td headers="sess_day" class="gt_row gt_right">1</td>
<td headers="sess_time" class="gt_row gt_right">46800</td></tr>
    <tr><td headers="sess_nbr" class="gt_row gt_right">5</td>
<td headers="sess_ptr" class="gt_row gt_right">43</td>
<td headers="sess_name" class="gt_row gt_left">Sun Morning - Field</td>
<td headers="sess_day" class="gt_row gt_right">2</td>
<td headers="sess_time" class="gt_row gt_right">30600</td></tr>
    <tr><td headers="sess_nbr" class="gt_row gt_right">6</td>
<td headers="sess_ptr" class="gt_row gt_right">38</td>
<td headers="sess_name" class="gt_row gt_left">Sun Morning - Track</td>
<td headers="sess_day" class="gt_row gt_right">2</td>
<td headers="sess_time" class="gt_row gt_right">30600</td></tr>
    <tr><td headers="sess_nbr" class="gt_row gt_right">7</td>
<td headers="sess_ptr" class="gt_row gt_right">47</td>
<td headers="sess_name" class="gt_row gt_left">Sun Afternoon - Field</td>
<td headers="sess_day" class="gt_row gt_right">2</td>
<td headers="sess_time" class="gt_row gt_right">46800</td></tr>
    <tr><td headers="sess_nbr" class="gt_row gt_right">8</td>
<td headers="sess_ptr" class="gt_row gt_right">45</td>
<td headers="sess_name" class="gt_row gt_left">Sun Afternoon - Track</td>
<td headers="sess_day" class="gt_row gt_right">2</td>
<td headers="sess_time" class="gt_row gt_right">47700</td></tr>
  </tbody>
  &#10;  
</table>
</div>

Fantastic.
