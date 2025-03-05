---
title: 'Byte-size: An Ode to Web Scraping with R'
author: ''
date: '2025-03-03'
categories: [R Scraping]
---

Last week I needed to pull some data from a website. I was building out a data pipeline to generate the end-of-season awards for Little Athletics, but I ran into a problem (more on that later). Like dependable friend, R came to the rescue with a simple, elegant solution. This post is a ‘byte-size’ ode to this dependable friend.

# Beauty and Terseness

I’ll get to the challenge I ran into shortly, but first we’ll take a look at what basic web scraping looks like. Suppose you want to get all the headlines from The Age’s website. You look at the source and see that all the `<a>` tags have an attribute *data-testid* equal to *article-link*. Here’s the pipeline that acheives this:

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

<div id="zpyqsswiby" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zpyqsswiby table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#zpyqsswiby thead, #zpyqsswiby tbody, #zpyqsswiby tfoot, #zpyqsswiby tr, #zpyqsswiby td, #zpyqsswiby th {
  border-style: none;
}
&#10;#zpyqsswiby p {
  margin: 0;
  padding: 0;
}
&#10;#zpyqsswiby .gt_table {
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
&#10;#zpyqsswiby .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#zpyqsswiby .gt_title {
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
&#10;#zpyqsswiby .gt_subtitle {
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
&#10;#zpyqsswiby .gt_heading {
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
&#10;#zpyqsswiby .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zpyqsswiby .gt_col_headings {
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
&#10;#zpyqsswiby .gt_col_heading {
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
&#10;#zpyqsswiby .gt_column_spanner_outer {
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
&#10;#zpyqsswiby .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#zpyqsswiby .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#zpyqsswiby .gt_column_spanner {
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
&#10;#zpyqsswiby .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#zpyqsswiby .gt_group_heading {
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
&#10;#zpyqsswiby .gt_empty_group_heading {
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
&#10;#zpyqsswiby .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#zpyqsswiby .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#zpyqsswiby .gt_row {
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
&#10;#zpyqsswiby .gt_stub {
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
&#10;#zpyqsswiby .gt_stub_row_group {
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
&#10;#zpyqsswiby .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#zpyqsswiby .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#zpyqsswiby .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zpyqsswiby .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#zpyqsswiby .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#zpyqsswiby .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zpyqsswiby .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zpyqsswiby .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#zpyqsswiby .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#zpyqsswiby .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#zpyqsswiby .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zpyqsswiby .gt_footnotes {
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
&#10;#zpyqsswiby .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zpyqsswiby .gt_sourcenotes {
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
&#10;#zpyqsswiby .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zpyqsswiby .gt_left {
  text-align: left;
}
&#10;#zpyqsswiby .gt_center {
  text-align: center;
}
&#10;#zpyqsswiby .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#zpyqsswiby .gt_font_normal {
  font-weight: normal;
}
&#10;#zpyqsswiby .gt_font_bold {
  font-weight: bold;
}
&#10;#zpyqsswiby .gt_font_italic {
  font-style: italic;
}
&#10;#zpyqsswiby .gt_super {
  font-size: 65%;
}
&#10;#zpyqsswiby .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#zpyqsswiby .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#zpyqsswiby .gt_indent_1 {
  text-indent: 5px;
}
&#10;#zpyqsswiby .gt_indent_2 {
  text-indent: 10px;
}
&#10;#zpyqsswiby .gt_indent_3 {
  text-indent: 15px;
}
&#10;#zpyqsswiby .gt_indent_4 {
  text-indent: 20px;
}
&#10;#zpyqsswiby .gt_indent_5 {
  text-indent: 25px;
}
&#10;#zpyqsswiby .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#zpyqsswiby div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="headline" class="gt_row gt_left">Target Time</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">Get 2-for-1 Comedy Festival tickets*</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">The Morning Edition podcast</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">The 100 most expensive Melbourne public schools revealed</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">$4b on a new station in Melbourne’s west – has Victoria lost its budgetary mind?</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">Daily atrocities spew out of the Oval Office. It’s still not enough to lure Australian voters back to the known</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">Trump administration upends US foreign policy, holds secret talks with Hamas</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">Trump’s speech was full of wild claims. Here are seven that weren’t true</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">US lands new blow on Ukraine after Oval Office stoush</td></tr>
    <tr><td headers="headline" class="gt_row gt_left">The number one reason why people are leaving Melbourne</td></tr>
  </tbody>
  &#10;  
</table>
</div>

There we go, five lines of R and you’ve got the headlines, plus a couple more to get it into a nicer structure. What makes it so simple? I think it comes down to two things: number one is R’s pipe operator, which menas you don’t have to pepper your code with temporary variables. Second is R’s vectorisation, which means you don’t need to worry about any loops. I also tip my hat to the relatively new [httr2](https://httr2.r-lib.org/) package which makes web requests fit much better into a pipeline.

# The Challenge

The challenge I ran into last week was that, while the data I needed was structured, it wasn’t in HTML, XML, or even JSON, it was actually JavaScript. Here’s an abridged sample of what was returned in one of the API calls:

``` json
sessions_NMRKeilor = [
  {"SessNbr":"1","SessPtr":"24","SessName":"Sat Morning - Field","SessDay":"1","SessTime":"30600"},
  {"SessNbr":"2","SessPtr":"25","SessName":"Sat Morning - Track","SessDay":"1","SessTime":"32400"},
  {"SessNbr":"3","SessPtr":"46","SessName":"Sat Afternoon - Field","SessDay":"1","SessTime":"46800"},
  ...
]
```

Not sure what to do, I fetch the data:

``` r
js_content <-
    request('https://lavic.resultshub.com.au/php/resultsFileFetch.php?season=2024&series=regions&round=3&venue=undefined') |>
    req_perform() |>
    resp_body_string()
```

The mind initially goes to dark places: can I solve this with a regex? Maybe filter out the variable assignment portions and parse as JSON? Pulling myself together, I think “this is just JavaScript, is there a way I can simply evaluate it?”. Some research shows that there’s an R library that provides API access into Google’s V8 JavaScript implementation. This should allow me to evaluate the JavaScript code we received:

``` r
jscontext <- v8()
jscontext$eval(js_content)
```

From there I can get the variable I need, and the library converts this from JSON to a clean data frame:

``` r
jscontext$get('sessions_NMRKeilor') |>
    gt()
```

<div id="siumjblpqx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#siumjblpqx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#siumjblpqx thead, #siumjblpqx tbody, #siumjblpqx tfoot, #siumjblpqx tr, #siumjblpqx td, #siumjblpqx th {
  border-style: none;
}
&#10;#siumjblpqx p {
  margin: 0;
  padding: 0;
}
&#10;#siumjblpqx .gt_table {
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
&#10;#siumjblpqx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#siumjblpqx .gt_title {
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
&#10;#siumjblpqx .gt_subtitle {
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
&#10;#siumjblpqx .gt_heading {
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
&#10;#siumjblpqx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#siumjblpqx .gt_col_headings {
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
&#10;#siumjblpqx .gt_col_heading {
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
&#10;#siumjblpqx .gt_column_spanner_outer {
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
&#10;#siumjblpqx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#siumjblpqx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#siumjblpqx .gt_column_spanner {
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
&#10;#siumjblpqx .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#siumjblpqx .gt_group_heading {
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
&#10;#siumjblpqx .gt_empty_group_heading {
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
&#10;#siumjblpqx .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#siumjblpqx .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#siumjblpqx .gt_row {
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
&#10;#siumjblpqx .gt_stub {
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
&#10;#siumjblpqx .gt_stub_row_group {
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
&#10;#siumjblpqx .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#siumjblpqx .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#siumjblpqx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#siumjblpqx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#siumjblpqx .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#siumjblpqx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#siumjblpqx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#siumjblpqx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#siumjblpqx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#siumjblpqx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#siumjblpqx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#siumjblpqx .gt_footnotes {
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
&#10;#siumjblpqx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#siumjblpqx .gt_sourcenotes {
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
&#10;#siumjblpqx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#siumjblpqx .gt_left {
  text-align: left;
}
&#10;#siumjblpqx .gt_center {
  text-align: center;
}
&#10;#siumjblpqx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#siumjblpqx .gt_font_normal {
  font-weight: normal;
}
&#10;#siumjblpqx .gt_font_bold {
  font-weight: bold;
}
&#10;#siumjblpqx .gt_font_italic {
  font-style: italic;
}
&#10;#siumjblpqx .gt_super {
  font-size: 65%;
}
&#10;#siumjblpqx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#siumjblpqx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#siumjblpqx .gt_indent_1 {
  text-indent: 5px;
}
&#10;#siumjblpqx .gt_indent_2 {
  text-indent: 10px;
}
&#10;#siumjblpqx .gt_indent_3 {
  text-indent: 15px;
}
&#10;#siumjblpqx .gt_indent_4 {
  text-indent: 20px;
}
&#10;#siumjblpqx .gt_indent_5 {
  text-indent: 25px;
}
&#10;#siumjblpqx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#siumjblpqx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SessNbr">SessNbr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SessPtr">SessPtr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="SessName">SessName</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SessDay">SessDay</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SessTime">SessTime</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="SessNbr" class="gt_row gt_right">1</td>
<td headers="SessPtr" class="gt_row gt_right">24</td>
<td headers="SessName" class="gt_row gt_left">Sat Morning - Field</td>
<td headers="SessDay" class="gt_row gt_right">1</td>
<td headers="SessTime" class="gt_row gt_right">30600</td></tr>
    <tr><td headers="SessNbr" class="gt_row gt_right">2</td>
<td headers="SessPtr" class="gt_row gt_right">25</td>
<td headers="SessName" class="gt_row gt_left">Sat Morning - Track</td>
<td headers="SessDay" class="gt_row gt_right">1</td>
<td headers="SessTime" class="gt_row gt_right">32400</td></tr>
    <tr><td headers="SessNbr" class="gt_row gt_right">3</td>
<td headers="SessPtr" class="gt_row gt_right">46</td>
<td headers="SessName" class="gt_row gt_left">Sat Afternoon - Field</td>
<td headers="SessDay" class="gt_row gt_right">1</td>
<td headers="SessTime" class="gt_row gt_right">46800</td></tr>
    <tr><td headers="SessNbr" class="gt_row gt_right">4</td>
<td headers="SessPtr" class="gt_row gt_right">30</td>
<td headers="SessName" class="gt_row gt_left">Sat Afternoon - Track</td>
<td headers="SessDay" class="gt_row gt_right">1</td>
<td headers="SessTime" class="gt_row gt_right">46800</td></tr>
    <tr><td headers="SessNbr" class="gt_row gt_right">5</td>
<td headers="SessPtr" class="gt_row gt_right">43</td>
<td headers="SessName" class="gt_row gt_left">Sun Morning - Field</td>
<td headers="SessDay" class="gt_row gt_right">2</td>
<td headers="SessTime" class="gt_row gt_right">30600</td></tr>
    <tr><td headers="SessNbr" class="gt_row gt_right">6</td>
<td headers="SessPtr" class="gt_row gt_right">38</td>
<td headers="SessName" class="gt_row gt_left">Sun Morning - Track</td>
<td headers="SessDay" class="gt_row gt_right">2</td>
<td headers="SessTime" class="gt_row gt_right">30600</td></tr>
    <tr><td headers="SessNbr" class="gt_row gt_right">7</td>
<td headers="SessPtr" class="gt_row gt_right">47</td>
<td headers="SessName" class="gt_row gt_left">Sun Afternoon - Field</td>
<td headers="SessDay" class="gt_row gt_right">2</td>
<td headers="SessTime" class="gt_row gt_right">46800</td></tr>
    <tr><td headers="SessNbr" class="gt_row gt_right">8</td>
<td headers="SessPtr" class="gt_row gt_right">45</td>
<td headers="SessName" class="gt_row gt_left">Sun Afternoon - Track</td>
<td headers="SessDay" class="gt_row gt_right">2</td>
<td headers="SessTime" class="gt_row gt_right">47700</td></tr>
  </tbody>
  &#10;  
</table>
</div>

That’s it: web request to JavaScript evaluation to structured R data in a few lines; a thing of beauty.
