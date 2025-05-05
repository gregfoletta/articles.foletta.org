---
title: "They're the BOM: Assessing the Bureau's Forecast Accuracy"
author: 'Greg Foletta'
date: '2025-05-04'
categories: [R Forecasting]
---

In this article I’m going to take a look at how accurate Australia’s Bureau of Metorology (BOM) is at forcasting temperature. But before I start a quick note.

I had trepidation writing this article, and it almost didn’t get off the ground. All too often people wander into fields they have no foundational knowledge in, and armed with data and start to make bold statements and inferences. I’m not a meteorologist, and simply put I didn’t want to be one of these people. But after having a chat with one of my great friends who has a Phd in meteorology, she assuaged my apprehension, and so here we are.

THe premise of this article came about after talking to people about the weather, often with them complaining that the forecasts were bad. I didn’t necessarily believe that and went looking for historical data on how the BOM has performed, but couldn’t find any. So what we’ll take a look at in this article is how well the BOM has done at forecasting the temperature from 1 to 7 days out.

# Weather Station, Forecast, and Temperature Data

In previous articles I’ve gone into detail about how I got the data I’m working wth. This is probably because it’s the most enjoyable part of the whole process. This time however I’m going to keep it brief and give you a quick overview about how I got the data, and what the data is.

I needed three pieces of data:

1.  A list of BOM weather stations
2.  The temperature at those weather stations over a period of time
3.  The one to seven day forecast at those stations

Number one was easy, as the BOM provides a [list of weather stattions](http://www.bom.gov.au/climate/data/lists_by_element/stations.txt), including their name, latitude, longitude. A shout out here to the [read_fwf()](https://readr.tidyverse.org/reference/read_fwf.html) function which makes reading in this human-readble style text table easy. This list get’s filtered down from ~6,500 total active weather stations to ~900 that have a world meteorological organisation ID. Here’s a map of all of these weather stations around Australia:

``` r
#source('temp_and_fcast_import.R', local = knitr::knit_global())
#source('temp_and_fcast_import.R')
temp_data <- readRDS('bom_temperature.Rdata') |> drop_na(temperature)
forecast_distinct <- readRDS('bom_forecasts_distinct.Rdata')
forecast_lagged <- readRDS('bom_forecasts_day_lags.Rdata')

weather_stations <- read_csv('weather_stations_current_data.csv', show_col_types = FALSE)
```

``` r
aus <- ne_countries(returnclass = "sf", scale = 'medium', country = 'Australia')
aus |>
    ggplot() +
    geom_sf() +
    geom_point(data = weather_stations, aes(x = lon, y = lat), size = .4) +
    coord_sf(xlim = c(110, 155), ylim = -c(10, 45)) +
    labs(
        x = '',
        y = ''
    )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="672" />
For the temperature and forecast data, I wrote a script which reaches out and gets the temperature and the forecasts for the clostest city/town to each of the weather stations. I then wrapped this up in a systemd service and timer and ran it every ten minutes, which appeared to be the update interval for temperature on the BOM website.

After some post-processing and cleaning, including a bit of a nightmare with timezones and an annoying daylight savings change during the sampling, the actual temperatures and the forecast temperatures are joined together. I then calculate the forecast period, which is the time between the time of the request and the time of the forecast. Here’s a sample of the first four observations of two of the sites we polled:

<div id="nbndefjlrh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#nbndefjlrh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#nbndefjlrh thead, #nbndefjlrh tbody, #nbndefjlrh tfoot, #nbndefjlrh tr, #nbndefjlrh td, #nbndefjlrh th {
  border-style: none;
}
&#10;#nbndefjlrh p {
  margin: 0;
  padding: 0;
}
&#10;#nbndefjlrh .gt_table {
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
&#10;#nbndefjlrh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#nbndefjlrh .gt_title {
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
&#10;#nbndefjlrh .gt_subtitle {
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
&#10;#nbndefjlrh .gt_heading {
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
&#10;#nbndefjlrh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#nbndefjlrh .gt_col_headings {
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
&#10;#nbndefjlrh .gt_col_heading {
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
&#10;#nbndefjlrh .gt_column_spanner_outer {
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
&#10;#nbndefjlrh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#nbndefjlrh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#nbndefjlrh .gt_column_spanner {
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
&#10;#nbndefjlrh .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#nbndefjlrh .gt_group_heading {
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
&#10;#nbndefjlrh .gt_empty_group_heading {
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
&#10;#nbndefjlrh .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#nbndefjlrh .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#nbndefjlrh .gt_row {
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
&#10;#nbndefjlrh .gt_stub {
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
&#10;#nbndefjlrh .gt_stub_row_group {
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
&#10;#nbndefjlrh .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#nbndefjlrh .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#nbndefjlrh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nbndefjlrh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#nbndefjlrh .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#nbndefjlrh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#nbndefjlrh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nbndefjlrh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#nbndefjlrh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#nbndefjlrh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#nbndefjlrh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#nbndefjlrh .gt_footnotes {
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
&#10;#nbndefjlrh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nbndefjlrh .gt_sourcenotes {
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
&#10;#nbndefjlrh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nbndefjlrh .gt_left {
  text-align: left;
}
&#10;#nbndefjlrh .gt_center {
  text-align: center;
}
&#10;#nbndefjlrh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#nbndefjlrh .gt_font_normal {
  font-weight: normal;
}
&#10;#nbndefjlrh .gt_font_bold {
  font-weight: bold;
}
&#10;#nbndefjlrh .gt_font_italic {
  font-style: italic;
}
&#10;#nbndefjlrh .gt_super {
  font-size: 65%;
}
&#10;#nbndefjlrh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#nbndefjlrh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#nbndefjlrh .gt_indent_1 {
  text-indent: 5px;
}
&#10;#nbndefjlrh .gt_indent_2 {
  text-indent: 10px;
}
&#10;#nbndefjlrh .gt_indent_3 {
  text-indent: 15px;
}
&#10;#nbndefjlrh .gt_indent_4 {
  text-indent: 20px;
}
&#10;#nbndefjlrh .gt_indent_5 {
  text-indent: 25px;
}
&#10;#nbndefjlrh .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#nbndefjlrh div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="request_datetime_local_floored">Request Date/Time</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="wmo">WMO ID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="forecast_datetime_local">Forecast Date/Time</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="forecast_period">Forecast Period</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="forecast_temp">forecast_temp</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="temperature">Recorded Temperature</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="Avalon - VIC">Avalon - VIC</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Avalon - VIC  request_datetime_local_floored" class="gt_row gt_right">2025-03-31 12:00:00</td>
<td headers="Avalon - VIC  wmo" class="gt_row gt_right">94854</td>
<td headers="Avalon - VIC  forecast_datetime_local" class="gt_row gt_right">2025-04-06 04:00:00</td>
<td headers="Avalon - VIC  forecast_period" class="gt_row gt_right">518400</td>
<td headers="Avalon - VIC  forecast_temp" class="gt_row gt_right">10</td>
<td headers="Avalon - VIC  temperature" class="gt_row gt_right">9.0</td></tr>
    <tr><td headers="Avalon - VIC  request_datetime_local_floored" class="gt_row gt_right">2025-03-31 13:00:00</td>
<td headers="Avalon - VIC  wmo" class="gt_row gt_right">94854</td>
<td headers="Avalon - VIC  forecast_datetime_local" class="gt_row gt_right">2025-04-01 05:00:00</td>
<td headers="Avalon - VIC  forecast_period" class="gt_row gt_right">86400</td>
<td headers="Avalon - VIC  forecast_temp" class="gt_row gt_right">13</td>
<td headers="Avalon - VIC  temperature" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="Avalon - VIC  request_datetime_local_floored" class="gt_row gt_right">2025-03-31 13:00:00</td>
<td headers="Avalon - VIC  wmo" class="gt_row gt_right">94854</td>
<td headers="Avalon - VIC  forecast_datetime_local" class="gt_row gt_right">2025-04-02 05:00:00</td>
<td headers="Avalon - VIC  forecast_period" class="gt_row gt_right">172800</td>
<td headers="Avalon - VIC  forecast_temp" class="gt_row gt_right">12</td>
<td headers="Avalon - VIC  temperature" class="gt_row gt_right">10.6</td></tr>
    <tr><td headers="Avalon - VIC  request_datetime_local_floored" class="gt_row gt_right">2025-03-31 13:00:00</td>
<td headers="Avalon - VIC  wmo" class="gt_row gt_right">94854</td>
<td headers="Avalon - VIC  forecast_datetime_local" class="gt_row gt_right">2025-04-03 05:00:00</td>
<td headers="Avalon - VIC  forecast_period" class="gt_row gt_right">259200</td>
<td headers="Avalon - VIC  forecast_temp" class="gt_row gt_right">12</td>
<td headers="Avalon - VIC  temperature" class="gt_row gt_right">10.9</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="St George - QLD">St George - QLD</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="St George - QLD  request_datetime_local_floored" class="gt_row gt_right">2025-03-31 12:00:00</td>
<td headers="St George - QLD  wmo" class="gt_row gt_right">94517</td>
<td headers="St George - QLD  forecast_datetime_local" class="gt_row gt_right">2025-04-01 04:00:00</td>
<td headers="St George - QLD  forecast_period" class="gt_row gt_right">86400</td>
<td headers="St George - QLD  forecast_temp" class="gt_row gt_right">18</td>
<td headers="St George - QLD  temperature" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="St George - QLD  request_datetime_local_floored" class="gt_row gt_right">2025-03-31 12:00:00</td>
<td headers="St George - QLD  wmo" class="gt_row gt_right">94517</td>
<td headers="St George - QLD  forecast_datetime_local" class="gt_row gt_right">2025-04-02 04:00:00</td>
<td headers="St George - QLD  forecast_period" class="gt_row gt_right">172800</td>
<td headers="St George - QLD  forecast_temp" class="gt_row gt_right">19</td>
<td headers="St George - QLD  temperature" class="gt_row gt_right">20.5</td></tr>
    <tr><td headers="St George - QLD  request_datetime_local_floored" class="gt_row gt_right">2025-03-31 12:00:00</td>
<td headers="St George - QLD  wmo" class="gt_row gt_right">94517</td>
<td headers="St George - QLD  forecast_datetime_local" class="gt_row gt_right">2025-04-03 04:00:00</td>
<td headers="St George - QLD  forecast_period" class="gt_row gt_right">259200</td>
<td headers="St George - QLD  forecast_temp" class="gt_row gt_right">17</td>
<td headers="St George - QLD  temperature" class="gt_row gt_right">18.5</td></tr>
    <tr><td headers="St George - QLD  request_datetime_local_floored" class="gt_row gt_right">2025-03-31 12:00:00</td>
<td headers="St George - QLD  wmo" class="gt_row gt_right">94517</td>
<td headers="St George - QLD  forecast_datetime_local" class="gt_row gt_right">2025-04-04 04:00:00</td>
<td headers="St George - QLD  forecast_period" class="gt_row gt_right">345600</td>
<td headers="St George - QLD  forecast_temp" class="gt_row gt_right">15</td>
<td headers="St George - QLD  temperature" class="gt_row gt_right">15.3</td></tr>
  </tbody>
  &#10;  
</table>
</div>

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />
That’s a total of ~1.34 million temperature readings over twenty days, which doesn’t make for the cleanest visualisation, but gives you a sense of the breadth of temperature difference in this vast country of mine. The elephant-sized gap in the middle is a period where my script stopped working, but as we’re looking at point differences between forecasts and actual temperatures, this won’t affect my results.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

# Forecasts

``` r
forecast_distinct |>
    ggplot() +
    geom_point(aes(request_datetime_local_floored, forecast_temp), size = .1) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

# Forecast Accuracy

``` r
forecast_lagged <-
    forecast_lagged |> 
    mutate(
        temperature_rounded = round(temperature),
        temp_diff = forecast_temp - temperature,
        temp_diff_rounded = forecast_temp - temperature_rounded,
        forecast_datetime_local = force_tz(forecast_datetime_local, tz = tz)
    ) |>
    mutate(location = glue("{city}, {state}")) |> 
    drop_na()
```

``` r
set.seed(32151)
forecast_lagged |>
    group_by(location) |>
    nest(.key = 'data') |>
    ungroup() |> 
    slice_sample(n = 4) |>
    unnest(data) |> 
    filter(forecast_period == days(1) * 7) |> 
    ggplot() +
    geom_point(aes(forecast_datetime_local, temperature_rounded), shape = 2, size = .4) +
    #geom_line(aes(forecast_datetime_local, temperature_rounded)) +
    geom_point(aes(forecast_datetime_local, forecast_temp, colour = location), alpha = .6, size = .4) +
    geom_line(aes(forecast_datetime_local, forecast_temp, colour = location), alpha = .6) +
    facet_wrap(~location) +
    labs(
        colour = "Forecast Temp"
    )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

``` r
forecast_lagged |>
    group_by(forecast_period) |>
    summarise(
        mean_forecast_error = mean(temp_diff),
        sd_forecast_error_round = sd(temp_diff)
    ) |>
    mutate(forecast_period = as.duration(forecast_period)) |> 
    gt() |>
    cols_label(
        forecast_period = "Forecast Period",
        mean_forecast_error = "Forecast Error Mean",
        sd_forecast_error_round = "Forecast Error Standard Deviation"
    )
```

<div id="hjmitruspz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hjmitruspz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#hjmitruspz thead, #hjmitruspz tbody, #hjmitruspz tfoot, #hjmitruspz tr, #hjmitruspz td, #hjmitruspz th {
  border-style: none;
}
&#10;#hjmitruspz p {
  margin: 0;
  padding: 0;
}
&#10;#hjmitruspz .gt_table {
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
&#10;#hjmitruspz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#hjmitruspz .gt_title {
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
&#10;#hjmitruspz .gt_subtitle {
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
&#10;#hjmitruspz .gt_heading {
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
&#10;#hjmitruspz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hjmitruspz .gt_col_headings {
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
&#10;#hjmitruspz .gt_col_heading {
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
&#10;#hjmitruspz .gt_column_spanner_outer {
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
&#10;#hjmitruspz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#hjmitruspz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#hjmitruspz .gt_column_spanner {
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
&#10;#hjmitruspz .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#hjmitruspz .gt_group_heading {
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
&#10;#hjmitruspz .gt_empty_group_heading {
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
&#10;#hjmitruspz .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#hjmitruspz .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#hjmitruspz .gt_row {
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
&#10;#hjmitruspz .gt_stub {
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
&#10;#hjmitruspz .gt_stub_row_group {
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
&#10;#hjmitruspz .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#hjmitruspz .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#hjmitruspz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hjmitruspz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#hjmitruspz .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#hjmitruspz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hjmitruspz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hjmitruspz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#hjmitruspz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#hjmitruspz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#hjmitruspz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hjmitruspz .gt_footnotes {
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
&#10;#hjmitruspz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hjmitruspz .gt_sourcenotes {
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
&#10;#hjmitruspz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hjmitruspz .gt_left {
  text-align: left;
}
&#10;#hjmitruspz .gt_center {
  text-align: center;
}
&#10;#hjmitruspz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#hjmitruspz .gt_font_normal {
  font-weight: normal;
}
&#10;#hjmitruspz .gt_font_bold {
  font-weight: bold;
}
&#10;#hjmitruspz .gt_font_italic {
  font-style: italic;
}
&#10;#hjmitruspz .gt_super {
  font-size: 65%;
}
&#10;#hjmitruspz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#hjmitruspz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#hjmitruspz .gt_indent_1 {
  text-indent: 5px;
}
&#10;#hjmitruspz .gt_indent_2 {
  text-indent: 10px;
}
&#10;#hjmitruspz .gt_indent_3 {
  text-indent: 15px;
}
&#10;#hjmitruspz .gt_indent_4 {
  text-indent: 20px;
}
&#10;#hjmitruspz .gt_indent_5 {
  text-indent: 25px;
}
&#10;#hjmitruspz .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#hjmitruspz div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="forecast_period">Forecast Period</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean_forecast_error">Forecast Error Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sd_forecast_error_round">Forecast Error Standard Deviation</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="forecast_period" class="gt_row gt_center">86400s (~1 days)</td>
<td headers="mean_forecast_error" class="gt_row gt_right">0.22832047</td>
<td headers="sd_forecast_error_round" class="gt_row gt_right">1.743535</td></tr>
    <tr><td headers="forecast_period" class="gt_row gt_center">172800s (~2 days)</td>
<td headers="mean_forecast_error" class="gt_row gt_right">0.22254116</td>
<td headers="sd_forecast_error_round" class="gt_row gt_right">1.812378</td></tr>
    <tr><td headers="forecast_period" class="gt_row gt_center">259200s (~3 days)</td>
<td headers="mean_forecast_error" class="gt_row gt_right">0.20565877</td>
<td headers="sd_forecast_error_round" class="gt_row gt_right">1.907875</td></tr>
    <tr><td headers="forecast_period" class="gt_row gt_center">345600s (~4 days)</td>
<td headers="mean_forecast_error" class="gt_row gt_right">0.20659782</td>
<td headers="sd_forecast_error_round" class="gt_row gt_right">2.018509</td></tr>
    <tr><td headers="forecast_period" class="gt_row gt_center">432000s (~5 days)</td>
<td headers="mean_forecast_error" class="gt_row gt_right">0.04902166</td>
<td headers="sd_forecast_error_round" class="gt_row gt_right">2.132341</td></tr>
    <tr><td headers="forecast_period" class="gt_row gt_center">518400s (~6 days)</td>
<td headers="mean_forecast_error" class="gt_row gt_right">-0.06330583</td>
<td headers="sd_forecast_error_round" class="gt_row gt_right">2.236941</td></tr>
    <tr><td headers="forecast_period" class="gt_row gt_center">604800s (~1 weeks)</td>
<td headers="mean_forecast_error" class="gt_row gt_right">0.48686348</td>
<td headers="sd_forecast_error_round" class="gt_row gt_right">2.238612</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
forecast_lagged |>
    ggplot() +
    geom_histogram(aes(temp_diff, after_stat(density), fill = as_factor(forecast_period)), binwidth = .2) +
    facet_wrap(vars(forecast_period)) +
    labs(
        fill = "Forecast Period\n(seconds)"
    )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

``` r
forecast_lagged |>
    ggplot(aes(sample = temp_diff, group = forecast_period)) +
    geom_qq(size = .1, distribution = stats::qnorm) +
    geom_qq_line(distribution = stats::qnorm) +
    facet_wrap(vars(forecast_period))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

``` r
wmo_sd <-
    forecast_lagged |>
    group_by(wmo, forecast_period) |>
    summarise(forecast_accuracy_sd = sd(temp_diff), .groups = 'drop') |>
    left_join(weather_stations, by = 'wmo')
    

aus |> 
 ggplot() +
    # geom_sf() +
    geom_point(data = wmo_sd |> filter(forecast_period == 518400), aes(x = lon, y = lat, colour = forecast_accuracy_sd), size = .4) +
    labs(
        x = '',
        y = ''
    )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

``` r
wmo_sd |>
    filter(forecast_period == days(7)) |> 
    ggplot() +
    geom_point(aes(lat, forecast_accuracy_sd, colour = state))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

``` r
temp_data |>
    group_by(wmo) |>
    summarise(temp_var = var(temperature, na.rm = TRUE)) |>
    left_join(wmo_sd, by = 'wmo') |>  
    filter(forecast_period == 86400*6) |> 
    ggplot() +
    geom_point(aes(temp_var, forecast_accuracy_sd, colour = state))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />
\# Best and Worse

``` r
wmo_sd |>
    filter(forecast_period == days(7)) |>
    slice_max(forecast_accuracy_sd, n = 4) |>
    left_join(forecast_lagged, by = c('wmo', 'forecast_period')) |> 
    ggplot() +
    geom_point(aes(forecast_datetime_local, temperature_rounded), shape = 2, size = .4) +
    geom_line(aes(forecast_datetime_local, temperature_rounded), alpha = .3) +
    geom_point(aes(forecast_datetime_local, forecast_temp, colour = location.x), alpha = .6, size = .4) +
    geom_line(aes(forecast_datetime_local, forecast_temp, colour = location.x), alpha = .6) +
    facet_wrap(~location.x) +
    labs(
        colour = "Forecast Temp"
    )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />
