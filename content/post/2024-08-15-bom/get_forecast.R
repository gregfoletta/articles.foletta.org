
library(httr2)
library(tibble)
library(dplyr)
library(purrr)
library(readr)
library(rvest)
library(tidyr)
library(lubridate)
library(here)
library(stringr)
library(glue)
library(tictoc)



get_and_write_forecasts <- function(file) {
    current_day_data <- NULL
    # Get the list of paths to locations
    location_data <-
        read_csv(file) |>
        select(site, wmo, station_id, location, path) |>
        filter(wmo == station_id)
   
    csv_dir <- here('content', 'post', '2024-08-15-bom', 'forecast_data', Sys.Date()) 
    if (!file.exists(csv_dir)) {
        dir.create(csv_dir)
    }
       
    csv_out_path <- here('content','post', '2024-08-15-bom', 'forecast_data', Sys.Date(), glue("{str_replace(Sys.time(), ' ', '_')}.csv")) 
    
    new_forecasts <- 
        location_data |>  
        mutate(
            reqs = forecast_req(path),
            resps = req_perform_parallel(reqs)
        ) |> 
            mutate(forecast = map(resps, ~extract_from_response(.x))) |>
            unnest(forecast) 
            #select(request_datetime, paths, forecast_datetime, forecast_temp)
   
    new_forecasts |>  
        select(site, wmo, station_id, location, request_datetime, forecast_datetime, forecast_temp) |> 
        write_csv(csv_out_path)
}


forecast_req <- function(paths) {
    reqs <- list(rep(NA, length(paths)))
    
    
    for (x in 1:length(paths)) {
        reqs[[x]] <-
        request('http://www.bom.gov.au') |>
        req_url_path(paths[[x]]) |>
        req_url_path_append('forecast/detailed') |> 
        req_url_query() |> 
        req_user_agent('Mozilla') |>
        req_throttle(capacity = 60, fill_time_s = 10)
    }
    
    reqs
}

extract_from_response <- function(resp) {
    # Get the date
    dates <-
        resp |> 
        resp_body_html() |> 
        html_elements('.forecast-day') |>
        html_attr('id')
    
    forecast <- 
        resp |> 
        resp_body_html() |>
        html_elements('.forecast-day') |>
        html_table() |>
        bind_rows() |>
        filter(From == 'Air temperature (°C)') |>  
        mutate(
            request_datetime = resp_date(resp),
            forecast_date = str_extract(dates, '\\d+-\\d+-\\d+')) |> 
        pivot_longer(cols = ends_with('AM') | ends_with("PM"), names_to = 'time', values_to = 'forecast_temp') |>
        filter(forecast_temp != '–') |> 
        mutate(forecast_temp = as.double(forecast_temp)) |> 
        mutate(forecast_datetime = ymd_hm(paste(forecast_date, time))) 
    
    forecast
}


#### Start Point ###
tic()
get_and_write_forecasts(here('content', 'post', '2024-08-15-bom', 'weather_stations_current_data.csv'))
toc()
