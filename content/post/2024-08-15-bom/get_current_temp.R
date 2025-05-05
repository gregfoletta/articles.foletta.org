library(httr2)
library(tibble)
library(dplyr)
library(purrr)
library(readr)
library(here)
library(glue)
library(stringr)
library(tidyr)
library(rvest)









current_temp_req <- function(paths) {
    reqs <- list(rep(NA, length(paths)))
    
    for (x in 1:length(paths)) {
        reqs[[x]] <-
            request('http://www.bom.gov.au') |>
            req_url_path(paths[[x]]) |>
            req_url_query() |> 
            req_user_agent('Mozilla') |>
            req_throttle(capacity = 60, fill_time_s = 10)
    }
    
    reqs
}



extract_from_response <- function(resp) {
    request_date <-
        resp |>
        resp_date()
    
    current_temp <- 
        resp |> 
        resp_body_html() |>
        html_element('.airT') |>
        html_text() |>
        str_extract("-?\\d+(\\.\\d+)?") |>
        as.double()
    
    current_temp_time <-
        resp |> 
        resp_body_html() |>
        html_element('.summary') |>
        html_element('h3') |>
        html_text() |>
        str_extract("\\d+:\\d+(am|pm)")
    
    station_id <-
        resp |> 
        resp_body_html() |>
        html_element('.summary') |>
        html_element('.station-id') |>
        html_text() |>
        str_extract("\\d+") |>
        as.double()
    
    tibble(request_date = request_date, page_station_id = station_id, current_temp = current_temp, current_temp_time = current_temp_time)
}




get_and_write_current_temp <- function(file) {
    current_day_data <- NULL
    # Get the list of paths to locations
    location_data <-
        read_csv(file) |>
        select(site, wmo, station_id, location, path) |>
        filter(wmo == station_id)
    
    csv_dir <- here('content', 'post', '2024-08-15-bom', 'temp_data', Sys.Date()) 
    if (!file.exists(csv_dir)) {
        dir.create(csv_dir)
    }
    
    csv_out_path <- here('content','post', '2024-08-15-bom', 'temp_data', Sys.Date(), glue("{str_replace(Sys.time(), ' ', '_')}.csv")) 
    
    new_temps <- 
        location_data |>  
        mutate(
            reqs = current_temp_req(path),
            resps = req_perform_parallel(reqs)
        ) |> 
        mutate(current_temp = map(resps, ~extract_from_response(.x))) |>
        print() |> 
        unnest(current_temp) |>
        select(request_date, site, wmo, station_id, page_station_id, location, path, current_temp, current_temp_time)
   
    new_temps |>  
        write_csv(csv_out_path)
}

##### Start Point
print(here())
get_and_write_current_temp(here('content', 'post', '2024-08-15-bom', 'weather_stations_current_data.csv')) -> foo
