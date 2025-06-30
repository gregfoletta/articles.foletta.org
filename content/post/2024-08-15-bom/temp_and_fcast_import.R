library(tidyverse)
library(httr2)
library(rvest)
library(janitor)
library(glue)
library(tictoc)
library(here)
library(furrr)

plan(multisession, workers = 8)

tic()

state_tz_map <-
    tibble(
        state = c("NSW", "NT",  "QLD", "SA",  "TAS", "VIC", "WA"),
        tz = c("Australia/NSW", "Australia/North", "Australia/Queensland", "Australia/South", "Australia/Tasmania", "Australia/Victoria", "Australia/West")
    )

###################################
# Load and clean the temperature
# data
###################################
print("Loading temperature data...")
    
load_and_clean_temps <- function(csvs) {
    csvs |> 
    read_csv(show_col_types = FALSE, progress = TRUE) |>
    rename(
        request_datetime_utc = request_date,
        temp_time_local = current_temp_time,
        temperature = current_temp
    ) |> 
    # Create a 10 minute floored time
    mutate(request_datetime_utc_floored = floor_date(request_datetime_utc, '10 minutes')) |>
    separate(location, into = c('city', 'state'), sep = ', ') |> 
    # Add request and date
    left_join(state_tz_map, by = 'state') |>
    mutate(request_datetime_local_floored = map2_vec(request_datetime_utc_floored, tz, ~{ with_tz(.x, tz = .y)})) |>
    mutate(temp_datetime_local = as_datetime(as_date(request_datetime_local_floored)) + temp_time_local) |> 
    select(request_datetime_utc, request_datetime_utc_floored, request_datetime_local_floored, temp_datetime_local, everything())
}

temp_data <-
    list.files('temp_data', recursive = TRUE, pattern = '*.csv', full.names = TRUE) |>
    head(n = 300) |> 
    future_map(~load_and_clean_temps(.x), .progress = TRUE) |> 
    bind_rows() |> 
    # Make sure there are no duplicates
    distinct(wmo, city, state, temp_datetime_local, .keep_all = TRUE)

saveRDS(temp_data, file = 'bom_temperature.Rdata', compress = 'gzip')
print("done")

###################################
# Load and clean the forecast data
###################################
print("Loading forecast data...")
load_clean_forecasts <- function(csvs) {
    csvs |>
    read_csv(show_col_types = FALSE) |>
    rename(
        request_datetime_utc = request_datetime,
        forecast_datetime_local = forecast_datetime
    ) |>
    separate(location, into = c('city', 'state'), sep = ', ') |>
    # Add a column that is the request time in the local timezone
    left_join(state_tz_map, by = 'state') |>
    mutate(
        request_datetime_local = map2_vec(request_datetime_utc, tz, ~{ with_tz(.x, .y)}),
        request_datetime_local_floored = floor_date(request_datetime_local, "10 minutes")
    ) |>
    # Calculate time difference between the request and the the forecast
    mutate(forecast_period = time_length(forecast_datetime_local - request_datetime_local_floored)) |>
    # Add the actual temperasture that was recorded at the forecast time
    left_join(temp_data |> select(temp_datetime_local, wmo, temperature), by = c('wmo', 'forecast_datetime_local' = 'temp_datetime_local'), relationship = 'many-to-one')

}

forecast_data <-
    list.files('forecast_data', recursive = TRUE, pattern = '*.csv', full.names = TRUE) |>
    head(n = 30) |> 
    future_map(~load_clean_forecasts(.x), .progress = TRUE) |>
    bind_rows()

# Get the unique forecasts
forecast_data |>
    distinct(wmo, city, state, forecast_datetime_local, forecast_temp, .keep_all = TRUE) |>
    saveRDS(file = 'bom_forecasts_distinct.Rdata', compress = 'gzip')

# Get the forecasts which were requested are 1, 2, 3, ..., 7 days before the actual forecast data
forecast_data |>
    filter(forecast_period %in% c(86400*1:7)) |>
    saveRDS(file = 'bom_forecasts_day_lags.Rdata', compress = 'gzip')

print("done")

toc()
