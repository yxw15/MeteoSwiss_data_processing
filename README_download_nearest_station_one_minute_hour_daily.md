# MeteoSwiss RUE Data Download (STAC-based)

# 🔄 Workflow overview

## Step 1 — Fetch STAC metadata
- Retrieves all available RUE dataset files from MeteoSwiss

## Step 2 — Extract file list
- Filters only relevant `.csv` files
- Includes:
  - historical
  - recent
  - nowcast datasets

## Step 3 — Download data
- Automatically downloads all selected files via URL

## Step 4 — Merge datasets
- Combines all stations and time periods
- Standardizes structure

## Step 5 — Time processing
- Parses timestamps to UTC format
- Handles MeteoSwiss date format (`dmy_hm`)

## Step 6 — Long format conversion
- Converts wide meteorological format into tidy format:
  - station
  - datetime
  - variable
  - value

## Step 7 — Variable filtering
- Keeps only LPJ-GUESS required variables

## Step 8 — Export
- Writes one CSV per variable per resolution


MeteoSwiss_variable/
│
├── global_radiation_ten_minutes_mean_10min.csv
├── air_temperature_2_m_above_ground_hourly_mean_hourly.csv
├── relative_air_humidity_2_m_above_ground_hourly_mean_hourly.csv
├── wind_speed_scalar_hourly_mean_hourly.csv
├── global_radiation_daily_mean_daily.csv
├── precipitation_daily_total_0_utc_-_0_utc_daily.csv
└── air_temperature_2_m_above_ground_daily_mean_daily.csv

