This R script automates the retrieval of meteorological data from the **MeteoSwiss (Open Government Data)** STAC API. It is designed to find the nearest weather stations to a specific geographic coordinate and download data across multiple temporal resolutions (10-minute, hourly, and daily).

## Features

- **Spatial Proximity:** Calculates the 20 nearest stations to a target latitude/longitude using the Haversine formula.
- **Multi-Resolution:** Supports downloading and processing data for:
  - **10-minute:** Radiation, Precipitation, Wind Speed.
  - **Hourly:** Temperature, Humidity.
  - **Daily:** Radiation, Precipitation, Temperature, Humidity, Wind Speed.
- **Automated Gap Filling:** If the primary (nearest) station has missing values (`NA`), the script automatically attempts to fill those gaps using data from the next closest available stations in the priority list.
- **STAC API Integration:** Dynamically fetches the latest file assets from the official Swiss Geoportal STAC v1 API.
- **Clean Output:** Saves each variable as an individual CSV file named by the variable and resolution (e.g., `precipitation_ten_minutes_total_10min.csv`).

## Prerequisites

The script requires the following R packages:
```r
install.packages(c("readr", "dplyr", "geosphere", "httr2", "purrr", "stringr", "tidyr", "lubridate"))
