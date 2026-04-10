# MeteoSwiss Station Data Processing

## Overview
This script downloads meteorological data from MeteoSwiss stations, processes daily measurements,
and fills missing values using data from nearby backup stations.

## Target Location
- **Latitude**: 47.439
- **Longitude**: 7.776
- **Altitude**: 500 m

## Date Range Processed
- **Start Date**: 1991-01-01
- **End Date**: 2025-12-31

## Primary Station
- **Station Abbreviation**: RUE
- **Station Name**: Rünenberg
- **Distance from Target**: 7.8 km
- **Altitude Difference**: 111 m

## Backup Stations Used
# A tibble: 9 × 3
  Station Name              Distance_km
  <chr>   <chr>                   <dbl>
1 STC     St. Chrischona           16.2
2 MOE     Möhlin                   16.7
3 GOE     Gösgen                   17.1
4 BAS     Basel / Binningen        18.4
5 WYN     Wynau                    20.5
6 BUS     Buchs / Aarau            23.7
7 DEM     Delémont                 33.6
8 EGO     Egolzwil                 33.7
9 PSI     Würenlingen / PSI        35.6

## Parameters Processed
- Global radiation; daily mean
- Precipitation; daily total 0 UTC - 0 UTC
- Air temperature 2 m above ground; daily mean
- Air temperature 2 m above ground; daily maximum
- Air temperature 2 m above ground; daily minimum
- Relative air humidity 2 m above ground; daily mean
- Wind speed scalar; daily mean in m/s

## Data Processing Steps
1. **Metadata Download**: Reads station metadata, parameter definitions, and inventory
2. **Station Selection**: Finds 10 nearest stations based on geographic coordinates
3. **Data Download**: Fetches daily data from STAC API (recent and historical)
4. **Data Processing**: 
   - Converts temperature from °C to Kelvin (+273.15)
   - Converts wind speed from km/h to m/s (÷3.6)
   - Standardizes date formats
   - Filters to specified date range
5. **Missing Value Filling**: 
   - Uses backup stations in order of proximity
   - Fills missing values on a per-variable, per-day basis
   - Tracks which backup station filled each value
6. **Output Generation**: Creates multiple CSV files with results

## Output Files

### Main Data Files
- **`daily_filled_19910101_to_20251231.csv`**: Complete filled daily data for the primary station
  - Columns: station_abbr, date, date_day, all meteorological parameters, filling metadata
  - Temperature in Kelvin, Precipitation in mm, Radiation in W/m², Humidity in %, Wind in m/s

### Filling Reports
- **`fill_source_report.csv`**: Detailed record of each filled value
  - Shows which backup station provided each value
  - Includes station name and distance
  - Tracks date, variable, and source station

- **`fill_source_summary_by_station.csv`**: Summary statistics by backup station
  - Count of times each backup station was used
  - Date range of contributions from each station
  - Breakdown by variable type

- **`filling_log.csv`**: Days where filling occurred
  - Shows number of variables filled per day
  - Includes the filled values

- **`still_missing_after_filling.csv`**: Days with remaining missing data
  - Identifies gaps that couldn't be filled by any backup station

### Summary Reports
- **`filling_summary.csv`**: Overall statistics
  - Primary station information
  - Original vs final NA counts
  - Fill percentage achieved

## Data Quality Notes
- **Temperature**: Converted to Kelvin (K) for thermodynamic consistency
- **Wind Speed**: Converted to meters per second (m/s)
- **Precipitation**: Daily totals in millimeters (mm)
- **Radiation**: Daily mean in Watts per square meter (W/m²)
- **Humidity**: Daily mean relative humidity (%)

## Filling Statistics
- **Original Missing Values**: 9
- **Values Filled**: 9
- **Remaining Missing**: 0
- **Fill Percentage**: 100%

## Usage Instructions

### Running the Script
```r
source("meteoswiss_data_processing.R")
```

### Modifying Parameters
Edit the user settings section at the top of the script:
```r
target_lat <- 47.439          # Target latitude
target_lon <- 7.776           # Target longitude
target_alt <- 500             # Target altitude (m)
define_start_date <- "1991-01-01"
define_end_date <- "2025-12-31"
```

### Customizing Variables
Modify the `wanted_desc` vector to select different meteorological parameters:
```r
wanted_desc <- c(
  "Global radiation; daily mean",
  "Precipitation; daily total 0 UTC - 0 UTC",
  # Add or remove parameters as needed
)
```

## Dependencies
Required R packages:
- `readr` - CSV reading
- `dplyr` - Data manipulation
- `geosphere` - Distance calculations
- `httr2` - HTTP requests
- `purrr` - Functional programming
- `stringr` - String manipulation
- `tidyr` - Data tidying
- `lubridate` - Date handling

## Data Source
Data provided by MeteoSwiss (Federal Office of Meteorology and Climatology)
- **Stations Metadata**: https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_stations.csv
- **Parameters Metadata**: https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_parameters.csv
- **Inventory**: https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_datainventory.csv
- **STAC API**: https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn/items

## License and Attribution
This script processes Open Government Data (OGD) from MeteoSwiss.
Please cite MeteoSwiss as the data source in any publications or derived products.

## Contact
For questions or issues, please refer to the MeteoSwiss data portal documentation.

## Last Updated
2026-04-10
