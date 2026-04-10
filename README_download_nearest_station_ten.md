# MeteoSwiss Station Data Processing - 10 Nearest Stations

## Overview
This script downloads meteorological data from the 10 nearest MeteoSwiss stations to a target location,
processes daily measurements, and fills missing values using data from the other stations as backups.
Each station is processed as the primary station once, with the remaining 9 stations serving as backups.

## Target Location
- **Latitude**: 47.439
- **Longitude**: 7.776
- **Altitude**: 500 m

## Date Range Processed
- **Start Date**: 1991-01-01
- **End Date**: 2025-12-31

## The 10 Nearest Stations
| Rank | Station Abbr | Station Name | Distance (km) | Alt Diff (m) | Latitude | Longitude | Altitude (m) |
|------|--------------|--------------|---------------|--------------|----------|-----------|--------------|
| 1 | RUE | Rünenberg | 7.8 | 111 | 47.434572 | 7.879414 | 611 |
| 2 | STC | St. Chrischona | 16.22 | 7 | 47.571767 | 7.687094 | 493 |
| 3 | MOE | Möhlin | 16.69 | 157 | 47.572197 | 7.877911 | 343 |
| 4 | GOE | Gösgen | 17.13 | 120 | 47.363147 | 7.973733 | 380 |
| 5 | BAS | Basel / Binningen | 18.41 | 184 | 47.541142 | 7.583525 | 316 |
| 6 | WYN | Wynau | 20.5 | 78 | 47.255025 | 7.787475 | 422 |
| 7 | BUS | Buchs / Aarau | 23.66 | 113 | 47.384381 | 8.07955 | 387 |
| 8 | DEM | Delémont | 33.57 | 61 | 47.351706 | 7.349567 | 439 |
| 9 | EGO | Egolzwil | 33.66 | 22 | 47.179428 | 8.004758 | 522 |
| 10 | PSI | Würenlingen / PSI | 35.61 | 166 | 47.536475 | 8.226944 | 334 |

## Parameters Processed
- Global radiation; daily mean
- Precipitation; daily total 0 UTC - 0 UTC
- Air temperature 2 m above ground; daily mean
- Air temperature 2 m above ground; daily maximum
- Air temperature 2 m above ground; daily minimum
- Relative air humidity 2 m above ground; daily mean
- Wind speed scalar; daily mean in m/s

## Data Processing Steps
1. **Metadata Download**: Reads station metadata, parameter definitions, and inventory from MeteoSwiss
2. **Station Selection**: Finds 10 nearest stations based on geographic coordinates
3. **Data Download**: Fetches daily data from STAC API (combines recent and historical data)
4. **Data Processing for Each Station**: 
   - Converts temperature from °C to Kelvin (+273.15)
   - Converts wind speed from km/h to m/s (÷3.6)
   - Standardizes date formats
   - Filters to specified date range
5. **Missing Value Filling** (for each station as primary): 
   - Uses the other 9 stations as backups in order of proximity
   - Fills missing values on a per-variable, per-day basis
   - Tracks which backup station filled each value
6. **Output Generation**: Creates multiple CSV files with results

## Output Files

### Main Data Files
- **`all_10_stations_filled_19910101_to_20251231.csv`**: Complete filled daily data for all 10 stations
  - Each row includes the primary station's data with its metadata
  - Columns: station_abbr, date, date_day, all meteorological parameters, filling metadata
  - Plus primary station coordinates, altitude, distance, and altitude difference
  - Temperature in Kelvin, Precipitation in mm, Radiation in W/m², Humidity in %, Wind in m/s

- **Individual station files**: `station_[ABBR]_filled_YYYYMMDD_to_YYYYMMDD.csv`
  - Same structure as above but filtered for each specific station

### Filling Reports
- **`all_stations_fill_source_report.csv`**: Detailed record of each filled value across all stations
  - **Columns**: date_day, variable, filled_by_station_abbr, filled_by_station_name, primary_station_abbr, primary_station_name
  - Shows which backup station provided each value (includes both abbreviation and full name)
  - Identifies which primary station received the fill
  - Simple format focused on station identification

- **`all_stations_filling_summary.csv`**: Summary statistics for each station
  - Station information (name, coordinates, altitude)
  - Distance and altitude difference to target
  - Total days processed
  - Original NA count, filled NA count, remaining NA count
  - Fill percentage achieved

## Data Quality Notes
- **Temperature**: Converted to Kelvin (K) for thermodynamic consistency (0°C = 273.15K)
- **Wind Speed**: Converted to meters per second (m/s) from km/h
- **Precipitation**: Daily totals in millimeters (mm)
- **Radiation**: Daily mean in Watts per square meter (W/m²)
- **Humidity**: Daily mean relative humidity (%)
- **Missing Data**: Values that couldn't be filled by any backup station remain as NA

## Overall Filling Statistics
- **Total Original Missing Values** (across all stations): 177564
- **Total Values Filled**: 177564
- **Total Remaining Missing**: 0
- **Overall Fill Percentage**: 100%

## Per-Station Filling Summary
   station_abbr      station_name distance_to_target_km total_days original_na filled_na remaining_na fill_percentage
1           RUE         Rünenberg              7.802335      12784           9         9            0             100
2           STC    St. Chrischona             16.221361      12186       28433     28433            0             100
3           MOE            Möhlin             16.690780      12784       44404     44404            0             100
4           GOE            Gösgen             17.125216      12784       12864     12864            0             100
5           BAS Basel / Binningen             18.409247      12784          23        23            0             100
6           WYN             Wynau             20.498284      12784          32        32            0             100
7           BUS     Buchs / Aarau             23.661765      12784          19        19            0             100
8           DEM          Delémont             33.571457      12782       16012     16012            0             100
9           EGO          Egolzwil             33.661168      12784       44629     44629            0             100
10          PSI Würenlingen / PSI             35.614937      12405       31139     31139            0             100

## Usage Instructions

### Running the Script
```r
source("meteoswiss_10_stations_processing.R")
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

## Methodology Notes
1. **Distance Calculation**: Haversine formula for great-circle distance between coordinates
2. **Station Ranking**: Sorted by distance, then by altitude difference to target
3. **Backup Priority**: Backup stations are tried in order of increasing distance from target
4. **Filling Strategy**: For each missing value, the script finds the closest backup station with data for that specific date and variable
5. **Data Completeness**: Not all stations have all parameters; the script handles missing parameters gracefully

## License and Attribution
This script processes Open Government Data (OGD) from MeteoSwiss.
Please cite MeteoSwiss as the data source in any publications or derived products.

## Contact
For questions about the data, please refer to the MeteoSwiss data portal documentation.
For questions about this script, refer to the comments in the source code.

## Last Updated
2026-04-10
