# MeteoSwiss Data Processing

A collection of R scripts for downloading, processing, and converting meteorological data from [MeteoSwiss](https://www.meteoswiss.admin.ch/) (the Swiss Federal Office of Meteorology and Climatology) into analysis-ready formats.

---

## Overview

This project automates the full pipeline from raw MeteoSwiss Open Government Data (OGD) to clean, gap-filled CSV files and CF-convention NetCDF files. It is designed for researchers who need long-term daily weather records for specific Swiss locations—particularly for use as climate forcing input to process-based ecosystem models such as [LPJ-GUESS](https://web.nateko.lu.se/lpj-guess/).

The pipeline covers:
- Discovering the nearest meteorological stations to any target coordinate in Switzerland
- Downloading historical and recent daily measurements via the MeteoSwiss STAC API
- Filling missing values using data from surrounding backup stations
- Converting the filled station data to NetCDF format with CF-1.6 metadata
- Downloading MeteoSwiss surface- and satellite-derived gridded NetCDF archives

---

## Features

- **Automatic station discovery** — finds the *N* nearest MeteoSwiss stations using the Haversine great-circle distance formula
- **Multi-station gap-filling** — fills missing daily values by drawing from nearby backup stations in order of proximity
- **Unit conversion** — temperatures converted from °C to Kelvin; wind speeds from km/h to m/s
- **Comprehensive output** — fill-source reports, per-variable summaries, and "still-missing" logs
- **NetCDF export** — CF-1.6-compliant NetCDF files with `double`-precision coordinates to avoid LPJ-GUESS "Coordinate not found" errors
- **Gridded archive download** — bulk download of MeteoSwiss surface- and satellite-derived grid products (1991–2024)

---

## Requirements

### Software
- **R** ≥ 4.0

### R Packages
Install all required packages with:

```r
install.packages(c(
  "readr", "dplyr", "geosphere", "httr2",
  "purrr", "stringr", "tidyr", "lubridate",
  "ncdf4", "tibble"
))
```

| Package | Used by | Purpose |
|---|---|---|
| `readr` | all station scripts | Reading/writing CSV files |
| `dplyr` | all scripts | Data manipulation |
| `geosphere` | station scripts | Haversine distance calculations |
| `httr2` | station & download scripts | HTTP requests to STAC API |
| `purrr` | all scripts | Functional programming utilities |
| `stringr` | all scripts | String manipulation |
| `tidyr` | all scripts | Data reshaping |
| `lubridate` | all scripts | Date/time handling |
| `ncdf4` | `station_to_netcdf.R` | Reading and writing NetCDF files |
| `tibble` | `download_netcdf.R` | Tidy data frames |

---

## Installation

```bash
git clone https://github.com/yxw15/MeteoSwiss_data_processing.git
cd MeteoSwiss_data_processing
```

No build step is required. Open the desired `.R` script in RStudio or run it from the R console.

> **Working directory**: Each script calls `setwd(...)` with a path specific to the author's cluster environment. Change this line to your own working directory before running any script.

---

## Configuration

All user-configurable settings are grouped at the top of each script under a clearly labelled **"User settings"** section. No external config files or environment variables are used.

### Common settings (station download scripts)

```r
# Target location
target_lat <- 47.439          # Latitude (decimal degrees)
target_lon <- 7.776           # Longitude (decimal degrees)
target_alt <- 500             # Altitude in metres

# Date range
define_start_date <- "1991-01-01"
define_end_date   <- "2025-12-31"

# Meteorological variables to retrieve
wanted_desc <- c(
  "Global radiation; daily mean",
  "Precipitation; daily total 0 UTC - 0 UTC",
  "Air temperature 2 m above ground; daily mean",
  "Air temperature 2 m above ground; daily maximum",
  "Air temperature 2 m above ground; daily minimum",
  "Relative air humidity 2 m above ground; daily mean",
  "Wind speed scalar; daily mean in m/s"
)
```

### MeteoSwiss API endpoints (pre-configured)

```r
stations_url   <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_stations.csv"
params_url     <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_parameters.csv"
inventory_url  <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_datainventory.csv"
stac_items_url <- "https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn/items"
```

---

## Usage

### 1. Download data from the single nearest station

Downloads daily data for the closest MeteoSwiss station and fills gaps with up to 9 backup stations.

```r
source("download_nearest_station_one.R")
```

Outputs are written to the `MeteoSwiss_station/` directory. See [`README_download_nearest_station_one.md`](README_download_nearest_station_one.md) for a full description of output files and filling statistics.

---

### 2. Download data from the 10 nearest stations

Processes all 10 nearest stations simultaneously. Each station is treated as primary in turn, with the remaining 9 acting as backups.

```r
source("download_nearest_station_ten.R")
```

Outputs are written to `MeteoSwiss_station/`. See [`README_download_nearest_station_ten.md`](README_download_nearest_station_ten.md) for details.

---

### 3. Convert station CSV to NetCDF

Transforms the gap-filled CSV produced by step 2 into CF-1.6-compliant NetCDF files for use with LPJ-GUESS.

**Input:** `MeteoSwiss_station/all_filtered_19910101_to_20251231.csv`

```r
source("station_to_netcdf.R")
```

Outputs are written to `MeteoSwiss_station_to_netcdf/`. See [`README_station_to_netcdf.md`](README_station_to_netcdf.md) for details on the NetCDF structure and troubleshooting tips.

---

### 4. Download gridded NetCDF archives

Bulk-downloads MeteoSwiss surface- and satellite-derived gridded products (1991–2024) via the STAC API.

```r
source("download_netcdf.R")
```

Files are saved to the directory configured as `out_dir` at the top of the script.

---

### 5. Check NetCDF files

Utility script for inspecting downloaded or generated NetCDF files.

```r
source("check_netcdf.R")
```

---

## Data Sources

All meteorological data are Open Government Data (OGD) provided by **MeteoSwiss**.

| Resource | URL |
|---|---|
| Station metadata | <https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_stations.csv> |
| Parameter metadata | <https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_parameters.csv> |
| Data inventory | <https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_datainventory.csv> |
| Daily station data (STAC API) | <https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn/items> |
| Surface-derived grids (STAC) | `ch.meteoschweiz.ogd-surface-derived-grid` collection |
| Satellite-derived grids (STAC) | `ch.meteoschweiz.ogd-satellite-derived-grid` collection |

---

## Project Structure

```
MeteoSwiss_data_processing/
├── download_nearest_station_one.R      # Download & gap-fill: single nearest station
├── download_nearest_station_ten.R      # Download & gap-fill: 10 nearest stations
├── station_to_netcdf.R                 # Convert filled CSV to NetCDF (LPJ-GUESS ready)
├── download_netcdf.R                   # Bulk download of gridded NetCDF archives
├── check_netcdf.R                      # Inspect NetCDF files
├── README.md                           # This file
├── README_download_nearest_station_one.md
├── README_download_nearest_station_ten.md
└── README_station_to_netcdf.md
```

**Generated directories (not tracked in git):**

| Directory | Created by | Contents |
|---|---|---|
| `MeteoSwiss_station/` | station download scripts | Gap-filled CSV files, fill reports |
| `MeteoSwiss_station_to_netcdf/` | `station_to_netcdf.R` | NetCDF climate files, gridlist, soil map |
| `MeteoSwiss_netcdf/` | `download_netcdf.R` | Raw gridded NetCDF archives |

---

## Development

### Linting

There is no automated linter configured for this project. For R code style, consider using [lintr](https://lintr.r-lib.org/):

```r
install.packages("lintr")
lintr::lint("download_nearest_station_one.R")
```

### Testing

There is no automated test suite. To verify a script produces expected output, run it against the default target location (lat 47.439, lon 7.776) and check that output CSV files are created in `MeteoSwiss_station/`.

---

## Contributing

Contributions are welcome. Please follow these steps:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-improvement`)
3. Commit your changes (`git commit -m "Add my improvement"`)
4. Push to your fork and open a Pull Request

Please keep scripts self-contained with a clearly documented **User settings** section at the top, consistent with the existing style.

---

## License

> **No license file is currently present in this repository.**
> The author has not yet chosen a license. Until a license is added, this code is under exclusive copyright by default—please contact the repository owner before using or distributing this code.
>
> If you are the repository owner, consider adding a license. Common choices for research software include [MIT](https://choosealicense.com/licenses/mit/), [GPL-3.0](https://choosealicense.com/licenses/gpl-3.0/), or [Apache-2.0](https://choosealicense.com/licenses/apache-2.0/).

---

## Disclaimer

This project is an independent research tool and is **not affiliated with or endorsed by MeteoSwiss**. All data retrieved from the MeteoSwiss Open Government Data portal are subject to MeteoSwiss's own terms of use. Please cite MeteoSwiss as the data source in any publications or derived products:

> MeteoSwiss – Federal Office of Meteorology and Climatology, Open Government Data, <https://www.meteoswiss.admin.ch/>
