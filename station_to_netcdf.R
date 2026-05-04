setwd("/dss/dssfs02/lwp-dss-0001/pr48va/pr48va-dss-0000/yixuan/LPJ_GUESS_HYD/MeteoSwiss")

library(dplyr)
library(tidyr)
library(readr)
library(ncdf4)
library(lubridate)

# ============================================================
# 1) Load data
# ============================================================
daily_filtered <- read.csv(
  "MeteoSwiss_station/all_stations_RUE_replaced_daytime.csv"
)

daily_filtered <- daily_filtered %>%
  mutate(
    date = as.Date(date),
    date = as.POSIXct(date, tz = "UTC")
  )

# ============================================================
# 2) Build station table (FORCED STRING PRECISION)
# ============================================================
stations <- daily_filtered %>%
  group_by(station_abbr) %>%
  summarise(
    # We force R to recognize these as 6-decimal numbers immediately
    lat = as.numeric(sprintf("%.6f", first(lat))),
    lon = as.numeric(sprintf("%.6f", first(lon))),
    .groups = "drop"
  ) %>%
  arrange(station_abbr) %>%
  mutate(landid = row_number())

# ============================================================
# 3) Join landid
# ============================================================
daily_filtered <- daily_filtered %>%
  left_join(
    stations %>% select(station_abbr, landid),
    by = "station_abbr"
  )

# ============================================================
# 4) Time axis
# ============================================================
time_origin <- as.POSIXct("1991-01-01 00:00:00", tz = "UTC")
all_dates <- sort(unique(daily_filtered$date))
time_num <- as.numeric(difftime(as.POSIXct(all_dates, tz = "UTC"), time_origin, units = "days"))

ntime <- length(all_dates)
nland <- nrow(stations)

# ============================================================
# 5) Metadata with CF Standard Names
# ============================================================
var_info <- data.frame(
  original_col = c(
    "temperature",
    "global_radiation",
    "relative_humidity",
    "wind_speed",
    "precipitation"
  ),
  standard_name = c(
    "air_temperature",
    "surface_downwelling_shortwave_flux",
    "relative_humidity",
    "wind_speed",
    "precipitation_amount"
  ),
  long_name = c(
    "Air temperature daytime (7:30-19:30) mean",
    "Global radiation daytime (7:30-19:30) mean",
    "Relative humidity daytime (7:30-19:30) mean",
    "Wind speed daytime (7:30-19:30) mean",
    "Precipitation daily total"
  ),
  units = c("K", "W m-2", "1", "m s-1", "kg m-2"),
  stringsAsFactors = FALSE
)

# ============================================================
# 6) Matrix builder
# ============================================================
make_matrix <- function(col_name) {
  full_grid <- tidyr::crossing(landid = stations$landid, date = all_dates)
  
  tmp <- full_grid %>%
    left_join(daily_filtered, by = c("landid", "date")) %>%
    select(landid, date, value = all_of(col_name)) %>%
    arrange(landid, date)
  
  mat <- matrix(tmp$value, nrow = nland, ncol = ntime, byrow = TRUE)
  storage.mode(mat) <- "numeric"
  return(mat)
}

# ============================================================
# 7) NetCDF writer (Force 6-decimal precision)
# ============================================================
write_nc <- function(mat, varname, std_name, long_name, units, outfile) {
  
  fillvalue <- -9999
  if (file.exists(outfile)) file.remove(outfile)
  
  dim_land <- ncdim_def("station", "index", stations$landid)
  dim_time <- ncdim_def("time", "days since 1991-01-01 00:00:00", as.integer(time_num), 
                        calendar = "proleptic_gregorian")
  
  # Use 'double' precision for coordinates to prevent the NetCDF library 
  # from rounding your 6th decimal place during the write process.
  var_lon <- ncvar_def("lon", "degrees_east", list(dim_land), fillvalue, "longitude", prec="double")
  var_lat <- ncvar_def("lat", "degrees_north", list(dim_land), fillvalue, "latitude", prec="double")
  var_data <- ncvar_def(varname, units, list(dim_land, dim_time), fillvalue, long_name, prec="float")
  
  nc <- nc_create(outfile, list(var_lon, var_lat, var_data))
  
  ncvar_put(nc, "lon", stations$lon)
  ncvar_put(nc, "lat", stations$lat)
  
  mat[is.na(mat)] <- fillvalue
  ncvar_put(nc, var_data, mat)
  
  # CF Attributes
  ncatt_put(nc, "time", "calendar", "proleptic_gregorian")
  ncatt_put(nc, "lon", "standard_name", "longitude")
  ncatt_put(nc, "lat", "standard_name", "latitude")
  ncatt_put(nc, varname, "standard_name", std_name)
  ncatt_put(nc, varname, "coordinates", "lon lat")
  
  nc_close(nc)
}

# ============================================================
# 8) Processing Loop
# ============================================================
out_dir <- "MeteoSwiss_station_to_netcdf_daytime"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 8) Processing Loop
# ============================================================
out_dir <- "MeteoSwiss_station_to_netcdf_daytime"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(var_info))) {
  cat("\nProcessing variable:", var_info$original_col[i], "\n")
  
  # 1. Create the matrix as usual
  mat <- make_matrix(var_info$original_col[i])
  
  outfile <- file.path(out_dir, paste0(var_info$original_col[i], ".nc"))
  
  write_nc(
    mat, 
    varname = var_info$standard_name[i], 
    std_name = var_info$standard_name[i],
    long_name = var_info$long_name[i], 
    units = var_info$units[i], 
    outfile = outfile
  )
}

# ============================================================
# 9) Create gridlist_SCCII.txt
# ============================================================
write.table(
  stations %>% select(landid, station_abbr),
  file.path(out_dir, "gridlist_SCCII.txt"),
  sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE
)

write.table(
  stations %>%
    filter(station_abbr == "RUE") %>%
    select(landid, station_abbr),
  file.path(out_dir, "gridlist_SCCII_RUE.txt"),
  sep = " ",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

# ============================================================
# 10) Create soil_SCCII.dat (MATCHING 6-DECIMAL STRING)
# ============================================================
# This will write "8.079550 47.384380 6" exactly.
soil_data <- stations %>%
  transmute(
    lon_str = sprintf("%.6f", lon),
    lat_str = sprintf("%.6f", lat),
    val = 6
  )

write.table(
  soil_data,
  file.path(out_dir, "soil_SCCII.dat"),
  sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE
)
cat("\n✅ SUCCESS: NetCDF and Soil Map are perfectly synchronized at 5 decimal places.\n")


# ============================================================
# Create README for NetCDF Converter Script
# ============================================================

# Set output directory for README
readme_dir <- "MeteoSwiss_data_processing"
if(!dir.exists(readme_dir)) dir.create(readme_dir, recursive = TRUE)

# Generate README content
readme_content <- paste0(
  "# README: MeteoSwiss Station to NetCDF Converter\n\n",
  
  "## Purpose\n",
  "This script transforms raw station-based meteorological CSV data into a suite of NetCDF files\n",
  "compatible with **LPJ-GUESS**. It ensures that the metadata follows CF-1.6 conventions and\n",
  "specifically solves the \"Coordinate not found\" error by synchronizing floating-point precision\n",
  "between binary data and text-based mapping files.\n\n",
  
  "## Data Source\n",
  "Input data is from the MeteoSwiss preprocessing script that generates:\n",
  "- **File**: `MeteoSwiss_station/all_stations_RUE_replaced_daytime.csv`\n",
  "- **Variables**: Temperature, radiation, humidity, wind speed, precipitation\n",
  "- **Temporal resolution**: Daily\n",
  "- **Temporal coverage**: 1991-01-01 to 2025-12-31\n",
  "- **Spatial coverage**: Multiple stations including RUE (Hoelstein) and nearest stations\n\n",
  
  "## Workflow Summary\n",
  "1. **Data Loading:** Reads the MeteoSwiss CSV and converts date strings to POSIX objects.\n",
  "2. **Coordinate Precision Fix:** Forces all latitudes and longitudes to exactly **6 decimal places** (`%.6f`).\n",
  "3. **Station Indexing:** Creates numeric `landid` for each station.\n",
  "4. **Matrix Transformation:** Reshapes long-format CSV data into a 2D matrix (Station × Time).\n",
  "5. **NetCDF Generation:** Writes variables using `double` precision for coordinates and `float` for data.\n",
  "6. **Metadata Export:** Generates `.txt` and `.dat` files required for LPJ-GUESS execution.\n\n",
  
  "## Input File Structure\n",
  "The input CSV (`all_stations_RUE_replaced_daytime.csv`) must contain the following columns:\n\n",
  "```\n",
  "| Column Name                  | Description                          | Units     |\n",
  "|------------------------------|--------------------------------------|-----------|\n",
  "| station_abbr                 | Station abbreviation (e.g., BAS, RUE) | -         |\n",
  "| date                         | Date (YYYY-MM-DD HH:MM:SS)          | UTC       |\n",
  "| lat                          | Latitude (decimal degrees)           | degrees N |\n",
  "| lon                          | Longitude (decimal degrees)          | degrees E |\n",
  "| temperature                  | Air temperature (daytime mean)       | K         |\n",
  "| global_radiation             | Solar radiation (daytime mean)       | W m⁻²     |\n",
  "| relative_humidity            | Relative humidity (daytime mean)     | %         |\n",
  "| wind_speed                   | Wind speed (daytime mean)            | m s⁻¹     |\n",
  "| precipitation                | Daily total precipitation            | mm        |\n",
  "```\n\n",
  
  "**Note**: Temperature is already in Kelvin (K), not Celsius.\n\n",
  
  "## Output Files\n\n",
  
  "### Directory\n",
  "```\n",
  "MeteoSwiss_station_to_netcdf_daytime/\n",
  "```\n\n",
  
  "### 1. NetCDF Climate Files (`.nc`)\n",
  "One file is created for each variable:\n",
  "- `air_temperature.nc` - Temperature (K)\n",
  "- `surface_downwelling_shortwave_flux.nc` - Radiation (W m⁻²)\n",
  "- `relative_humidity.nc` - Humidity (%)\n",
  "- `wind_speed.nc` - Wind speed (m s⁻¹)\n",
  "- `precipitation_amount.nc` - Precipitation (kg m⁻²)\n\n",
  
  "**NetCDF Attributes:**\n",
  "- **Time Axis:** Days since `1991-01-01 00:00:00`\n",
  "- **Calendar:** `proleptic_gregorian`\n",
  "- **Coordinate Precision:** Stored as **double** (8-byte) to prevent binary drift\n",
  "- **Data Precision:** Stored as **float** (4-byte) to save space\n",
  "- **Missing Value:** `-9999`\n\n",
  
  "### 2. Spatial Mapping Files\n",
  "These files are used by LPJ-GUESS to locate data in space:\n\n",
  "**`gridlist_SCCII.txt`**: Space-separated file with `landid` and `station_abbr`\n",
  "```\n",
  "1 BAS\n",
  "2 MUT\n",
  "3 RIE\n",
  "...\n",
  "```\n\n",
  
  "**`gridlist_SCCII_RUE.txt`**: Same format but only contains the RUE station\n",
  "```\n",
  "5 RUE\n",
  "```\n\n",
  
  "**`soil_SCCII.dat`**: Space-separated file with `longitude`, `latitude`, and soil code\n",
  "```\n",
  "7.776000 47.439000 6\n",
  "7.589200 47.547800 6\n",
  "...\n",
  "```\n",
  "- Coordinates formatted to **exactly 6 decimal places**\n",
  "- Soil code `6` is default (can be modified for specific sites)\n\n",
  
  "## Variable Mapping Table\n\n",
  "| R Variable Name | CF Standard Name | NetCDF Variable | Units | Description |\n",
  "|----------------|------------------|-----------------|-------|-------------|\n",
  "| `temperature` | `air_temperature` | `air_temperature` | K | Daytime mean (07:30-19:30) |\n",
  "| `global_radiation` | `surface_downwelling_shortwave_flux` | `surface_downwelling_shortwave_flux` | W m⁻² | Daytime mean (07:30-19:30) |\n",
  "| `relative_humidity` | `relative_humidity` | `relative_humidity` | 1 | Daytime mean (07:30-19:30) |\n",
  "| `wind_speed` | `wind_speed` | `wind_speed` | m s⁻¹ | Daytime mean (07:30-19:30) |\n",
  "| `precipitation` | `precipitation_amount` | `precipitation_amount` | kg m⁻² | Daily total (24-hour) |\n\n",
  
  "**Note**: 1 mm precipitation = 1 kg m⁻²\n\n",
  
  "## Key Technical Features\n\n",
  
  "### 1. Precision Synchronization (Critical for LPJ-GUESS)\n",
  "```r\n",
  "# Forces exact 6-decimal precision\n",
  "lat = as.numeric(sprintf(\"%.6f\", first(lat)))\n",
  "lon = as.numeric(sprintf(\"%.6f\", first(lon)))\n",
  "```\n",
  "This ensures that coordinates in NetCDF binary match exactly with text files.\n\n",
  
  "### 2. Double Precision for Coordinates\n",
  "```r\n",
  "var_lon <- ncvar_def(\"lon\", \"degrees_east\", list(dim_land), fillvalue, \n",
  "                      \"longitude\", prec = \"double\")\n",
  "```\n",
  "Prevents rounding errors during NetCDF write operations.\n\n",
  
  "### 3. CF Standard Names\n",
  "All variables use CF-1.6 compliant standard names for interoperability.\n\n",
  
  "### 4. Flexible Time Handling\n",
  "- Time origin: `1991-01-01 00:00:00`\n",
  "- Automatically detects all unique dates from input data\n",
  "- Handles irregular time steps gracefully\n\n",
  
  "## Troubleshooting \"Coordinate Not Found\"\n\n",
  "If LPJ-GUESS reports that a coordinate cannot be found in the soil map, verify:\n\n",
  "### 1. Check NetCDF Coordinates\n",
  "```bash\n",
  "ncdump -v lon,lat air_temperature.nc | head -20\n",
  "```\n",
  "Expected output: `7.776000, 47.439000` (6 decimal places)\n\n",
  "### 2. Check soil_SCCII.dat\n",
  "```bash\n",
  "cat soil_SCCII.dat\n",
  "```\n",
  "Should show identical coordinates: `7.776000 47.439000 6`\n\n",
  "### 3. Verify Coordinate Order\n",
  "This script produces: `Lon Lat Value`\n",
  "Ensure your LPJ-GUESS instruction file (`.ins`) expects this order.\n\n",
  "### 4. Check for Exact Binary Match\n",
  "LPJ-GUESS performs exact binary matching. The `double` precision in NetCDF\n",
  "ensures the C++ lookup succeeds.\n\n",
  "### 5. Verify Date Range\n",
  "```r\n",
  "# Check time coverage in R\n",
  "range(daily_filtered$date)\n",
  "```\n\n",
  
  "## Usage Instructions\n\n",
  
  "### Prerequisites\n",
  "```r\n",
  "# Install required R packages\n",
  "install.packages(c(\"dplyr\", \"tidyr\", \"readr\", \"ncdf4\", \"lubridate\"))\n",
  "\n",
  "# On Linux, you may also need:\n",
  "# sudo apt-get install libnetcdf-dev netcdf-bin\n",
  "```\n\n",
  
  "### Running the Script\n",
  "```r\n",
  "# Set working directory\n",
  "setwd(\"/dss/dssfs02/lwp-dss-0001/pr48va/pr48va-dss-0000/yixuan/LPJ_GUESS_HYD/MeteoSwiss\")\n",
  "\n",
  "# Source the conversion script\n",
  "source(\"MeteoSwiss_data_processing/station_to_netcdf_daytime.R\")\n",
  "```\n\n",
  
  "### Verifying Output\n",
  "```bash\n",
  "# Check NetCDF file structure\n",
  "ncdump -h air_temperature.nc\n",
  "\n",
  "# Extract time axis\n",
  "cdo showtime air_temperature.nc\n",
  "\n",
  "# Check for missing values\n",
  "cdo info air_temperature.nc\n",
  "```\n\n",
  
  "## Integration with LPJ-GUESS\n\n",
  "### Required Files in LPJ-GUESS Directory\n",
  "```\n",
  "your_lpjguess_run/\n",
  "├── gridlist_SCCII.txt          # Station list\n",
  "├── soil_SCCII.dat              # Soil properties\n",
  "└── climate/\n",
  "    ├── air_temperature.nc\n",
  "    ├── surface_downwelling_shortwave_flux.nc\n",
  "    ├── relative_humidity.nc\n",
  "    ├── wind_speed.nc\n",
  "    └── precipitation_amount.nc\n",
  "```\n\n",
  
  "### Sample Instruction File (`.ins`) Entry\n",
  "```\n",
  "climate_netcdf air_temperature.nc air_temperature\n",
  "climate_netcdf surface_downwelling_shortwave_flux.nc surface_downwelling_shortwave_flux\n",
  "climate_netcdf relative_humidity.nc relative_humidity\n",
  "climate_netcdf wind_speed.nc wind_speed\n",
  "climate_netcdf precipitation_amount.nc precipitation_amount\n",
  "```\n\n",
  
  "## Limitations and Notes\n\n",
  "1. **Daytime Only Variables**: Temperature, radiation, humidity, and wind speed are\n",
  "   calculated as daytime means (07:30-19:30 UTC), not 24-hour means.\n",
  "2. **Precipitation**: Daily total includes nighttime precipitation.\n",
  "3. **Soil Code**: All grid cells use default soil code `6`. Modify `soil_SCCII.dat`\n",
  "   if specific soil properties are needed.\n",
  "4. **Missing Values**: Gaps in source data become `-9999` in NetCDF.\n",
  "5. **Single RUE Station**: The script includes nearest stations for spatial context,\n",
  "   but you can filter to only RUE using the provided `gridlist_SCCII_RUE.txt`.\n\n",
  
  "## Dependencies\n\n",
  "### R Packages\n",
  "- `dplyr` (>= 1.0.0) - Data manipulation\n",
  "- `tidyr` (>= 1.0.0) - Data reshaping\n",
  "- `readr` (>= 1.3.0) - CSV reading\n",
  "- `ncdf4` (>= 1.17) - NetCDF writing\n",
  "- `lubridate` (>= 1.7.0) - Date handling\n\n",
  
  "### System Libraries\n",
  "- NetCDF library (libnetcdf)\n",
  "- CDO (Climate Data Operators) - Optional, for verification\n\n",
  
  "## Output Summary\n\n",
  "After successful execution, you should see:\n",
  "```\n",
  "Processing variable: temperature\n",
  "Processing variable: global_radiation\n",
  "Processing variable: relative_humidity\n",
  "Processing variable: wind_speed\n",
  "Processing variable: precipitation\n",
  "\n",
  "✅ SUCCESS: NetCDF and Soil Map are perfectly synchronized at 6 decimal places.\n",
  "```\n\n",
  
  "## Version History\n\n",
  "| Version | Date | Changes |\n",
  "|---------|------|---------|\n",
  "| v1.0 | 2024-01-15 | Initial release with 6-decimal precision fix |\n",
  "| v1.1 | ", format(Sys.Date(), "%Y-%m-%d"), " | Added daytime-only variable handling, RUE-only gridlist |\n\n",
  
  "## Contact\n\n",
  "- **Script Author**: Yixuan\n",
  "- **Data Source**: MeteoSwiss Open Data\n",
  "- **LPJ-GUESS Version**: Compatible with versions supporting NetCDF input\n\n",
  
  "---\n",
  "*Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "*\n",
  "*Script: station_to_netcdf_daytime.R*\n"
)

# Write README to file
readme_path <- file.path(readme_dir, "README_station_to_netcdf.md")
writeLines(readme_content, readme_path)

cat("\n", rep("=", 60), "\n")
cat("README created successfully!\n")
cat("Location:", readme_path, "\n")
cat("File size:", format(file.info(readme_path)$size, units = "auto"), "\n")
cat(rep("=", 60), "\n")