# README: MeteoSwiss Station to NetCDF Converter

## Purpose
This script transforms raw station-based meteorological CSV data into a suite of NetCDF files
compatible with **LPJ-GUESS**. It ensures that the metadata follows CF-1.6 conventions and
specifically solves the "Coordinate not found" error by synchronizing floating-point precision
between binary data and text-based mapping files.

## Data Source
Input data is from the MeteoSwiss preprocessing script that generates:
- **File**: `MeteoSwiss_station/all_stations_RUE_replaced_daytime.csv`
- **Variables**: Temperature, radiation, humidity, wind speed, precipitation
- **Temporal resolution**: Daily
- **Temporal coverage**: 1991-01-01 to 2025-12-31
- **Spatial coverage**: Multiple stations including RUE (Hoelstein) and nearest stations

## Workflow Summary
1. **Data Loading:** Reads the MeteoSwiss CSV and converts date strings to POSIX objects.
2. **Coordinate Precision Fix:** Forces all latitudes and longitudes to exactly **6 decimal places** (`%.6f`).
3. **Station Indexing:** Creates numeric `landid` for each station.
4. **Matrix Transformation:** Reshapes long-format CSV data into a 2D matrix (Station × Time).
5. **NetCDF Generation:** Writes variables using `double` precision for coordinates and `float` for data.
6. **Metadata Export:** Generates `.txt` and `.dat` files required for LPJ-GUESS execution.

## Input File Structure
The input CSV (`all_stations_RUE_replaced_daytime.csv`) must contain the following columns:

```
| Column Name                  | Description                          | Units     |
|------------------------------|--------------------------------------|-----------|
| station_abbr                 | Station abbreviation (e.g., BAS, RUE) | -         |
| date                         | Date (YYYY-MM-DD HH:MM:SS)          | UTC       |
| lat                          | Latitude (decimal degrees)           | degrees N |
| lon                          | Longitude (decimal degrees)          | degrees E |
| temperature                  | Air temperature (daytime mean)       | K         |
| global_radiation             | Solar radiation (daytime mean)       | W m⁻²     |
| relative_humidity            | Relative humidity (daytime mean)     | %         |
| wind_speed                   | Wind speed (daytime mean)            | m s⁻¹     |
| precipitation                | Daily total precipitation            | mm        |
```

**Note**: Temperature is already in Kelvin (K), not Celsius.

## Output Files

### Directory
```
MeteoSwiss_station_to_netcdf_daytime/
```

### 1. NetCDF Climate Files (`.nc`)
One file is created for each variable:
- `air_temperature.nc` - Temperature (K)
- `surface_downwelling_shortwave_flux.nc` - Radiation (W m⁻²)
- `relative_humidity.nc` - Humidity (%)
- `wind_speed.nc` - Wind speed (m s⁻¹)
- `precipitation_amount.nc` - Precipitation (kg m⁻²)

**NetCDF Attributes:**
- **Time Axis:** Days since `1991-01-01 00:00:00`
- **Calendar:** `proleptic_gregorian`
- **Coordinate Precision:** Stored as **double** (8-byte) to prevent binary drift
- **Data Precision:** Stored as **float** (4-byte) to save space
- **Missing Value:** `-9999`

### 2. Spatial Mapping Files
These files are used by LPJ-GUESS to locate data in space:

**`gridlist_SCCII.txt`**: Space-separated file with `landid` and `station_abbr`
```
1 BAS
2 MUT
3 RIE
...
```

**`gridlist_SCCII_RUE.txt`**: Same format but only contains the RUE station
```
5 RUE
```

**`soil_SCCII.dat`**: Space-separated file with `longitude`, `latitude`, and soil code
```
7.776000 47.439000 6
7.589200 47.547800 6
...
```
- Coordinates formatted to **exactly 6 decimal places**
- Soil code `6` is default (can be modified for specific sites)

## Variable Mapping Table

| R Variable Name | CF Standard Name | NetCDF Variable | Units | Description |
|----------------|------------------|-----------------|-------|-------------|
| `temperature` | `air_temperature` | `air_temperature` | K | Daytime mean (07:30-19:30) |
| `global_radiation` | `surface_downwelling_shortwave_flux` | `surface_downwelling_shortwave_flux` | W m⁻² | Daytime mean (07:30-19:30) |
| `relative_humidity` | `relative_humidity` | `relative_humidity` | 1 | Daytime mean (07:30-19:30) |
| `wind_speed` | `wind_speed` | `wind_speed` | m s⁻¹ | Daytime mean (07:30-19:30) |
| `precipitation` | `precipitation_amount` | `precipitation_amount` | kg m⁻² | Daily total (24-hour) |

**Note**: 1 mm precipitation = 1 kg m⁻²

## Key Technical Features

### 1. Precision Synchronization (Critical for LPJ-GUESS)
```r
# Forces exact 6-decimal precision
lat = as.numeric(sprintf("%.6f", first(lat)))
lon = as.numeric(sprintf("%.6f", first(lon)))
```
This ensures that coordinates in NetCDF binary match exactly with text files.

### 2. Double Precision for Coordinates
```r
var_lon <- ncvar_def("lon", "degrees_east", list(dim_land), fillvalue, 
                      "longitude", prec = "double")
```
Prevents rounding errors during NetCDF write operations.

### 3. CF Standard Names
All variables use CF-1.6 compliant standard names for interoperability.

### 4. Flexible Time Handling
- Time origin: `1991-01-01 00:00:00`
- Automatically detects all unique dates from input data
- Handles irregular time steps gracefully

## Troubleshooting "Coordinate Not Found"

If LPJ-GUESS reports that a coordinate cannot be found in the soil map, verify:

### 1. Check NetCDF Coordinates
```bash
ncdump -v lon,lat air_temperature.nc | head -20
```
Expected output: `7.776000, 47.439000` (6 decimal places)

### 2. Check soil_SCCII.dat
```bash
cat soil_SCCII.dat
```
Should show identical coordinates: `7.776000 47.439000 6`

### 3. Verify Coordinate Order
This script produces: `Lon Lat Value`
Ensure your LPJ-GUESS instruction file (`.ins`) expects this order.

### 4. Check for Exact Binary Match
LPJ-GUESS performs exact binary matching. The `double` precision in NetCDF
ensures the C++ lookup succeeds.

### 5. Verify Date Range
```r
# Check time coverage in R
range(daily_filtered$date)
```

## Usage Instructions

### Prerequisites
```r
# Install required R packages
install.packages(c("dplyr", "tidyr", "readr", "ncdf4", "lubridate"))

# On Linux, you may also need:
# sudo apt-get install libnetcdf-dev netcdf-bin
```

### Running the Script
```r
# Set working directory
setwd("/dss/dssfs02/lwp-dss-0001/pr48va/pr48va-dss-0000/yixuan/LPJ_GUESS_HYD/MeteoSwiss")

# Source the conversion script
source("MeteoSwiss_data_processing/station_to_netcdf_daytime.R")
```

### Verifying Output
```bash
# Check NetCDF file structure
ncdump -h air_temperature.nc

# Extract time axis
cdo showtime air_temperature.nc

# Check for missing values
cdo info air_temperature.nc
```

## Integration with LPJ-GUESS

### Required Files in LPJ-GUESS Directory
```
your_lpjguess_run/
├── gridlist_SCCII.txt          # Station list
├── soil_SCCII.dat              # Soil properties
└── climate/
    ├── air_temperature.nc
    ├── surface_downwelling_shortwave_flux.nc
    ├── relative_humidity.nc
    ├── wind_speed.nc
    └── precipitation_amount.nc
```

### Sample Instruction File (`.ins`) Entry
```
climate_netcdf air_temperature.nc air_temperature
climate_netcdf surface_downwelling_shortwave_flux.nc surface_downwelling_shortwave_flux
climate_netcdf relative_humidity.nc relative_humidity
climate_netcdf wind_speed.nc wind_speed
climate_netcdf precipitation_amount.nc precipitation_amount
```

## Limitations and Notes

1. **Daytime Only Variables**: Temperature, radiation, humidity, and wind speed are
   calculated as daytime means (07:30-19:30 UTC), not 24-hour means.
2. **Precipitation**: Daily total includes nighttime precipitation.
3. **Soil Code**: All grid cells use default soil code `6`. Modify `soil_SCCII.dat`
   if specific soil properties are needed.
4. **Missing Values**: Gaps in source data become `-9999` in NetCDF.
5. **Single RUE Station**: The script includes nearest stations for spatial context,
   but you can filter to only RUE using the provided `gridlist_SCCII_RUE.txt`.

## Dependencies

### R Packages
- `dplyr` (>= 1.0.0) - Data manipulation
- `tidyr` (>= 1.0.0) - Data reshaping
- `readr` (>= 1.3.0) - CSV reading
- `ncdf4` (>= 1.17) - NetCDF writing
- `lubridate` (>= 1.7.0) - Date handling

### System Libraries
- NetCDF library (libnetcdf)
- CDO (Climate Data Operators) - Optional, for verification

## Output Summary

After successful execution, you should see:
```
Processing variable: temperature
Processing variable: global_radiation
Processing variable: relative_humidity
Processing variable: wind_speed
Processing variable: precipitation

✅ SUCCESS: NetCDF and Soil Map are perfectly synchronized at 6 decimal places.
```

## Version History

| Version | Date | Changes |
|---------|------|---------|
| v1.0 | 2024-01-15 | Initial release with 6-decimal precision fix |
| v1.1 | 2026-05-04 | Added daytime-only variable handling, RUE-only gridlist |

## Contact

- **Script Author**: Yixuan
- **Data Source**: MeteoSwiss Open Data
- **LPJ-GUESS Version**: Compatible with versions supporting NetCDF input

---
*Generated: 2026-05-04 16:41:03*
*Script: station_to_netcdf_daytime.R*

