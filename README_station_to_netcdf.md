# README: MeteoSwiss Station to NetCDF Converter

## Purpose
This script transforms raw station-based meteorological CSV data into a suite of NetCDF files compatible with **LPJ-GUESS**. It ensures that the metadata follows CF-1.6 conventions and specifically solves the "Coordinate not found" error by synchronizing floating-point precision between binary data and text-based mapping files.

## Workflow Summary
1.  **Data Cleaning:** Reads the MeteoSwiss CSV and converts date strings into POSIX objects.
2.  **Coordinate Precision Fix:** Forces all latitudes and longitudes to exactly **6 decimal places** (`%.6f`).
3.  **Matrix Transformation:** Reshapes long-format CSV data into a 2D matrix (Station × Time).
4.  **NetCDF Generation:** Writes variables using `double` precision for coordinates and `float` for data.
5.  **Metadata Export:** Generates the `.txt` and `.dat` files required for model execution.

---

## File Outputs
All outputs are generated in the directory: `MeteoSwiss_station_to_netcdf/`

### 1. NetCDF Climate Files (`.nc`)
One file is created for each variable (e.g., `mean_temperature.nc`, `precipitation.nc`).
* **Time Axis:** Days since `1991-01-01 00:00:00`.
* **Calendar:** `proleptic_gregorian`.
* **Coordinate Precision:** Stored as **double** to prevent binary drift and matching errors.

### 2. Spatial Mapping Files
These files are used by LPJ-GUESS to locate the data in space:
* **`gridlist_SCCII.txt`**: A space-separated file containing the `landid` (numeric index) and `station_abbr`.
* **`soil_SCCII.dat`**: A space-separated file containing `longitude`, `latitude`, and a default soil code (`6`).
    * *Note: Coordinates in this file are strictly formatted to 6 decimal places to match the NetCDF binary headers.*

---

## Variable Mapping Table

| Internal Name | CF Standard Name | Units | Description |
| :--- | :--- | :--- | :--- |
| `mean_temperature` | `air_temperature` | K | Daily mean at 2m |
| `max_temperature` | `air_temperature` | K | Daily maximum at 2m |
| `min_temperature` | `air_temperature` | K | Daily minimum at 2m |
| `radiation` | `surface_downwelling_shortwave_flux` | W m-2 | Global radiation |
| `relative_humidity` | `relative_humidity` | 1 | Fraction (0 to 1) |
| `wind_speed` | `wind_speed` | m s-1 | Scalar wind speed |
| `precipitation` | `precipitation_amount` | kg m-2 | Daily total |

---

## Troubleshooting "Coordinate Not Found"
If LPJ-GUESS still reports that a coordinate at a specific location could not be found in the soil map, verify the following:

1.  **Check `ncdump`:** Run `ncdump -v lon,lat [filename].nc`. The values should show 6 decimal places.
2.  **Check `soil_SCCII.dat`:** Open the file in a text editor. Ensure that the coordinates look identical to the `ncdump` output (e.g., `8.079550` instead of `8.07955`).
3.  **Coordinate Order:** This script produces `Lon Lat Value`. Ensure your LPJ-GUESS instruction file (`.ins`) is configured to read in that specific order.
4.  **Internal Search:** LPJ-GUESS performs an exact binary match. This script uses `double` precision for coordinates in the NetCDF to ensure the internal C++ lookup succeeds.

## Requirements
* **R Packages:** `dplyr`, `tidyr`, `readr`, `ncdf4`, `lubridate`.
* **Input Directory:** Expects a folder named `MeteoSwiss_station/` containing the source CSV.