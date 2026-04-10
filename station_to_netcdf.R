setwd("/dss/dsshome1/0D/ge35qej2")

# Load required libraries
library(ncdf4)
library(dplyr)
library(tidyr)
library(lubridate)

# Read the data
daily_filled <- read.csv("MeteoSwiss_station/all_10_stations_filled_19910101_to_20251231.csv")
daily_filled_report <- read.csv("MeteoSwiss_station/all_stations_fill_source_report.csv")

# Set parameters
target_lat <- 47.439
target_lon <- 7.776
soil_type <- 6  # Soil type number

# Define the mapping between filled_data columns and NetCDF files
variable_mapping <- list(
  temp = list(
    file = "MeteoSwiss_station_to_netcdf/temp_mean_1991_2024.nc",
    variable_name = "temp",
    data_column = "Air.temperature.2.m.above.ground..daily.mean",
    units = "K",
    long_name = "air_temperature"
  ),
  min_temp = list(
    file = "MeteoSwiss_station_to_netcdf/temp_min_1991_2024.nc",
    variable_name = "min_temp",
    data_column = "Air.temperature.2.m.above.ground..daily.minimum",
    units = "K",
    long_name = "air_temperature_min"
  ),
  max_temp = list(
    file = "MeteoSwiss_station_to_netcdf/temp_max_1991_2024.nc",
    variable_name = "max_temp",
    data_column = "Air.temperature.2.m.above.ground..daily.maximum",
    units = "K",
    long_name = "air_temperature_max"
  ),
  prec = list(
    file = "MeteoSwiss_station_to_netcdf/prec_1991_2024.nc",
    variable_name = "prec",
    data_column = "Precipitation; daily total 0 UTC - 0 UTC",
    units = "kg m-2",
    long_name = "precipitation_amount"
  ),
  insol = list(
    file = "MeteoSwiss_station_to_netcdf/insol_1991_2024.nc",
    variable_name = "insol",
    data_column = "Global.radiation..daily.mean",
    units = "W m-2",
    long_name = "surface_downwelling_shortwave_flux"
  ),
  wind = list(
    file = "MeteoSwiss_station_to_netcdf/wind_1991_2024.nc",
    variable_name = "wind_speed",
    data_column = "Wind.speed.scalar..daily.mean.in.m.s",
    units = "m s-1",
    long_name = "wind_speed"
  ),
  relhum = list(
    file = "MeteoSwiss_station_to_netcdf/relhum_1991_2024.nc",
    variable_name = "rhum",
    data_column = "Relative.air.humidity.2.m.above.ground..daily.mean",
    units = "1",
    long_name = "relative_humidity"
  ),
  precip = list(
    file = "MeteoSwiss_station_to_netcdf/precip_1991_2024.nc",
    variable_name = "rhum",
    data_column = "Precipitation..daily.total.0.UTC...0.UTC",
    units = "kg m-2",
    long_name = "precipitation_amount"
  )
)

# Function to create NetCDF file for a variable
create_netcdf <- function(data, var_info, date_column = "date") {
  
  # Parse dates
  dates <- as.Date(data[[date_column]])
  time_vals <- as.numeric(difftime(dates, as.Date("1991-01-01"), units = "days"))
  
  # Define dimensions
  time_dim <- ncdim_def(
    name = "time",
    units = "days since 1991-01-01",
    vals = time_vals,
    calendar = "standard",
    unlim = TRUE
  )
  
  lat_dim <- ncdim_def(
    name = "latitude",
    units = "degrees_north",
    vals = target_lat,
    longname = "latitude"
  )
  
  lon_dim <- ncdim_def(
    name = "longitude",
    units = "degrees_east",
    vals = target_lon,
    longname = "longitude"
  )
  
  # Define variable
  var_def <- ncvar_def(
    name = var_info$variable_name,
    units = var_info$units,
    dim = list(time_dim, lat_dim, lon_dim),
    missval = -999.0,
    longname = var_info$long_name,
    prec = "double"
  )
  
  # Create NetCDF file
  nc_file <- nc_create(var_info$file, list(var_def), force_v4 = TRUE)
  
  # Add global attributes
  ncatt_put(nc_file, 0, "title", paste("MeteoSwiss", var_info$long_name, "data"))
  ncatt_put(nc_file, 0, "institution", "MeteoSwiss")
  ncatt_put(nc_file, 0, "source", "Daily filled station data")
  ncatt_put(nc_file, 0, "station_latitude", target_lat)
  ncatt_put(nc_file, 0, "station_longitude", target_lon)
  ncatt_put(nc_file, 0, "conventions", "CF-1.6")
  
  # Write data
  # Reshape data to [time, lat, lon] dimensions
  data_values <- data[[var_info$data_column]]
  data_array <- array(data_values, dim = c(length(time_vals), 1, 1))
  
  ncvar_put(nc_file, var_def, data_array)
  
  # Close file
  nc_close(nc_file)
  
  message(paste("Created:", var_info$file))
}

# Create NetCDF files for each variable
for (var_name in names(variable_mapping)) {
  var_info <- variable_mapping[[var_name]]
  
  # Check if the column exists in the dataframe
  if (var_info$data_column %in% colnames(daily_filled)) {
    message(paste("Processing:", var_info$data_column))
    create_netcdf(daily_filled, var_info)
  } else {
    warning(paste("Column", var_info$data_column, "not found in dataframe"))
  }
}

message("All NetCDF files have been created successfully!")

# Create gridlist_SCCII.txt
# Format: longitude latitude (space-separated)
gridlist_content <- paste(target_lon, target_lat)

# Write gridlist_SCCII.txt
writeLines(gridlist_content, "MeteoSwiss_station_to_netcdf/gridlist_SCCII.txt")
message("Created: gridlist_SCCII.txt")

# Create soil_SCCII.dat
# Format: longitude latitude soil_type (space-separated)
soil_content <- paste(target_lon, target_lat, soil_type)

# Write soil_SCCII.dat
writeLines(soil_content, "MeteoSwiss_station_to_netcdf/soil_SCCII.dat")
message("Created: soil_SCCII.dat")

# Display the contents
cat("\nContents of gridlist_SCCII.txt:\n")
cat(gridlist_content, "\n")

cat("\nContents of soil_SCCII.dat:\n")
cat(soil_content, "\n")