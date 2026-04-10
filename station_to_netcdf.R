setwd("/dss/dsshome1/0D/ge35qej2")

library(dplyr)
library(tidyr)
library(readr)
library(ncdf4)
library(lubridate)

# ============================================================
# 1) Load data
# ============================================================
daily_filtered <- read.csv(
  "MeteoSwiss_station/all_filtered_19910101_to_20251231.csv"
)

daily_filtered <- daily_filtered %>%
  mutate(
    date_day = as.Date(date_day),
    time = as.POSIXct(date_day, tz = "UTC")
  )

# ============================================================
# 2) Build station table
# ============================================================
stations <- daily_filtered %>%
  group_by(station_abbr) %>%
  summarise(
    lat = first(primary_station_lat),
    lon = first(primary_station_lon),
    alt = first(primary_station_alt_m),
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
all_dates <- sort(unique(daily_filtered$date_day))
time_num <- as.numeric(difftime(as.POSIXct(all_dates, tz = "UTC"), time_origin, units = "days"))

ntime <- length(all_dates)
nland <- nrow(stations)

# ============================================================
# 5) Metadata & Easy Variable Names
# ============================================================
var_info <- data.frame(
  original_col = c(
    "Air.temperature.2.m.above.ground..daily.mean",
    "Air.temperature.2.m.above.ground..daily.maximum",
    "Air.temperature.2.m.above.ground..daily.minimum",
    "Global.radiation..daily.mean",
    "Relative.air.humidity.2.m.above.ground..daily.mean",
    "Wind.speed.scalar..daily.mean.in.m.s",
    "Precipitation..daily.total.0.UTC...0.UTC"
  ),
  clean_name = c(
    "mean_temperature",
    "max_temperature",
    "min_temperature",
    "radiation",
    "relative_humidity",
    "wind_speed",
    "precipitation"
  ),
  long_name = c(
    "Air temperature daily mean",
    "Air temperature daily maximum",
    "Air temperature daily minimum",
    "Global radiation daily mean",
    "Relative humidity daily mean",
    "Wind speed daily mean",
    "Precipitation daily total"
  ),
  units = c("K", "K", "K", "W m-2", "1", "m s-1", "kg m-2"),
  stringsAsFactors = FALSE
)

# ============================================================
# 6) Matrix builder (Map data to [nland, ntime])
# ============================================================
make_matrix <- function(col_name) {
  full_grid <- tidyr::crossing(landid = stations$landid, date_day = all_dates)
  
  tmp <- full_grid %>%
    left_join(daily_filtered, by = c("landid", "date_day")) %>%
    select(landid, date_day, value = all_of(col_name)) %>%
    arrange(landid, date_day)
  
  # Return matrix with stations as rows, time as columns
  mat <- matrix(tmp$value, nrow = nland, ncol = ntime, byrow = TRUE)
  storage.mode(mat) <- "numeric"
  return(mat)
}

# ============================================================
# 7) NetCDF writer
# ============================================================
write_nc <- function(mat, varname, long_name, units, outfile) {
  
  fillvalue <- -9999
  if (file.exists(outfile)) file.remove(outfile)
  
  # Define Dimensions
  dim_land <- ncdim_def("landid", "station_index", stations$landid)
  dim_time <- ncdim_def("time", "days since 1970-01-01", time_num)
  
  # Define Coordinate Variables
  var_lon <- ncvar_def("lon", "degrees_east", list(dim_land), fillvalue, "longitude", prec="float")
  var_lat <- ncvar_def("lat", "degrees_north", list(dim_land), fillvalue, "latitude", prec="float")
  var_alt <- ncvar_def("altitude", "m", list(dim_land), fillvalue, "altitude", prec="float")
  
  # Define Main Data Variable (Short Name used here)
  var_data <- ncvar_def(varname, units, list(dim_land, dim_time), fillvalue, long_name, prec="float")
  
  # Create File
  nc <- nc_create(outfile, list(var_lon, var_lat, var_alt, var_data))
  
  # Fill Data
  ncvar_put(nc, var_lon, stations$lon)
  ncvar_put(nc, var_lat, stations$lat)
  ncvar_put(nc, var_alt, stations$alt)
  
  mat[is.na(mat)] <- fillvalue
  ncvar_put(nc, var_data, mat)
  
  # Global attributes
  ncatt_put(nc, 0, "title", "MeteoSwiss station forcing")
  ncatt_put(nc, 0, "Conventions", "CF-1.6")
  ncatt_put(nc, 0, "featureType", "timeSeries")
  
  nc_close(nc)
}

# ============================================================
# 8) Processing Loop
# ============================================================
out_dir <- "MeteoSwiss_station_to_netcdf"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(var_info))) {
  orig_col <- var_info$original_col[i]
  short_name <- var_info$clean_name[i]
  
  cat("\nProcessing variable:", short_name, "\n")
  
  mat <- make_matrix(orig_col)
  outfile <- file.path(out_dir, paste0(short_name, ".nc"))
  
  write_nc(
    mat, 
    varname = short_name, 
    long_name = var_info$long_name[i], 
    units = var_info$units[i], 
    outfile = outfile
  )
}

# Save station metadata
write.csv(stations, file.path(out_dir, "stations_used.csv"), row.names = FALSE)

cat("\n✅ DONE. Cleaned NetCDF files are in:", out_dir, "\n")

# ============================================================
# 9) Create gridlist_SCCII.txt
# ============================================================
# Format: landid station_abbr (space separated)
# Ensuring landid matches the dimension in the NetCDF
gridlist_path <- file.path(out_dir, "gridlist_SCCII.txt")

write.table(
  stations %>% select(landid, station_abbr),
  file = gridlist_path,
  sep = " ",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

cat("\nCreated gridlist_SCCII.txt - Land IDs match NetCDF dimensions.")

# ============================================================
# 10) Create soil_SCCII.dat
# ============================================================
# Format: lon lat value (space separated)
# Using raw numeric values to match NetCDF coordinate precision
soil_path <- file.path(out_dir, "soil_SCCII.dat")

soil_data <- stations %>%
  transmute(
    lon = lon,  # Matches ncvar_put(nc, var_lon, stations$lon)
    lat = lat,  # Matches ncvar_put(nc, var_lat, stations$lat)
    val = 6     # Your specified soil type value
  )

write.table(
  soil_data,
  file = soil_path,
  sep = " ",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

cat("\nCreated soil_SCCII.dat - Coordinates match NetCDF variables.\n")

cat("\n✅ SUCCESS: All files are synchronized.\n")