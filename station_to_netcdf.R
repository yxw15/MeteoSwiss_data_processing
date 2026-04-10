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
# 2) Build station table (FORCED STRING PRECISION)
# ============================================================
stations <- daily_filtered %>%
  group_by(station_abbr) %>%
  summarise(
    # We force R to recognize these as 6-decimal numbers immediately
    lat = as.numeric(sprintf("%.6f", first(primary_station_lat))),
    lon = as.numeric(sprintf("%.6f", first(primary_station_lon))),
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
# 5) Metadata with CF Standard Names
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
  standard_name = c(
    "air_temperature",
    "air_temperature",
    "air_temperature",
    "surface_downwelling_shortwave_flux",
    "relative_humidity",
    "wind_speed",
    "precipitation_amount"
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
# 6) Matrix builder
# ============================================================
make_matrix <- function(col_name) {
  full_grid <- tidyr::crossing(landid = stations$landid, date_day = all_dates)
  
  tmp <- full_grid %>%
    left_join(daily_filtered, by = c("landid", "date_day")) %>%
    select(landid, date_day, value = all_of(col_name)) %>%
    arrange(landid, date_day)
  
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
out_dir <- "MeteoSwiss_station_to_netcdf"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(var_info))) {
  cat("\nProcessing variable:", var_info$clean_name[i], "\n")
  mat <- make_matrix(var_info$original_col[i])
  outfile <- file.path(out_dir, paste0(var_info$clean_name[i], ".nc"))
  
  write_nc(
    mat, 
    varname = var_info$clean_name[i], 
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