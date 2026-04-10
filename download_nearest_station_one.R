setwd("/dss/dsshome1/0D/ge35qej2")

library(readr)
library(dplyr)
library(geosphere)
library(httr2)
library(purrr)
library(stringr)
library(tidyr)
library(lubridate)

# ============================================================
# 1) User settings
# ============================================================
target_lat <- 47.439
target_lon <- 7.776
target_alt <- 500

# Define date range
define_start_date <- "1991-01-01"
define_end_date <- "2025-12-31"

wanted_desc <- c(
  "Global radiation; daily mean",
  "Precipitation; daily total 0 UTC - 0 UTC",
  "Air temperature 2 m above ground; daily mean",
  "Air temperature 2 m above ground; daily maximum",
  "Air temperature 2 m above ground; daily minimum",
  "Relative air humidity 2 m above ground; daily mean",
  "Wind speed scalar; daily mean in m/s"
)

stations_url   <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_stations.csv"
params_url     <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_parameters.csv"
inventory_url  <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_datainventory.csv"
stac_items_url <- "https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn/items"

out_dir <- "MeteoSwiss_station"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ============================================================
# 2) Helpers
# ============================================================
`%||%` <- function(x, y) if (is.null(x)) y else x

read_csv_semicolon <- function(x) {
  read_delim(x, delim = ";", show_col_types = FALSE, progress = FALSE,
             locale = locale(encoding = "Windows-1252"))
}

detect_date_col <- function(df) {
  candidate_date_cols <- c("date", "reference_timestamp", "timestamp", "datetime")
  hit <- candidate_date_cols[candidate_date_cols %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

sanitize_station_df <- function(df) {
  if (is.null(df)) return(NULL)
  names(df) <- gsub("'", "", names(df), fixed = TRUE)
  df
}

fetch_json <- function(url) {
  request(url) %>%
    req_user_agent("R MeteoSwiss STAC downloader") %>%
    req_perform() %>%
    resp_body_json(simplifyVector = FALSE)
}

download_csv_from_href <- function(url) {
  cat("  Downloading:", basename(url), "\n")
  tryCatch({
    resp <- request(url) %>%
      req_user_agent("R MeteoSwiss STAC downloader") %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
    
    if (resp_status(resp) != 200) stop("HTTP ", resp_status(resp))
    
    tf <- tempfile(fileext = ".csv")
    writeBin(resp_body_raw(resp), tf)
    sanitize_station_df(read_csv_semicolon(tf))
  }, error = function(e) {
    message("  Failed: ", conditionMessage(e))
    NULL
  })
}

fetch_all_stac_items <- function(base_url, limit = 100) {
  all_features <- list()
  next_url <- paste0(base_url, "?limit=", limit)
  
  repeat {
    cat("Fetching STAC page...\n")
    page <- fetch_json(next_url)
    all_features <- c(all_features, page$features)
    
    next_link <- NULL
    if (!is.null(page$links)) {
      for (lnk in page$links) {
        if (!is.null(lnk$rel) && identical(lnk$rel, "next")) {
          next_link <- lnk$href
          break
        }
      }
    }
    if (is.null(next_link)) break
    next_url <- next_link
  }
  all_features
}

extract_assets <- function(feature) {
  if (is.null(feature$assets) || length(feature$assets) == 0) {
    return(tibble(item_id = character(), asset_name = character(), href = character()))
  }
  item_id <- feature$id %||% NA_character_
  map_dfr(names(feature$assets), function(nm) {
    href <- feature$assets[[nm]]$href %||% NA_character_
    tibble(item_id = item_id, asset_name = nm, href = href)
  })
}

# ============================================================
# 3) Download and process station data
# ============================================================
process_station_data <- function(station_abbr, assets_tbl, wanted_params, 
                                 wanted_shortnames, rename_df, date_start = NULL, date_end = NULL) {
  
  cat("\nProcessing station:", station_abbr, "\n")
  
  # Find station assets
  station_assets <- assets_tbl %>%
    filter(str_detect(item_id_lower, str_to_lower(station_abbr)) |
             str_detect(asset_name_lower, str_to_lower(station_abbr)) |
             str_detect(href_lower, paste0("/", str_to_lower(station_abbr), "/")))
  
  if(nrow(station_assets) == 0) {
    cat("  No assets found for station\n")
    return(NULL)
  }
  
  # Get daily assets
  daily_assets <- station_assets %>%
    filter(str_detect(item_id_lower, "daily|_d_|\\bd\\b") |
             str_detect(asset_name_lower, "daily|_d_|\\bd\\b") |
             str_detect(href_lower, "daily|_d_|\\bd\\b"))
  
  # Get hrefs
  recent_href <- daily_assets %>%
    filter(str_detect(asset_name_lower, "recent") | str_detect(href_lower, "recent")) %>%
    slice(1) %>% pull(href)
  
  historical_href <- daily_assets %>%
    filter(str_detect(asset_name_lower, "historical") | str_detect(href_lower, "historical")) %>%
    slice(1) %>% pull(href)
  
  if(length(recent_href) == 0) recent_href <- NA_character_
  if(length(historical_href) == 0) historical_href <- NA_character_
  
  # Download data
  daily_recent <- if (!is.na(recent_href)) download_csv_from_href(recent_href) else NULL
  daily_hist <- if (!is.na(historical_href)) download_csv_from_href(historical_href) else NULL
  
  if(is.null(daily_recent) && is.null(daily_hist)) {
    cat("  No data downloaded\n")
    return(NULL)
  }
  
  # Combine and process
  daily_raw <- bind_rows(daily_hist, daily_recent) %>% distinct()
  
  # Select requested variables
  date_col <- detect_date_col(daily_raw)
  meta_cols <- c("station_abbr", "station_name", date_col)
  meta_cols <- meta_cols[meta_cols %in% names(daily_raw)]
  keep_cols <- unique(c(meta_cols, wanted_shortnames))
  
  daily_selected <- daily_raw %>% select(any_of(keep_cols))
  
  if (!is.na(date_col) && date_col %in% names(daily_selected) && date_col != "date") {
    daily_selected <- daily_selected %>% rename(date = all_of(date_col))
  }
  
  # Rename columns
  for (i in seq_len(nrow(rename_df))) {
    old <- rename_df$parameter_shortname[i]
    new <- rename_df$parameter_description_en[i]
    if (old %in% names(daily_selected)) {
      names(daily_selected)[names(daily_selected) == old] <- new
    }
  }
  
  # Unit conversions
  if ("Wind speed scalar; daily mean in km/h" %in% names(daily_selected)) {
    daily_selected <- daily_selected %>%
      mutate(`Wind speed scalar; daily mean in m/s` = `Wind speed scalar; daily mean in km/h` / 3.6)
  }
  
  # Convert temperature to Kelvin
  temp_cols <- grep("Air temperature", names(daily_selected), value = TRUE)
  if(length(temp_cols) > 0) {
    daily_selected <- daily_selected %>% mutate(across(all_of(temp_cols), ~ . + 273.15))
  }
  
  # Format dates
  if("date" %in% names(daily_selected)) {
    if(is.character(daily_selected$date)) {
      daily_selected <- daily_selected %>%
        mutate(date = as.POSIXct(date, format = "%d.%m.%Y %H:%M", tz = "UTC"))
      if(any(is.na(daily_selected$date))) {
        daily_selected <- daily_selected %>%
          mutate(date = as.POSIXct(date, format = "%d.%m.%Y", tz = "UTC"))
      }
    }
    daily_selected <- daily_selected %>%
      mutate(date_day = as.Date(date)) %>%
      arrange(date)
  }
  
  # Filter by date range if specified
  if(!is.null(date_start) && !is.null(date_end) && "date_day" %in% names(daily_selected)) {
    daily_selected <- daily_selected %>%
      filter(date_day >= as.Date(date_start) & date_day <= as.Date(date_end))
  }
  
  cat("  Processed:", nrow(daily_selected), "rows,", 
      min(daily_selected$date_day, na.rm = TRUE), "to",
      max(daily_selected$date_day, na.rm = TRUE), "\n")
  
  return(daily_selected)
}

# ============================================================
# 4) Fill missing values using multiple backup stations
# ============================================================
fill_missing_from_backups <- function(primary_data, backup_data_list, backup_names, 
                                      backup_stations_info, measure_vars) {
  
  # Initialize filled data with primary
  filled_data <- primary_data
  total_filled <- 0
  
  # Track filling source for each cell
  fill_source <- matrix(NA_character_, 
                        nrow = nrow(primary_data), 
                        ncol = length(measure_vars))
  colnames(fill_source) <- measure_vars
  rownames(fill_source) <- as.character(primary_data$date_day)
  
  # For each variable
  for(var_idx in seq_along(measure_vars)) {
    var <- measure_vars[var_idx]
    if(!var %in% names(filled_data)) next
    
    # Track which rows are still NA
    still_na <- is.na(filled_data[[var]])
    
    # Try each backup station in order
    for(backup_idx in seq_along(backup_data_list)) {
      backup_data <- backup_data_list[[backup_idx]]
      backup_abbr <- backup_names[backup_idx]
      backup_name <- backup_stations_info$station_name[backup_stations_info$station_abbr == backup_abbr]
      backup_dist <- backup_stations_info$dist_km[backup_stations_info$station_abbr == backup_abbr]
      
      if(length(backup_name) == 0) backup_name <- backup_abbr
      if(length(backup_dist) == 0) backup_dist <- NA
      if(is.null(backup_data) || !var %in% names(backup_data)) next
      
      # Find matching dates where primary is NA and backup has value
      for(row_idx in which(still_na)) {
        date_val <- filled_data$date_day[row_idx]
        backup_row <- which(backup_data$date_day == date_val)
        
        if(length(backup_row) > 0 && !is.na(backup_data[[var]][backup_row])) {
          filled_data[[var]][row_idx] <- backup_data[[var]][backup_row]
          # Store both abbreviation and name with distance
          fill_source[row_idx, var_idx] <- paste0(backup_abbr, "|", backup_name, "|", backup_dist)
          total_filled <- total_filled + 1
          still_na[row_idx] <- FALSE
        }
      }
      
      # Exit if all filled
      if(!any(still_na)) break
    }
  }
  
  # Convert fill_source to data frame for reporting with station_name
  fill_source_df <- as.data.frame(fill_source) %>%
    mutate(date_day = rownames(fill_source)) %>%
    pivot_longer(cols = -date_day, names_to = "variable", values_to = "filled_by") %>%
    filter(!is.na(filled_by)) %>%
    separate(filled_by, into = c("filled_by_station_abbr", "filled_by_station_name", "filled_by_distance_km"), 
             sep = "\\|", extra = "merge") %>%
    mutate(
      filled_by_distance_km = as.numeric(filled_by_distance_km)
    )
  
  # Add filling metadata to filled_data
  filled_data <- filled_data %>%
    mutate(
      n_filled = rowSums(is.na(select(primary_data, all_of(measure_vars)))) -
        rowSums(is.na(select(filled_data, all_of(measure_vars)))),
      any_filled = n_filled > 0
    )
  
  attr(filled_data, "total_filled") <- total_filled
  attr(filled_data, "fill_source") <- fill_source_df
  
  return(filled_data)
}

# ============================================================
# 5) Main execution
# ============================================================

cat("\n" , rep("=", 60), "\n")
cat("STEP 1: Reading metadata\n")
cat(rep("=", 60), "\n")

stations <- read_csv_semicolon(stations_url)
params <- read_csv_semicolon(params_url)
inventory <- read_csv_semicolon(inventory_url)

cat("\n", rep("=", 60), "\n")
cat("STEP 2: Finding nearest stations\n")
cat(rep("=", 60), "\n")

# Find nearest stations
nearest_10_stations <- stations %>%
  filter(!is.na(station_coordinates_wgs84_lat), !is.na(station_coordinates_wgs84_lon)) %>%
  mutate(
    dist_m = distHaversine(
      matrix(c(station_coordinates_wgs84_lon, station_coordinates_wgs84_lat), ncol = 2),
      c(target_lon, target_lat)
    ),
    dist_km = dist_m / 1000,
    alt_diff_m = abs(station_height_masl - target_alt)
  ) %>%
  arrange(dist_km, alt_diff_m) %>%
  slice_head(n = 10)

cat("\nNearest 10 stations:\n")
print(nearest_10_stations %>% select(station_abbr, station_name, dist_km, alt_diff_m))

nearest_station_abbr <- nearest_10_stations$station_abbr[1]
cat("\nPrimary station:", nearest_station_abbr, "\n")

# Map descriptions to parameter shortnames
wanted_params <- params %>%
  filter(parameter_description_en %in% wanted_desc) %>%
  select(parameter_shortname, parameter_description_en, parameter_unit)

# Check availability at primary station
station_inventory <- inventory %>% filter(station_abbr == nearest_station_abbr)
available_params <- wanted_params %>%
  inner_join(station_inventory, by = "parameter_shortname") %>%
  distinct(parameter_shortname, .keep_all = TRUE)

wanted_shortnames <- unique(available_params$parameter_shortname)

if(length(wanted_shortnames) == 0) {
  stop("None of the requested parameters are available at the nearest station.")
}

# Create rename mapping
rename_df <- wanted_params %>% filter(parameter_shortname %in% wanted_shortnames)

# Fetch STAC assets
cat("\n", rep("=", 60), "\n")
cat("STEP 3: Fetching STAC assets\n")
cat(rep("=", 60), "\n")

features <- fetch_all_stac_items(stac_items_url)
assets_tbl <- map_dfr(features, extract_assets) %>%
  mutate(
    item_id_lower = str_to_lower(item_id),
    asset_name_lower = str_to_lower(asset_name),
    href_lower = str_to_lower(href)
  )

# Process primary station
cat("\n", rep("=", 60), "\n")
cat("STEP 4: Processing primary station\n")
cat(rep("=", 60), "\n")

primary_data <- process_station_data(
  station_abbr = nearest_station_abbr,
  assets_tbl = assets_tbl,
  wanted_params = wanted_params,
  wanted_shortnames = wanted_shortnames,
  rename_df = rename_df,
  date_start = define_start_date,
  date_end = define_end_date
)

if(is.null(primary_data)) stop("Failed to download primary station data")

# Process backup stations (stations 2-10)
cat("\n", rep("=", 60), "\n")
cat("STEP 5: Processing backup stations\n")
cat(rep("=", 60), "\n")

backup_stations <- nearest_10_stations$station_abbr[2:10]
backup_data_list <- list()
backup_names <- c()

for(i in seq_along(backup_stations)) {
  station_abbr <- backup_stations[i]
  
  # Check if station has required parameters
  station_inv <- inventory %>% filter(station_abbr == station_abbr)
  available <- wanted_params %>%
    inner_join(station_inv, by = "parameter_shortname") %>%
    distinct(parameter_shortname)
  
  if(nrow(available) > 0) {
    cat("\n--- Backup station", i, ":", station_abbr, "---\n")
    data <- process_station_data(
      station_abbr = station_abbr,
      assets_tbl = assets_tbl,
      wanted_params = wanted_params,
      wanted_shortnames = wanted_shortnames,
      rename_df = rename_df,
      date_start = define_start_date,
      date_end = define_end_date
    )
    
    if(!is.null(data)) {
      backup_data_list[[length(backup_data_list) + 1]] <- data
      backup_names <- c(backup_names, station_abbr)
    }
  }
}

cat("\n", rep("=", 60), "\n")
cat("STEP 6: Filling missing values\n")
cat(rep("=", 60), "\n")

# Define measurement variables
measure_vars <- c(
  "Air temperature 2 m above ground; daily mean",
  "Air temperature 2 m above ground; daily maximum",
  "Air temperature 2 m above ground; daily minimum",
  "Global radiation; daily mean",
  "Relative air humidity 2 m above ground; daily mean",
  "Wind speed scalar; daily mean in m/s",
  "Precipitation; daily total 0 UTC - 0 UTC"
)

# Only keep variables that exist in primary data
measure_vars <- measure_vars[measure_vars %in% names(primary_data)]

# Create backup stations info dataframe with names and distances
backup_stations_info <- nearest_10_stations %>%
  filter(station_abbr %in% backup_names) %>%
  select(station_abbr, station_name, dist_km)

# Fill missing values
filled_data <- fill_missing_from_backups(
  primary_data = primary_data,
  backup_data_list = backup_data_list,
  backup_names = backup_names,
  backup_stations_info = backup_stations_info,
  measure_vars = measure_vars
)

# ============================================================
# 6) Results and reporting
# ============================================================

cat("\n", rep("=", 60), "\n")
cat("STEP 7: Results summary\n")
cat(rep("=", 60), "\n")

# Summary statistics
original_na <- sum(is.na(primary_data[, measure_vars]))
final_na <- sum(is.na(filled_data[, measure_vars]))
total_filled <- attr(filled_data, "total_filled")

cat("\n=== Filling Summary ===\n")
cat("Original NAs:", original_na, "\n")
cat("Total NAs filled:", total_filled, "\n")
cat("Remaining NAs:", final_na, "\n")
if(original_na > 0) {
  cat("Percentage filled:", round(total_filled/original_na * 100, 2), "%\n")
}

# Get fill source information (already has station_name)
fill_source <- attr(filled_data, "fill_source")

cat("\n=== Fill Source Summary ===\n")
fill_summary <- fill_source %>%
  group_by(filled_by_station_abbr, filled_by_station_name, variable) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))

print(fill_summary)

# Identify still missing values
still_missing <- filled_data %>%
  filter(any_filled == FALSE) %>%
  select(date_day, n_filled, all_of(measure_vars)) %>%
  filter(rowSums(is.na(select(., all_of(measure_vars)))) > 0)

cat("\n=== Still Missing After All Backups ===\n")
cat("Days with remaining NAs:", nrow(still_missing), "\n")

if(nrow(still_missing) > 0) {
  print(head(still_missing, 20))
}

# ============================================================
# 7) Save outputs
# ============================================================

cat("\n", rep("=", 60), "\n")
cat("STEP 8: Saving outputs\n")
cat(rep("=", 60), "\n")

# Create filename with date range
date_range_filename <- paste0("daily_filled_", 
                              gsub("-", "", define_start_date), 
                              "_to_", 
                              gsub("-", "", define_end_date), 
                              ".csv")

# Save filled data
write_csv(filled_data, file.path(out_dir, date_range_filename))
cat("Saved:", date_range_filename, "\n")

# Save fill source report (includes station_name)
write_csv(fill_source, file.path(out_dir, "fill_source_report.csv"))
cat("Saved: fill_source_report.csv (includes station_name and distance)\n")

# Save fill source summary by station
fill_source_summary <- fill_source %>%
  group_by(filled_by_station_abbr, filled_by_station_name, variable) %>%
  summarise(
    times_used = n(),
    first_date = min(date_day),
    last_date = max(date_day),
    .groups = "drop"
  ) %>%
  arrange(desc(times_used))

write_csv(fill_source_summary, file.path(out_dir, "fill_source_summary_by_station.csv"))
cat("Saved: fill_source_summary_by_station.csv\n")

# Save still missing report
if(nrow(still_missing) > 0) {
  write_csv(still_missing, file.path(out_dir, "still_missing_after_filling.csv"))
  cat("Saved: still_missing_after_filling.csv\n")
}

# Save detailed filling log
filling_log <- filled_data %>%
  filter(any_filled == TRUE) %>%
  select(date_day, n_filled, all_of(measure_vars))

write_csv(filling_log, file.path(out_dir, "filling_log.csv"))
cat("Saved: filling_log.csv\n")

# Summary report
summary_report <- data.frame(
  metric = c(
    "primary_station",
    "primary_station_name",
    "backup_stations_used",
    "date_range",
    "total_days",
    "original_na_count",
    "filled_na_count",
    "remaining_na_count",
    "fill_percentage"
  ),
  value = c(
    nearest_station_abbr,
    nearest_10_stations$station_name[nearest_10_stations$station_abbr == nearest_station_abbr][1],
    paste(backup_names, collapse = ", "),
    paste(min(filled_data$date_day), "to", max(filled_data$date_day)),
    as.character(nrow(filled_data)),
    as.character(original_na),
    as.character(total_filled),
    as.character(final_na),
    if(original_na > 0) paste0(round(total_filled/original_na * 100, 2), "%") else "0%"
  )
)

write_csv(summary_report, file.path(out_dir, "filling_summary.csv"))
cat("Saved: filling_summary.csv\n")

# ============================================================
# 8) Create README.md file
# ============================================================

cat("\n", rep("=", 60), "\n")
cat("STEP 9: Creating README.md\n")
cat(rep("=", 60), "\n")

readme_content <- paste0(
  "# MeteoSwiss Station Data Processing\n\n",
  
  "## Overview\n",
  "This script downloads meteorological data from MeteoSwiss stations, processes daily measurements,\n",
  "and fills missing values using data from nearby backup stations.\n\n",
  
  "## Target Location\n",
  "- **Latitude**: ", target_lat, "\n",
  "- **Longitude**: ", target_lon, "\n",
  "- **Altitude**: ", target_alt, " m\n\n",
  
  "## Date Range Processed\n",
  "- **Start Date**: ", define_start_date, "\n",
  "- **End Date**: ", define_end_date, "\n\n",
  
  "## Primary Station\n",
  "- **Station Abbreviation**: ", nearest_station_abbr, "\n",
  "- **Station Name**: ", nearest_10_stations$station_name[nearest_10_stations$station_abbr == nearest_station_abbr][1], "\n",
  "- **Distance from Target**: ", round(nearest_10_stations$dist_km[nearest_10_stations$station_abbr == nearest_station_abbr][1], 2), " km\n",
  "- **Altitude Difference**: ", nearest_10_stations$alt_diff_m[nearest_10_stations$station_abbr == nearest_station_abbr][1], " m\n\n",
  
  "## Backup Stations Used\n",
  paste0(capture.output(print(
    nearest_10_stations %>% 
      filter(station_abbr %in% backup_names) %>%
      select(Station = station_abbr, Name = station_name, Distance_km = dist_km)
  )), collapse = "\n"), "\n\n",
  
  "## Parameters Processed\n",
  paste0("- ", paste(wanted_desc, collapse = "\n- ")), "\n\n",
  
  "## Data Processing Steps\n",
  "1. **Metadata Download**: Reads station metadata, parameter definitions, and inventory\n",
  "2. **Station Selection**: Finds 10 nearest stations based on geographic coordinates\n",
  "3. **Data Download**: Fetches daily data from STAC API (recent and historical)\n",
  "4. **Data Processing**: \n",
  "   - Converts temperature from °C to Kelvin (+273.15)\n",
  "   - Converts wind speed from km/h to m/s (÷3.6)\n",
  "   - Standardizes date formats\n",
  "   - Filters to specified date range\n",
  "5. **Missing Value Filling**: \n",
  "   - Uses backup stations in order of proximity\n",
  "   - Fills missing values on a per-variable, per-day basis\n",
  "   - Tracks which backup station filled each value\n",
  "6. **Output Generation**: Creates multiple CSV files with results\n\n",
  
  "## Output Files\n\n",
  
  "### Main Data Files\n",
  "- **`", date_range_filename, "`**: Complete filled daily data for the primary station\n",
  "  - Columns: station_abbr, date, date_day, all meteorological parameters, filling metadata\n",
  "  - Temperature in Kelvin, Precipitation in mm, Radiation in W/m², Humidity in %, Wind in m/s\n\n",
  
  "### Filling Reports\n",
  "- **`fill_source_report.csv`**: Detailed record of each filled value\n",
  "  - Shows which backup station provided each value\n",
  "  - Includes station name and distance\n",
  "  - Tracks date, variable, and source station\n\n",
  
  "- **`fill_source_summary_by_station.csv`**: Summary statistics by backup station\n",
  "  - Count of times each backup station was used\n",
  "  - Date range of contributions from each station\n",
  "  - Breakdown by variable type\n\n",
  
  "- **`filling_log.csv`**: Days where filling occurred\n",
  "  - Shows number of variables filled per day\n",
  "  - Includes the filled values\n\n",
  
  "- **`still_missing_after_filling.csv`**: Days with remaining missing data\n",
  "  - Identifies gaps that couldn't be filled by any backup station\n\n",
  
  "### Summary Reports\n",
  "- **`filling_summary.csv`**: Overall statistics\n",
  "  - Primary station information\n",
  "  - Original vs final NA counts\n",
  "  - Fill percentage achieved\n\n",
  
  "## Data Quality Notes\n",
  "- **Temperature**: Converted to Kelvin (K) for thermodynamic consistency\n",
  "- **Wind Speed**: Converted to meters per second (m/s)\n",
  "- **Precipitation**: Daily totals in millimeters (mm)\n",
  "- **Radiation**: Daily mean in Watts per square meter (W/m²)\n",
  "- **Humidity**: Daily mean relative humidity (%)\n\n",
  
  "## Filling Statistics\n",
  paste0(
    "- **Original Missing Values**: ", original_na, "\n",
    "- **Values Filled**: ", total_filled, "\n",
    "- **Remaining Missing**: ", final_na, "\n",
    "- **Fill Percentage**: ", if(original_na > 0) round(total_filled/original_na * 100, 2) else 100, "%\n\n"
  ),
  
  "## Usage Instructions\n\n",
  "### Running the Script\n",
  "```r\n",
  "source(\"meteoswiss_data_processing.R\")\n",
  "```\n\n",
  
  "### Modifying Parameters\n",
  "Edit the user settings section at the top of the script:\n",
  "```r\n",
  "target_lat <- 47.439          # Target latitude\n",
  "target_lon <- 7.776           # Target longitude\n",
  "target_alt <- 500             # Target altitude (m)\n",
  "define_start_date <- \"1991-01-01\"\n",
  "define_end_date <- \"2025-12-31\"\n",
  "```\n\n",
  
  "### Customizing Variables\n",
  "Modify the `wanted_desc` vector to select different meteorological parameters:\n",
  "```r\n",
  "wanted_desc <- c(\n",
  "  \"Global radiation; daily mean\",\n",
  "  \"Precipitation; daily total 0 UTC - 0 UTC\",\n",
  "  # Add or remove parameters as needed\n",
  ")\n",
  "```\n\n",
  
  "## Dependencies\n",
  "Required R packages:\n",
  "- `readr` - CSV reading\n",
  "- `dplyr` - Data manipulation\n",
  "- `geosphere` - Distance calculations\n",
  "- `httr2` - HTTP requests\n",
  "- `purrr` - Functional programming\n",
  "- `stringr` - String manipulation\n",
  "- `tidyr` - Data tidying\n",
  "- `lubridate` - Date handling\n\n",
  
  "## Data Source\n",
  "Data provided by MeteoSwiss (Federal Office of Meteorology and Climatology)\n",
  "- **Stations Metadata**: ", stations_url, "\n",
  "- **Parameters Metadata**: ", params_url, "\n",
  "- **Inventory**: ", inventory_url, "\n",
  "- **STAC API**: ", stac_items_url, "\n\n",
  
  "## License and Attribution\n",
  "This script processes Open Government Data (OGD) from MeteoSwiss.\n",
  "Please cite MeteoSwiss as the data source in any publications or derived products.\n\n",
  
  "## Contact\n",
  "For questions or issues, please refer to the MeteoSwiss data portal documentation.\n\n",
  
  "## Last Updated\n",
  format(Sys.Date(), "%Y-%m-%d")
)

# Write README.md
writeLines(readme_content, file.path("R_scripts/README_download_nearest_station_one.md"))
cat("Saved: README.md\n")

cat("\n", rep("=", 60), "\n")
cat("PROCESS COMPLETED SUCCESSFULLY\n")
cat("Date range processed:", define_start_date, "to", define_end_date, "\n")
cat("Output directory:", out_dir, "\n")
cat(rep("=", 60), "\n")

