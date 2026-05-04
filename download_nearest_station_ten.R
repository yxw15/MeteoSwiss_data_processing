setwd("/home/yixuan/Documents/Manuscript3/Data")

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
    ) %>%
    # Keep only essential columns, remove distance
    select(date_day, variable, 
           filled_by_station_abbr, filled_by_station_name)
  
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
# 5) Main execution - Process all 10 stations
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

# Find nearest stations and include coordinates
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
  slice_head(n = 10) %>%
  select(station_abbr, station_name, station_height_masl, 
         station_coordinates_wgs84_lat, station_coordinates_wgs84_lon,
         dist_km, alt_diff_m)

cat("\nNearest 10 stations:\n")
print(nearest_10_stations)

# Map descriptions to parameter shortnames
wanted_params <- params %>%
  filter(parameter_description_en %in% wanted_desc) %>%
  select(parameter_shortname, parameter_description_en, parameter_unit)

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

# Define measurement variables
measure_vars <- c(
  "Air temperature 2 m above ground; daily mean",
  # "Air temperature 2 m above ground; daily maximum",
  # "Air temperature 2 m above ground; daily minimum",
  "Global radiation; daily mean",
  "Relative air humidity 2 m above ground; daily mean",
  "Wind speed scalar; daily mean in m/s",
  "Precipitation; daily total 0 UTC - 0 UTC"
)

# ============================================================
# Process each station as primary with others as backup
# ============================================================

all_stations_filled <- list()
all_fill_sources <- list()
all_summaries <- list()

for(station_idx in 1:nrow(nearest_10_stations)) {
  
  cat("\n", rep("=", 60), "\n")
  cat("PROCESSING STATION", station_idx, "OF 10:", nearest_10_stations$station_abbr[station_idx], "\n")
  cat(rep("=", 60), "\n")
  
  current_station_abbr <- nearest_10_stations$station_abbr[station_idx]
  current_station_name <- nearest_10_stations$station_name[station_idx]
  current_station_lat <- nearest_10_stations$station_coordinates_wgs84_lat[station_idx]
  current_station_lon <- nearest_10_stations$station_coordinates_wgs84_lon[station_idx]
  current_station_alt <- nearest_10_stations$station_height_masl[station_idx]
  
  # Check availability at current station
  station_inventory <- inventory %>% filter(station_abbr == current_station_abbr)
  available_params <- wanted_params %>%
    inner_join(station_inventory, by = "parameter_shortname") %>%
    distinct(parameter_shortname, .keep_all = TRUE)
  
  wanted_shortnames <- unique(available_params$parameter_shortname)
  
  if(length(wanted_shortnames) == 0) {
    cat("Skipping station", current_station_abbr, "- no requested parameters available\n")
    next
  }
  
  # Create rename mapping
  rename_df <- wanted_params %>% filter(parameter_shortname %in% wanted_shortnames)
  
  # Process primary station (current station)
  cat("\n--- Downloading primary station data ---\n")
  primary_data <- process_station_data(
    station_abbr = current_station_abbr,
    assets_tbl = assets_tbl,
    wanted_params = wanted_params,
    wanted_shortnames = wanted_shortnames,
    rename_df = rename_df,
    date_start = define_start_date,
    date_end = define_end_date
  )
  
  if(is.null(primary_data)) {
    cat("Skipping station", current_station_abbr, "- failed to download data\n")
    next
  }
  
  # Only keep variables that exist in primary data
  station_measure_vars <- measure_vars[measure_vars %in% names(primary_data)]
  
  # Process backup stations (all other stations except current)
  backup_stations_list <- nearest_10_stations$station_abbr[-station_idx]
  backup_data_list <- list()
  backup_names <- c()
  
  cat("\n--- Processing backup stations ---\n")
  for(backup_idx in seq_along(backup_stations_list)) {
    backup_abbr <- backup_stations_list[backup_idx]
    
    # Check if backup station has required parameters
    backup_inv <- inventory %>% filter(station_abbr == backup_abbr)
    backup_available <- wanted_params %>%
      inner_join(backup_inv, by = "parameter_shortname") %>%
      distinct(parameter_shortname)
    
    if(nrow(backup_available) > 0) {
      cat("\n  Backup station", backup_idx, ":", backup_abbr, "\n")
      backup_data <- process_station_data(
        station_abbr = backup_abbr,
        assets_tbl = assets_tbl,
        wanted_params = wanted_params,
        wanted_shortnames = wanted_shortnames,
        rename_df = rename_df,
        date_start = define_start_date,
        date_end = define_end_date
      )
      
      if(!is.null(backup_data)) {
        backup_data_list[[length(backup_data_list) + 1]] <- backup_data
        backup_names <- c(backup_names, backup_abbr)
      }
    }
  }
  
  # Create backup stations info dataframe
  backup_stations_info <- nearest_10_stations %>%
    filter(station_abbr %in% backup_names) %>%
    select(station_abbr, station_name, dist_km)
  
  # Fill missing values
  cat("\n--- Filling missing values for", current_station_abbr, "---\n")
  filled_data <- fill_missing_from_backups(
    primary_data = primary_data,
    backup_data_list = backup_data_list,
    backup_names = backup_names,
    backup_stations_info = backup_stations_info,
    measure_vars = station_measure_vars
  )
  
  # Add metadata to identify the station including coordinates
  filled_data <- filled_data %>%
    mutate(
      primary_station_abbr = current_station_abbr,
      primary_station_name = current_station_name,
      primary_station_lat = current_station_lat,
      primary_station_lon = current_station_lon,
      primary_station_alt_m = current_station_alt,
      primary_station_distance_km = nearest_10_stations$dist_km[station_idx],
      primary_station_alt_diff_m = nearest_10_stations$alt_diff_m[station_idx]
    )
  
  # Store results
  all_stations_filled[[station_idx]] <- filled_data
  
  # Store fill source with station identifier (only abbr and name)
  fill_source <- attr(filled_data, "fill_source")
  if(!is.null(fill_source) && nrow(fill_source) > 0) {
    fill_source <- fill_source %>%
      mutate(
        primary_station_abbr = current_station_abbr,
        primary_station_name = current_station_name
      )
    all_fill_sources[[station_idx]] <- fill_source
  }
  
  # Summary for this station
  original_na <- sum(is.na(primary_data[, station_measure_vars]))
  final_na <- sum(is.na(filled_data[, station_measure_vars]))
  total_filled <- attr(filled_data, "total_filled")
  
  station_summary <- data.frame(
    station_abbr = current_station_abbr,
    station_name = current_station_name,
    latitude = current_station_lat,
    longitude = current_station_lon,
    altitude_m = current_station_alt,
    distance_to_target_km = nearest_10_stations$dist_km[station_idx],
    alt_diff_to_target_m = nearest_10_stations$alt_diff_m[station_idx],
    total_days = nrow(filled_data),
    original_na = original_na,
    filled_na = total_filled,
    remaining_na = final_na,
    fill_percentage = if(original_na > 0) round(total_filled/original_na * 100, 2) else 100
  )
  
  all_summaries[[station_idx]] <- station_summary
  
  cat("\n--- Summary for", current_station_abbr, "---\n")
  cat("  Original NAs:", original_na, "\n")
  cat("  Filled NAs:", total_filled, "\n")
  cat("  Remaining NAs:", final_na, "\n")
  cat("  Fill percentage:", station_summary$fill_percentage, "%\n")
}

# ============================================================
# 6) Combine all results
# ============================================================

cat("\n", rep("=", 60), "\n")
cat("STEP 6: Combining all station results\n")
cat(rep("=", 60), "\n")

# Remove NULL entries
all_stations_filled <- all_stations_filled[!sapply(all_stations_filled, is.null)]
all_fill_sources <- all_fill_sources[!sapply(all_fill_sources, is.null)]
all_summaries <- all_summaries[!sapply(all_summaries, is.null)]

# Combine all filled data into one dataframe
combined_filled_data <- bind_rows(all_stations_filled)

# Combine all fill sources
combined_fill_sources <- bind_rows(all_fill_sources)

# Combine all summaries
combined_summaries <- bind_rows(all_summaries) %>%
  arrange(distance_to_target_km)

# ============================================================
# 7) Save outputs
# ============================================================

cat("\n", rep("=", 60), "\n")
cat("STEP 7: Saving outputs\n")
cat(rep("=", 60), "\n")

# Create filename with date range
date_range_filename <- paste0("all_10_stations_filled_", 
                              gsub("-", "", define_start_date), 
                              "_to_", 
                              gsub("-", "", define_end_date), 
                              ".csv")

# Save combined filled data for all stations
write_csv(combined_filled_data, file.path(out_dir, date_range_filename))
cat("Saved:", date_range_filename, "\n")
cat("  Total rows:", nrow(combined_filled_data), "\n")
cat("  Total stations:", length(unique(combined_filled_data$primary_station_abbr)), "\n")

# Save combined fill source report (simplified - only primary station abbr and name)
if(nrow(combined_fill_sources) > 0) {
  # Ensure only these columns are kept
  combined_fill_sources_simplified <- combined_fill_sources %>%
    select(date_day, variable, 
           filled_by_station_abbr, filled_by_station_name,
           primary_station_abbr, primary_station_name)
  
  write_csv(combined_fill_sources_simplified, file.path(out_dir, "all_stations_fill_source_report.csv"))
  cat("Saved: all_stations_fill_source_report.csv (includes primary station abbr and name only)\n")
}

# Save summary report for all stations
write_csv(combined_summaries, file.path(out_dir, "all_stations_filling_summary.csv"))
cat("Saved: all_stations_filling_summary.csv\n")

# Print summary
cat("\n", rep("=", 60), "\n")
cat("FINAL SUMMARY FOR ALL 10 STATIONS\n")
cat(rep("=", 60), "\n")
print(combined_summaries)

# Save individual station files (optional)
cat("\n--- Saving individual station files ---\n")
for(i in 1:length(all_stations_filled)) {
  station_data <- all_stations_filled[[i]]
  station_abbr <- unique(station_data$primary_station_abbr)[1]
  station_filename <- paste0("station_", station_abbr, "_filled_", 
                             gsub("-", "", define_start_date), 
                             "_to_", 
                             gsub("-", "", define_end_date), 
                             ".csv")
  write_csv(station_data, file.path(out_dir, station_filename))
  cat("Saved:", station_filename, "\n")
}

# Get the maximum row count
max_rows <- combined_filled_data %>%
  group_by(station_abbr) %>%
  summarise(row_count = n(), .groups = "drop") %>%
  pull(row_count) %>%
  max()

# Keep only the complete stations (with max rows)
daily_filtered <- combined_filled_data %>%
  group_by(station_abbr) %>%
  filter(n() == max_rows) %>%
  ungroup()

# Verify the result
daily_filtered %>%
  group_by(station_abbr) %>%
  summarise(row_count = n(), .groups = "drop") %>%
  arrange(desc(row_count))

# Save filtered dataset
write.csv(
  daily_filtered,
  "MeteoSwiss_station/all_filtered_19910101_to_20251231.csv",
  row.names = FALSE
)

# ============================================================
# 8) Create README.md file (Improved)
# ============================================================

cat("\n", rep("=", 60), "\n")
cat("STEP 8: Creating README.md\n")
cat(rep("=", 60), "\n")

# ---- Overall stats ----
total_original_na <- sum(combined_summaries$original_na)
total_filled_na <- sum(combined_summaries$filled_na)
total_remaining_na <- sum(combined_summaries$remaining_na)

overall_fill_percentage <- if(total_original_na > 0) {
  round(total_filled_na / total_original_na * 100, 2)
} else 100

# ---- Stations table ----
stations_table <- paste0(
  "| Rank | Station | Distance (km) | Alt Diff (m) |\n",
  "|------|---------|---------------|--------------|\n",
  paste0(
    "| ", seq_len(nrow(nearest_10_stations)),
    " | ", nearest_10_stations$station_abbr,
    " | ", round(nearest_10_stations$dist_km, 2),
    " | ", round(nearest_10_stations$alt_diff_m, 2),
    " |\n",
    collapse = ""
  )
)

# ---- Per-station summary table (Markdown!) ----
summary_table <- combined_summaries %>%
  select(station_abbr, distance_to_target_km, total_days, 
         original_na, filled_na, fill_percentage) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

summary_md <- paste0(
  "| Station | Dist (km) | Days | NA (orig) | Filled | Fill % |\n",
  "|---------|-----------|------|------------|--------|--------|\n",
  paste0(
    "| ", summary_table$station_abbr,
    " | ", summary_table$distance_to_target_km,
    " | ", summary_table$total_days,
    " | ", summary_table$original_na,
    " | ", summary_table$filled_na,
    " | ", summary_table$fill_percentage,
    " |\n",
    collapse = ""
  )
)

# ---- README content ----
readme_content <- paste0(
  "# MeteoSwiss Data Processing - 10 Nearest Stations

## Target Location
- Latitude: ", target_lat, "
- Longitude: ", target_lon, "
- Altitude: ", target_alt, " m

## Date Range
- ", define_start_date, " to ", define_end_date, "
- Total days: ", as.numeric(as.Date(define_end_date) - as.Date(define_start_date)) + 1, "

## Nearest Stations
", stations_table, "

## Variables
", paste0("- ", wanted_desc, collapse = "\n"), "

## Unit Conversions
- Temperature: °C → Kelvin (+273.15)
- Wind speed: km/h → m/s

## Output Files
- **", date_range_filename, "**
- all_stations_fill_source_report.csv
- all_stations_filling_summary.csv
- filtered dataset
- individual station files

## Filling Summary

### Overall
| Metric | Value |
|--------|-------|
| Original NA | ", total_original_na, " |
| Filled | ", total_filled_na, " |
| Remaining | ", total_remaining_na, " |
| Fill % | ", overall_fill_percentage, "% |

### Per Station
", summary_md, "

## Method
Missing values are filled using nearby stations in order of distance.

## Limitations
- No temporal interpolation
- No elevation correction
- First-available station used (no weighting)

## Data Source
MeteoSwiss Open Government Data

---
Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "
"
)

# ---- Save README ----
readme_path <- file.path(getwd(), "../MeteoSwiss_data_processing/README_download_nearest_station_ten.md")
writeLines(readme_content, readme_path)
cat("Saved:", readme_path, "\n")

# Copy to output folder
writeLines(readme_content, file.path(out_dir, "README.md"))

cat("\nPROCESS COMPLETED SUCCESSFULLY\n")

