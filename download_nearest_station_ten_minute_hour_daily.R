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
# 1) SETTINGS
# ============================================================
target_lat <- 47.439
target_lon <- 7.776

start_date <- as.POSIXct("1991-01-01", tz="UTC")
end_date   <- as.POSIXct("2025-12-31", tz="UTC")

out_dir <- "MeteoSwiss_variable"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

params_by_res <- list(
  "10min" = c(
    "Global radiation; ten minutes mean",
    "Precipitation; ten minutes total",
    "Wind speed scalar; ten minutes mean in m/s"
  ),
  "hourly" = c(
    "Air temperature 2 m above ground; hourly mean",
    "Relative air humidity 2 m above ground; hourly mean"
  ),
  "daily" = c(
    "Global radiation; daily mean",
    "Precipitation; daily total 0 UTC - 0 UTC",
    "Air temperature 2 m above ground; daily mean",
    "Relative air humidity 2 m above ground; daily mean",
    "Wind speed scalar; daily mean in m/s"
  )
)

# ============================================================
# 2) METADATA
# ============================================================
stations <- read_delim(
  "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_stations.csv",
  delim=";", show_col_types = FALSE
)

params <- read_delim(
  "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_parameters.csv",
  delim=";", show_col_types = FALSE
)

# Create a mapping from shortname to descriptive name
param_mapping <- params %>%
  select(parameter_shortname, parameter_description_en) %>%
  distinct()

# ============================================================
# 3) NEAREST STATIONS
# ============================================================
nearest <- stations %>%
  filter(!is.na(station_coordinates_wgs84_lat)) %>%
  mutate(
    dist_km = distHaversine(
      cbind(station_coordinates_wgs84_lon, station_coordinates_wgs84_lat),
      c(target_lon, target_lat)
    ) / 1000
  ) %>%
  arrange(dist_km) %>%
  slice_head(n = 20)

station_order <- nearest$station_abbr
print(station_order)

# ============================================================
# 4) STAC
# ============================================================
fetch_json <- function(url) {
  request(url) %>%
    req_user_agent("R MeteoSwiss") %>%
    req_perform() %>%
    resp_body_json(simplifyVector = FALSE)
}

fetch_stac <- function() {
  base <- "https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn/items"
  items <- list()
  next_url <- paste0(base, "?limit=100")
  
  repeat {
    cat("STAC...\n")
    res <- fetch_json(next_url)
    items <- c(items, res$features)
    
    next_link <- NULL
    for(l in res$links) {
      if(l$rel == "next") next_link <- l$href
    }
    if(is.null(next_link)) break
    next_url <- next_link
  }
  items
}

extract_assets <- function(features) {
  map_dfr(features, function(f) {
    if(is.null(f$assets)) return(NULL)
    map_dfr(names(f$assets), function(nm) {
      tibble(
        href = tolower(f$assets[[nm]]$href)
      )
    })
  })
}

# ============================================================
# 5) DOWNLOAD
# ============================================================
download_station <- function(station, res, wanted_desc) {
  
  pat <- switch(res,
                "10min" = "_t_",
                "hourly" = "_h_",
                "daily" = "_d_")
  
  urls <- assets_tbl %>%
    filter(str_detect(href, paste0("/", tolower(station), "/"))) %>%
    filter(str_detect(href, pat)) %>%
    pull(href)
  
  if(length(urls) == 0) return(NULL)
  
  dfs <- list()
  
  for(u in urls) {
    tmp <- tryCatch(read_delim(u, delim=";", show_col_types = FALSE), error=function(e) NULL)
    if(!is.null(tmp)) dfs[[length(dfs)+1]] <- tmp
  }
  
  if(length(dfs) == 0) return(NULL)
  
  df <- bind_rows(dfs)
  
  time_col <- names(df)[str_detect(names(df), "date|time")][1]
  names(df)[names(df)==time_col] <- "datetime"
  
  df <- df %>%
    mutate(datetime = suppressWarnings(ymd_hms(datetime, tz="UTC"))) %>%
    filter(datetime >= start_date, datetime <= end_date)
  
  # Get shortnames for the wanted descriptions
  short <- params %>%
    filter(parameter_description_en %in% wanted_desc) %>%
    pull(parameter_shortname)
  
  keep <- intersect(short, names(df))
  if(length(keep) == 0) return(NULL)
  
  # Rename columns from shortnames to descriptive names
  for(col in keep) {
    desc_name <- params %>%
      filter(parameter_shortname == col) %>%
      pull(parameter_description_en) %>%
      first()
    
    if(!is.na(desc_name)) {
      names(df)[names(df) == col] <- desc_name
    }
  }
  
  # Keep only the renamed columns
  keep_desc <- params %>%
    filter(parameter_shortname %in% keep) %>%
    pull(parameter_description_en)
  
  df <- df %>%
    select(datetime, all_of(keep_desc)) %>%
    mutate(station = station, resolution = res)
  
  df
}

# ============================================================
# 6) GAP FILL AND SAVE EACH VARIABLE DIRECTLY
# ============================================================
fill_gaps_and_save <- function(list_df, res, wanted_desc) {
  
  # ensure list is ordered by station priority
  list_df <- list_df[station_order[station_order %in% names(list_df)]]
  
  if(length(list_df) == 0) {
    cat("  No data for resolution:", res, "\n")
    return()
  }
  
  # Process each wanted variable separately
  for(v in wanted_desc) {
    
    cat("  Processing variable:", v, "\n")
    
    # Check if variable exists in base station
    base <- list_df[[1]]
    if(!(v %in% names(base))) {
      cat("    Variable not in nearest station, skipping\n")
      next
    }
    
    # Extract the variable data from base station
    var_data <- base %>%
      select(datetime, station, resolution, value = all_of(v))
    
    # Fill missing values for this variable
    missing_count <- sum(is.na(var_data$value))
    
    if(missing_count > 0) {
      cat("    Missing values:", missing_count, "- attempting to fill\n")
      
      # For each missing value, try to fill from backup stations
      for(i in seq_len(nrow(var_data))) {
        
        if(!is.na(var_data$value[i])) next
        
        t <- var_data$datetime[i]
        
        # try stations in order (skip the first/base station)
        for(j in seq_along(list_df)) {
          if(j == 1) next  # skip base station
          
          if(!(v %in% names(list_df[[j]]))) next
          
          val <- list_df[[j]] %>%
            filter(datetime == t) %>%
            pull(v)
          
          if(length(val) > 0 && !is.na(val[1])) {
            var_data$value[i] <- val[1]
            break
          }
        }
      }
      
      filled_now <- missing_count - sum(is.na(var_data$value))
      cat("    Filled:", filled_now, "values, Remaining:", sum(is.na(var_data$value)), "\n")
    }
    
    # Create clean filename from descriptive name
    fname <- v %>%
      str_replace_all("[ ;/\\\\]", "_") %>%
      str_to_lower()
    
    # Add resolution to filename
    output_file <- file.path(out_dir, paste0(fname, "_", res, ".csv"))
    
    # Save variable data
    write_csv(var_data, output_file)
    cat("    Saved:", basename(output_file), "\n")
  }
}

# ============================================================
# 7) RUN DOWNLOAD AND PROCESS
# ============================================================
cat("\n", rep("=", 60), "\n")
cat("STEP 1: Fetching STAC catalog\n")
cat(rep("=", 60), "\n")

features <- fetch_stac()
assets_tbl <- extract_assets(features)

cat("\n", rep("=", 60), "\n")
cat("STEP 2: Processing each resolution\n")
cat(rep("=", 60), "\n")

for(res in names(params_by_res)) {
  
  cat("\n", rep("=", 60), "\n")
  cat("RESOLUTION:", toupper(res), "\n")
  cat(rep("=", 60), "\n")
  
  station_list <- list()
  wanted_desc <- params_by_res[[res]]
  
  # Download data for all stations at this resolution
  cat("Downloading data for", length(station_order), "stations...\n")
  
  for(st in station_order) {
    cat("  Station:", st, "\n")
    df <- download_station(st, res, wanted_desc)
    if(!is.null(df)) {
      station_list[[st]] <- df
      cat("    Downloaded", nrow(df), "records\n")
    } else {
      cat("    No data available\n")
    }
  }
  
  cat("\nStations with data:", length(station_list), "out of", length(station_order), "\n")
  
  # Process and save variables directly
  if(length(station_list) > 0) {
    fill_gaps_and_save(station_list, res, wanted_desc)
  } else {
    cat("No data available for resolution:", res, "\n")
  }
}

cat("\n", rep("=", 60), "\n")
cat("PROCESS COMPLETED SUCCESSFULLY\n")
cat("Output directory:", out_dir, "\n")
cat(rep("=", 60), "\n")

# List all created files
cat("\nCreated files:\n")
all_files <- list.files(out_dir, pattern = "\\.csv$")
for(f in all_files) {
  file_info <- file.info(file.path(out_dir, f))
  cat("  -", f, "-", format(file_info$size, units = "auto"), "\n")
}