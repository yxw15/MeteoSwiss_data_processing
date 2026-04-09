setwd("/dss/dsshome1/0D/ge35qej2")

# setwd("/dss/dssfs02/lwp-dss-0001/pr48va/pr48va-dss-0000/yixuan/NDVI_PSI_project")

# ============================================================
# STAC download: MeteoSwiss surface- & satellite-derived grids
# Years: 1991-2024 (only those present in the archives)
# ============================================================

library(httr2)
library(purrr)
library(dplyr)
library(stringr)
library(tibble)
library(readr)

# ----------------------------
# 1) Settings
# ----------------------------
# Use v0.9 API (the version that actually works)
stac_root <- "https://data.geo.admin.ch/api/stac/v0.9"

# The two collections of interest
collection_ids <- c(
  "ch.meteoschweiz.ogd-surface-derived-grid",
  "ch.meteoschweiz.ogd-satellite-derived-grid"
)

out_dir <- "/dss/dsshome1/0D/ge35qej2/MeteoSwiss_netcdf"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Years you want (1991-2024)
years <- 1991:2024
year_pattern <- paste0("(", paste(years, collapse = "|"), ")")

ua <- "Mozilla/5.0 (STAC downloader; R httr2)"

# ----------------------------
# 2) Helper functions
# ----------------------------
fetch_json <- function(url) {
  request(url) |>
    req_headers(
      "User-Agent" = ua,
      "Accept" = "application/json"
    ) |>
    req_options(followlocation = TRUE) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
}

extract_asset_hrefs_from_item <- function(stac_item) {
  # stac_item is the parsed JSON of a single STAC Item
  if (is.null(stac_item$assets)) return(character())
  hrefs <- map_chr(stac_item$assets, ~ .x$href %||% NA_character_)
  hrefs <- hrefs[!is.na(hrefs)]
  unname(hrefs)
}

download_one <- function(url, out_dir) {
  fname <- basename(str_split(url, "\\?", n = 2)[[1]][1])
  dest <- file.path(out_dir, fname)
  
  if (file.exists(dest) && file.size(dest) > 0) {
    message("Skip (exists): ", fname)
    return(dest)
  }
  
  message("Downloading: ", fname)
  
  resp <- request(url) |>
    req_headers("User-Agent" = ua, "Accept" = "*/*") |>
    req_options(followlocation = TRUE) |>
    req_perform()
  
  if (resp_status(resp) != 200) stop("HTTP ", resp_status(resp), " for ", url)
  
  writeBin(resp_body_raw(resp), dest)
  dest
}

# ----------------------------
# 3) Loop over collections, get assets, filter by year
# ----------------------------
all_hrefs <- character()

for (col_id in collection_ids) {
  message("\nProcessing collection: ", col_id)
  
  # Each collection has a single item with ID "archive-ch"
  item_url <- paste0(stac_root, "/collections/", col_id, "/items/archive-ch")
  
  item_json <- fetch_json(item_url)
  
  hrefs <- extract_asset_hrefs_from_item(item_json)
  message("  Total assets found: ", length(hrefs))
  
  # Keep only .nc files (already true, but safe)
  hrefs_nc <- hrefs[str_detect(tolower(hrefs), "\\.nc")]
  
  # Filter for years 1991-2024 based on filename
  # The year appears as YYYY in the filename (e.g., ..._19910101000000_...)
  hrefs_filtered <- hrefs_nc[str_detect(hrefs_nc, year_pattern)]
  
  message("  NetCDF assets for years 1991-2024: ", length(hrefs_filtered))
  
  all_hrefs <- c(all_hrefs, hrefs_filtered)
}

# Remove duplicates (if any)
all_hrefs <- unique(all_hrefs)
message("\nTotal unique NetCDF URLs to download: ", length(all_hrefs))

# ----------------------------
# 4) Save URL list and download
# ----------------------------
write_lines(all_hrefs, file.path(out_dir, "urls_netcdf_1991_2024.txt"))

# Download
downloaded_paths <- map(all_hrefs, download_one, out_dir = out_dir)

# Manifest
manifest <- tibble(
  url = all_hrefs,
  file = basename(str_replace(all_hrefs, "\\?.*$", "")),
  downloaded_path = unlist(downloaded_paths)
)

write_csv(manifest, file.path(out_dir, "manifest_netcdf_1991_2024.csv"))

message("\nDone. Download directory: ", out_dir)