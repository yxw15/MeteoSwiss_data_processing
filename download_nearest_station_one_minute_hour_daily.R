setwd("/dss/dssfs02/lwp-dss-0001/pr48va/pr48va-dss-0000/yixuan/LPJ_GUESS_HYD")

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(lubridate)
library(httr2)

# ============================================================
# OUTPUT DIRECTORY
# ============================================================

out_dir <- "MeteoSwiss_variable"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 1) USER VARIABLES (LPJ-GUESS FORCING SET)
# ============================================================

params_by_res <- list(
  "10min" = c(
    "Global radiation; ten minutes mean"
  ),
  "hourly" = c(
    "Air temperature 2 m above ground; hourly mean",
    "Relative air humidity 2 m above ground; hourly mean",
    "Global radiation; hourly mean",
    "Wind speed scalar; hourly mean in m/s"
  ),
  "daily" = c(
    "Global radiation; daily mean",
    "Precipitation; daily total 0 UTC - 0 UTC",
    "Air temperature 2 m above ground; daily mean",
    "Relative air humidity 2 m above ground; daily mean",
    "Wind speed scalar; daily mean in m/s"
  )
)

all_wanted <- unique(unlist(params_by_res))

# ============================================================
# 2) STAC FETCH
# ============================================================

fetch_json <- function(url) {
  request(url) %>%
    req_user_agent("R MeteoSwiss") %>%
    req_perform() %>%
    resp_body_json(simplifyVector = FALSE)
}

stac_url <- "https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn/items"

all_features <- list()
next_url <- paste0(stac_url, "?limit=100")

repeat {
  
  cat("Fetching STAC...\n")
  
  res <- fetch_json(next_url)
  all_features <- c(all_features, res$features)
  
  next_link <- NULL
  for (l in res$links) {
    if (l$rel == "next") next_link <- l$href
  }
  
  if (is.null(next_link)) break
  next_url <- next_link
}

cat("Total STAC items:", length(all_features), "\n")

# ============================================================
# 3) EXTRACT FILES
# ============================================================

assets_tbl <- map_dfr(all_features, function(f) {
  if (is.null(f$assets)) return(NULL)
  
  tibble(
    href = map_chr(f$assets, ~.x$href)
  )
}) %>%
  mutate(href = tolower(href)) %>%
  distinct()

data_files <- assets_tbl %>%
  filter(str_detect(href, "ogd-smn_rue_")) %>%
  filter(str_detect(href, "\\.csv$")) %>%
  filter(str_detect(href, "historical|recent|now|_h_|_d_|_t_")) %>%
  distinct(href) %>%
  pull(href)

cat("Data files found:", length(data_files), "\n")

# ============================================================
# 4) DOWNLOAD ALL FILES
# ============================================================

read_one <- function(url) {
  
  cat("Downloading:", url, "\n")
  
  tryCatch({
    
    read_delim(url, delim = ";", show_col_types = FALSE) %>%
      mutate(source = url)
    
  }, error = function(e) {
    cat("FAILED:", url, "\n")
    NULL
  })
}

data_all <- map(data_files, read_one) %>%
  compact() %>%
  bind_rows()

cat("Total rows:", nrow(data_all), "\n")

# ============================================================
# 5) RESOLUTION TAGGING
# ============================================================

data_all <- data_all %>%
  mutate(
    resolution = case_when(
      str_detect(source, "_t_") ~ "10min",
      str_detect(source, "_h_") ~ "hourly",
      str_detect(source, "_d_") ~ "daily",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# 6) TIME PARSING (RUE FORMAT)
# ============================================================

data_all <- data_all %>%
  rename(datetime_raw = reference_timestamp) %>%
  mutate(
    datetime = dmy_hm(datetime_raw, tz = "UTC")
  )

cat("Missing datetime:", sum(is.na(data_all$datetime)), "\n")

# ============================================================
# 7) PARAMETER METADATA
# ============================================================

params <- read_delim(
  "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_parameters.csv",
  delim = ";",
  show_col_types = FALSE
)

# ============================================================
# 8) LONG FORMAT
# ============================================================

long_data <- data_all %>%
  pivot_longer(
    cols = -c(station_abbr, datetime_raw, datetime, resolution, source),
    names_to = "parameter_shortname",
    values_to = "value"
  ) %>%
  left_join(
    params %>%
      select(parameter_shortname, parameter_description_en),
    by = "parameter_shortname"
  )

# ============================================================
# 9) FILTER ONLY YOUR VARIABLES
# ============================================================

long_data <- long_data %>%
  filter(parameter_description_en %in% all_wanted)

# ============================================================
# 10) EXPORT PER VARIABLE × RESOLUTION
# ============================================================

for (r in names(params_by_res)) {
  
  cat("\n====================\n")
  cat("RESOLUTION:", r, "\n")
  cat("====================\n")
  
  df_r <- long_data %>%
    filter(resolution == r)
  
  for (v in params_by_res[[r]]) {
    
    df_v <- df_r %>%
      filter(parameter_description_en == v) %>%
      select(datetime, station_abbr, value)
    
    if (nrow(df_v) == 0) {
      cat("Skipping:", v, "\n")
      next
    }
    
    fname <- paste0(
      str_replace_all(v, "[ ;/\\\\]", "_") %>% str_to_lower(),
      "_",
      r,
      ".csv"
    )
    
    write_csv(df_v, file.path(out_dir, fname))
    
    cat("Saved:", fname, "\n")
  }
}

# ============================================================
# DONE
# ============================================================

cat("\n=========================================\n")
cat("DONE: MeteoSwiss RUE extraction complete\n")
cat("Output folder:", out_dir, "\n")
cat("=========================================\n")

