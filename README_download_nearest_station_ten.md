# MeteoSwiss Data Processing - 10 Nearest Stations

## Target Location
- Latitude: 47.439
- Longitude: 7.776
- Altitude: 500 m

## Date Range
- 1991-01-01 to 2025-12-31
- Total days: 12784

## Nearest Stations
| Rank | Station | Distance (km) | Alt Diff (m) |
|------|---------|---------------|--------------|
| 1 | RUE | 7.8 | 111 |
| 2 | STC | 16.22 | 7 |
| 3 | MOE | 16.69 | 157 |
| 4 | GOE | 17.13 | 120 |
| 5 | BAS | 18.41 | 184 |
| 6 | WYN | 20.5 | 78 |
| 7 | BUS | 23.66 | 113 |
| 8 | DEM | 33.57 | 61 |
| 9 | EGO | 33.66 | 22 |
| 10 | PSI | 35.61 | 166 |


## Variables
- Global radiation; daily mean
- Precipitation; daily total 0 UTC - 0 UTC
- Air temperature 2 m above ground; daily mean
- Relative air humidity 2 m above ground; daily mean
- Wind speed scalar; daily mean in m/s

## Unit Conversions
- Temperature: °C → Kelvin (+273.15)
- Wind speed: km/h → m/s

## Output Files
- **all_10_stations_filled_19910101_to_20251231.csv**
- all_stations_fill_source_report.csv
- all_stations_filling_summary.csv
- filtered dataset
- individual station files

## Filling Summary

### Overall
| Metric | Value |
|--------|-------|
| Original NA | 137743 |
| Filled | 137743 |
| Remaining | 0 |
| Fill % | 100% |

### Per Station
| Station | Dist (km) | Days | NA (orig) | Filled | Fill % |
|---------|-----------|------|------------|--------|--------|
| RUE | 7.8 | 12784 | 9 | 9 | 100 |
| STC | 16.22 | 12186 | 17609 | 17609 | 100 |
| MOE | 16.69 | 12784 | 29990 | 29990 | 100 |
| GOE | 17.13 | 12784 | 12824 | 12824 | 100 |
| BAS | 18.41 | 12784 | 23 | 23 | 100 |
| WYN | 20.5 | 12784 | 32 | 32 | 100 |
| BUS | 23.66 | 12784 | 19 | 19 | 100 |
| DEM | 33.57 | 12782 | 15979 | 15979 | 100 |
| EGO | 33.66 | 12784 | 30119 | 30119 | 100 |
| PSI | 35.61 | 12405 | 31139 | 31139 | 100 |


## Method
Missing values are filled using nearby stations in order of distance.

## Limitations
- No temporal interpolation
- No elevation correction
- First-available station used (no weighting)

## Data Source
MeteoSwiss Open Government Data

---
Generated: 2026-05-04 13:07:43

