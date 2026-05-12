# GlosendaR

An R package for downloading GPS and sensor tracking data from the [Glosendas OrniTrack](https://cpanel.glosendas.net) wildlife tracking portal.

## Features

- **Automatic device discovery** — reads all devices from the portal on every run; new tags are picked up instantly
- **Flexible filtering** — filter by any keyword (species name, site, year, etc.)
- **All data formats** — GPS+SENSORS V1/V2, GPS only, sensors only
- **Tidy output** — returns a clean data frame with a `tag_name` column added, `UTC_datetime` parsed as `POSIXct`
- **CSV backup** — saves a timestamped CSV alongside the data frame

## Installation

```r
# Install from GitHub
# install.packages("remotes")  # if not already installed
remotes::install_github("Yohay287/GlosendaR")
```

## Quick Start

```r
library(GlosendaR)

# Download last 7 days of GPS+SENSORS_V2 data for all Houbara devices
df <- glosendas_download(
  username    = "myuser",
  password    = "mypass",
  filter_word = "Houbara"
)

head(df)
table(df$tag_name)
```

## Main Functions

### `glosendas_download()`

Downloads data and returns a data frame.

```r
df <- glosendas_download(
  username    = "myuser",
  password    = "mypass",
  filter_word = "Houbara",      # filter by name keyword; "" = all devices
  from_dt     = "2026-01-01 00:00",
  to_dt       = "2026-05-12 23:59",
  format_code = 3,              # 3 = GPS+SENSORS_V2 (default)
  output_dir  = "my_data"       # folder for CSV backup
)
```

**`format_code` options:**

| Code | Format |
|------|--------|
| 0 | GPS+SENSORS (V1) |
| 1 | GPS only (V1) |
| 2 | SENSORS only (V1) |
| **3** | **GPS+SENSORS_V2 (default)** |
| 4 | GPS only (V2) |
| 5 | SENSORS only (V2) |

### `glosendas_list_devices()`

Explore what devices are on the portal without downloading data.

```r
# All devices on the account
all_devices <- glosendas_list_devices("myuser", "mypass")

# Only Black Eagle devices
eagles <- glosendas_list_devices("myuser", "mypass", filter_word = "BE 20")
```

## Working with the Data

```r
# After downloading
unique(df$tag_name)
table(df$tag_name)

# Filter to one individual
kelach2 <- subset(df, tag_name == "Houbara Kelach 2")

# All Hazerim birds
hazerim <- subset(df, grepl("Hazerim", tag_name))

# With dplyr
library(dplyr)
df |>
  filter(tag_name == "Houbara Kelach 2") |>
  arrange(UTC_datetime) |>
  select(UTC_datetime, Latitude, Longitude, speed_km_h)
```

## Notes

- Credentials are passed directly to the portal and are never stored by the package.
- The portal uses per-device session tokens; the package handles these automatically.
- Devices with no data in the requested date range are reported in the preview but do not cause errors.
- `verbose = FALSE` suppresses all progress messages if you want quiet operation.

## Requirements

R >= 4.0.0 and the following packages (installed automatically):

- `httr`
- `stringr`
- `lubridate`
- `curl`

## License

MIT
