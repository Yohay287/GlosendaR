# GlosendaR

An R package for downloading GPS and sensor tracking data from the [Glosendas OrniTrack](https://cpanel.glosendas.net) wildlife tracking portal.

## Features

- **Automatic device discovery** — reads all devices from the portal on every run; new tags are picked up instantly
- **Flexible filtering** — filter by species name keyword, specific tag S/N numbers, or both combined
- **All data formats** — GPS+SENSORS V1/V2, GPS only, sensors only
- **Tidy output** — returns a clean data frame with a `tag_name` column added, `UTC_datetime` parsed as `POSIXct`
- **Optional CSV backup** — choose whether to save a timestamped CSV file; the R data frame is always returned

## Installation

```r
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

Downloads data and returns a data frame. All parameters except `username` and `password` are optional.

```r
df <- glosendas_download(
  username     = "myuser",
  password     = "mypass",
  filter_word  = "Houbara",          # keyword filter on device name; "" = all devices
  tag_numbers  = NULL,               # specific S/N list, e.g. c("216417", "223083")
  from_dt      = "2026-01-01 00:00", # start date (UTC); default = 7 days ago
  to_dt        = "2026-05-12 23:59", # end date (UTC);   default = now
  format_code  = 3,                  # data format (see table below)
  save_csv     = TRUE,               # save a CSV file to output_dir?
  output_dir   = "glosendas_data",   # folder for CSV (only used if save_csv = TRUE)
  verbose      = TRUE                # print progress messages?
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

#### Filtering options

**By name keyword** — downloads all devices whose name contains the word:
```r
df <- glosendas_download("myuser", "mypass", filter_word = "Houbara")
df <- glosendas_download("myuser", "mypass", filter_word = "BE 20")
df <- glosendas_download("myuser", "mypass", filter_word = "")   # all devices
```

**By tag S/N number** — downloads only the listed tags:
```r
df <- glosendas_download(
  username    = "myuser",
  password    = "mypass",
  tag_numbers = c("216417", "223083", "227064")
)
```

**Both combined** — only tags that match the keyword AND are in the S/N list:
```r
df <- glosendas_download(
  username    = "myuser",
  password    = "mypass",
  filter_word = "Houbara",
  tag_numbers = c("216417", "223083")
)
```

#### CSV saving

The R data frame is **always** returned. The CSV file is optional:
```r
# No CSV saved — just the R data frame (faster, no disk writes)
df <- glosendas_download("myuser", "mypass",
        filter_word = "Houbara",
        save_csv    = FALSE)

# Save CSV to a custom folder (default behaviour)
df <- glosendas_download("myuser", "mypass",
        filter_word = "Houbara",
        save_csv    = TRUE,
        output_dir  = "C:/MyData/Houbara")
```

---

### `glosendas_list_devices()`

Explore what devices are on the portal without downloading any data. Supports the same filtering options as `glosendas_download()`.

```r
# All devices on the account
all_devices <- glosendas_list_devices("myuser", "mypass")

# Only Houbara devices
houbara <- glosendas_list_devices("myuser", "mypass", filter_word = "Houbara")

# Only Black Eagle devices
eagles <- glosendas_list_devices("myuser", "mypass", filter_word = "BE 20")

# Specific tags by S/N
some <- glosendas_list_devices("myuser", "mypass",
                               tag_numbers = c("216417", "223083", "227064"))
```

Returns a data frame with columns `sn`, `name`, and `imei`.

---

## Working with the Data

```r
# Explore
names(df)
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
  select(UTC_datetime, Latitude, Longitude)
```

## Notes

- Credentials are passed directly to the portal and are never stored by the package.
- The portal uses per-device session tokens; the package handles these automatically.
- Devices with no data in the requested date range are reported in the preview but do not cause errors.
- `tag_numbers` accepts both character (`"216417"`) and numeric (`216417`) vectors.
- `verbose = FALSE` suppresses all progress messages for quiet/batch operation.

## Requirements

R >= 4.0.0 and the following packages (installed automatically):

- `httr`
- `stringr`
- `lubridate`
- `curl`

## License

MIT
