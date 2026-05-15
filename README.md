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
  username        = "myuser",
  password        = "mypass",
  filter_word     = "Houbara",          # keyword filter on device name; "" = all devices
  tag_numbers     = NULL,               # specific S/N list, e.g. c("216417", "223083")
  from_dt         = "2026-01-01 00:00", # start date (UTC); default = 7 days ago
  to_dt           = "2026-05-12 23:59", # end date (UTC);   default = now
  format_code     = 3,                  # data format (see table below)
  save_csv        = TRUE,               # save a CSV file to output_dir?
  output_dir      = "glosendas_data",   # folder for CSV (only used if save_csv = TRUE)
  drop_empty_cols = TRUE,               # drop columns that are entirely NA?
  verbose         = TRUE                # print progress messages?
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

### `analyze_acc()`

Analyses accelerometer (ACC) bursts in the data frame returned by `glosendas_download()` and attaches per-burst statistics to the GPS row immediately preceding each burst. Handles any sampling frequency (5 Hz, 10 Hz, 20 Hz, 50 Hz, etc.) automatically.

```r
# Basic usage (mean, SD, ODBA per burst — default)
gps_df <- analyze_acc(df)

# Advanced metrics (adds range, quantiles, skewness, kurtosis,
# covariance, correlation, axis differences, mean amplitude)
# Requires: install.packages("moments")
gps_df <- analyze_acc(df, advanced = TRUE)

# Keep all original rows (raw ACC rows not removed)
full_df <- analyze_acc(df, include_burst_rows = TRUE)

# Wider GPS matching window (default is 10 seconds)
gps_df <- analyze_acc(df, gps_window_sec = 30)
```

**How it works:**
- Detects burst boundaries from `SEN_ACC_<N>Hz_START` / `SEN_ACC_<N>Hz_END` rows
- Includes both START and END rows in calculations (both contain valid readings)
- Uses `UTC_date` + `UTC_time` + `milliseconds` for sub-second duration accuracy
- Attaches burst stats to the preceding GPS row (within `gps_window_sec`)
- If no GPS row is nearby, inserts a new `ACC_SUMMARY` row instead
- Handles truncated bursts (no END marker) gracefully

**Basic columns added (always):**

| Column | Description |
|--------|-------------|
| `acc_burst_n` | Number of ACC readings in the burst |
| `acc_freq_hz` | Sampling frequency (Hz) |
| `acc_duration_sec` | Burst duration in seconds |
| `acc_x_mean` / `acc_x_sd` | Mean and SD — X axis |
| `acc_y_mean` / `acc_y_sd` | Mean and SD — Y axis |
| `acc_z_mean` / `acc_z_sd` | Mean and SD — Z axis |
| `acc_odba` | Overall Dynamic Body Acceleration |
| `acc_burst_type` | e.g. `"SEN_ACC_10Hz"` |

**Advanced columns added (`advanced = TRUE`):**
range, min, max, L2-norm, Q25/Q50/Q75, skewness, kurtosis (per axis);
covariance and correlation between axis pairs (XY, XZ, YZ);
mean and SD of axis differences; mean amplitude (mean |Δ|) per axis.

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
- `drop_empty_cols = TRUE` (default) removes columns where every value is `NA` (e.g. `depth_m`, `conductivity_mS/cm`) — set to `FALSE` to keep all portal columns.
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

---

### `analyze_acc()`

Processes ACC bursts in the data frame returned by `glosendas_download()` and attaches per-burst summary statistics to the corresponding GPS row.

```r
# Download data then analyse ACC bursts
df     <- glosendas_download("myuser", "mypass", filter_word = "Houbara")
gps_df <- analyze_acc(df)
```

**What it does:**
- Detects all ACC bursts automatically (any frequency — 5Hz, 10Hz, or other)
- Includes both START and END rows in calculations (they contain valid readings)
- Attaches burst statistics to the GPS row immediately preceding the burst (within `gps_window_sec`, default 10 s)
- If no GPS row is found nearby, inserts a new `ACC_SUMMARY` row instead
- Handles truncated bursts (no END marker) gracefully

**New columns added to the GPS row:**

| Column | Description |
|--------|-------------|
| `acc_burst_n` | Number of ACC readings in the burst |
| `acc_freq_hz` | Sampling frequency (Hz) |
| `acc_duration_sec` | Burst duration in seconds |
| `acc_x_mean` / `acc_x_sd` | Mean and SD of X axis |
| `acc_y_mean` / `acc_y_sd` | Mean and SD of Y axis |
| `acc_z_mean` / `acc_z_sd` | Mean and SD of Z axis |
| `acc_odba` | Overall Dynamic Body Acceleration |
| `acc_burst_type` | Burst type string (e.g. `SEN_ACC_10Hz`) |

```r
# Default: remove raw ACC rows, keep only GPS+summary rows
gps_df <- analyze_acc(df)

# Keep all original rows (raw ACC rows retained)
full_df <- analyze_acc(df, include_burst_rows = TRUE)

# Wider GPS matching window (default is 10 seconds)
gps_df <- analyze_acc(df, gps_window_sec = 30)
```
