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
  save_csv        = FALSE,              # save a CSV file? (default off)
  output_dir      = "glosendas_data",   # folder for CSV (used when save_csv = TRUE)
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

#### Saving to CSV

CSV saving is **off by default** (`save_csv = FALSE`). When enabled, the file
is written from the clean R dataframe — so no gap rows, ever.

```r
# Save to default folder (glosendas_data/) with auto-generated filename
df <- glosendas_download("myuser", "mypass",
        filter_word = "Houbara",
        save_csv    = TRUE)

# Save to a custom folder and filename
df <- glosendas_download("myuser", "mypass",
        filter_word = "Houbara",
        save_csv    = TRUE,
        output_dir  = "C:/MyData")

# Save after ACC analysis
gps_df <- analyze_acc(df)
glosendas_save(gps_df, output_dir = "C:/MyData")
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




---

### `detect_gps_burst()`

Analyses the GPS fix pattern and automatically detects programmed GPS bursts. Fully vectorised — runs in milliseconds on any dataset size.

```r
# Detect dominant burst size (default: must appear >= 10 times)
info <- detect_gps_burst(df)

# Include truncated bursts (partial sequences from missed fixes)
info <- detect_gps_burst(df, include_truncated = TRUE)

# Lower threshold for shorter datasets
info <- detect_gps_burst(df, min_sequences = 5)

# Pipe directly into collapse_gps_burst()
df_c <- collapse_gps_burst(df, burst_size = info$burst_size)

# Collapse dominant + truncated sizes (apply largest first)
for (sz in rev(info$collapse_sizes)) {
  df_c <- collapse_gps_burst(df_c, burst_size = sz)
}
```

**Parameters:**

| Parameter | Description | Default |
|-----------|-------------|---------|
| `min_sequences` | Min times a run length must appear to be a valid burst | `10` |
| `max_gap_sec` | Max seconds between fixes within the same burst | `2` |
| `include_truncated` | Include shorter (truncated) bursts in `collapse_sizes` | `FALSE` |

**The returned list contains:** `burst_size`, `n_bursts`, `burst_pct`, `interval_summary` (median/mean/SD/range), `collapse_sizes` (ready to pass to `collapse_gps_burst()`), `all_candidates`, `all_run_lengths`, `total_gps_fixes`.

---

### `collapse_gps_burst()`

Identifies GPS bursts — runs of **exactly** `burst_size` consecutive GPS fixes each ≤ `max_gap_sec` seconds apart — and collapses each burst into a single representative row. All other rows (ACC bursts, SENSORS, flight detection sequences of a different length) are left completely untouched.

```r
# Collapse 5-fix bursts using the mean (default)
df_c <- collapse_gps_burst(df, burst_size = 5)

# Use the first or last fix instead
df_c <- collapse_gps_burst(df, burst_size = 5, method = "first")
df_c <- collapse_gps_burst(df, burst_size = 5, method = "last")

# Chain with ACC analysis
df_c   <- collapse_gps_burst(df, burst_size = 5)
gps_df <- analyze_acc(df_c)
```

**Parameters:**

| Parameter | Description | Default |
|-----------|-------------|---------|
| `burst_size` | Exact number of fixes per burst (device setting, constant) | `5` |
| `method` | `"mean"`, `"first"`, or `"last"` | `"mean"` |
| `max_gap_sec` | Max seconds between consecutive fixes in the same burst | `2` |

**Key behaviour:**
- Only collapses runs of **exactly** `burst_size` — longer flight detection sequences are never touched
- `"mean"` rounds each column to the same decimal precision as the raw data
- Datetime columns always use the first fix of the burst (averaging timestamps is meaningless)

---

### `add_event_id()`

Assigns a unique `Event_ID` to each event in the data frame. An event is a group of consecutive rows belonging to the same activity — a GPS fix (possibly a burst), an ACC burst, a flight sequence, or any other multi-row event.

**Event_ID format:** `"<tag_name>_<UTC_timestamp_of_first_row>"`
e.g. `"Houbara Kelach 2_2026-02-07 03:27:55"`

```r
# Add Event_ID column (default)
df_ev <- add_event_id(df_acc)

# Also add Event_count (number of rows per event)
df_ev <- add_event_id(df_acc, add_event_count = TRUE)

# Wider gap threshold
df_ev <- add_event_id(df_acc, max_gap_sec = 60)
```

**Event boundary rules — a new event starts when:**
1. Tag name changes
2. Time gap from the previous row exceeds `max_gap_sec` (default: 30 s)
3. A GPS/GPSF row appears after a non-GPS row (e.g. after `ACC_END`) — this always signals a new fix/event

**Key behaviour:**
- A GPS/GPSF burst immediately followed by an ACC burst = **one** event
- `ACC_END` → `GPSF` = **two** events (the GPSF starts a new event)
- `Event_count` (when enabled) is the same value on every row of the same event — useful for filtering by event size

```r
# Filter to a specific event
one <- df_ev[df_ev$Event_ID == "Houbara Kelach 2_2026-02-07 03:27:55", ]

# Filter to large events (e.g. flights > 60 rows)
df_ev   <- add_event_id(df_acc, add_event_count = TRUE)
flights <- df_ev[df_ev$Event_count > 60, ]
```

| Parameter | Description | Default |
|-----------|-------------|---------|
| `max_gap_sec` | Max seconds between rows in the same event | `30` |
| `add_event_count` | Add `Event_count` column | `FALSE` |


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
- When `save_csv = TRUE`, the `output_dir` folder is created automatically if it does not already exist.
- `from_dt` and `to_dt` are always sent to the portal in UTC. If you pass a string with a timezone label (e.g. `"2023-03-06 18:47 IST"`) or a `POSIXct` object in any timezone, it is automatically converted to UTC. Plain strings without a timezone label (e.g. `"2023-03-06 18:47"`) are treated as UTC directly.
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

Processes ACC bursts in the data frame returned by `glosendas_download()` and attaches per-burst summary statistics to the corresponding GPS fix.

```r
# Download data then analyse ACC bursts
df     <- glosendas_download("myuser", "mypass", filter_word = "Houbara")
gps_df <- analyze_acc(df)
```

**How GPS fixes are matched to ACC bursts:**

A GPS fix is assigned to a burst when **both** conditions are met:
1. The GPS row is **exactly one row before** `ACC_START`
2. The GPS timestamp is within **`adj_gps_max_min` minutes** (default: 2 min) before **or** after the burst start time

A small negative time gap is valid and expected — when GPS and ACC are triggered simultaneously, the GPS precise timestamp can be fractionally later than the burst start due to device acquisition lag.

If no GPS fix matches, the burst is recorded as an `ACC_SUMMARY` row with no GPS coordinates.

**What it does:**
- Detects all ACC bursts automatically (any frequency — 5 Hz, 10 Hz, 20 Hz, 50 Hz, or other)
- Includes both START and END rows in calculations (they contain valid readings)
- Uses `UTC_date` + `UTC_time` + `milliseconds` for sub-second duration accuracy
- GPS rows with ACC stats attached are labelled `GPS_ACC` in the `datatype` column
- Handles truncated bursts (no END marker) gracefully

**Basic columns added (always):**

| Column | Description |
|--------|-------------|
| `acc_burst_n` | Number of ACC readings in the burst |
| `acc_freq_hz` | Sampling frequency (Hz) |
| `acc_duration_sec` | Burst duration in seconds |
| `mean_x` / `sd_x` | Mean and SD — X axis |
| `mean_y` / `sd_y` | Mean and SD — Y axis |
| `mean_z` / `sd_z` | Mean and SD — Z axis |
| `acc_odba` | Overall Dynamic Body Acceleration |
| `acc_burst_type` | Burst type string (e.g. `"SEN_ACC_10Hz"`) |
| `gps_to_burst_sec` | Signed time gap (seconds) between GPS fix and burst start (positive = GPS before burst, negative = acquisition lag, NA = no GPS assigned) |

**Advanced columns added (`advanced = TRUE`):**

| Columns | Description |
|---------|-------------|
| `range_x/y/z` | Value range per axis |
| `max_x/y/z`, `min_x/y/z` | Max and min per axis |
| `norm_x/y/z` | L2-norm per axis |
| `q25_x/y/z`, `q50_x/y/z`, `q75_x/y/z` | 25th, 50th, 75th percentile per axis |
| `skewness_x/y/z` | Skewness per axis |
| `kurtosis_x/y/z` | Kurtosis per axis |
| `cov_x_y`, `cov_x_z`, `cov_y_z` | Covariance between axis pairs |
| `cor_x_y`, `cor_x_z`, `cor_y_z` | Correlation between axis pairs |
| `mean_diff_x_y/x_z/y_z` | Mean of axis differences |
| `sd_diff_x_y/x_z/y_z` | SD of axis differences |
| `mean_amplitude_x/y/z` | Mean absolute amplitude of successive differences |

```r
# Default: GPS rows only, ACC stats attached, raw ACC rows removed
gps_df <- analyze_acc(df)

# Wider matching threshold (default 2 minutes)
gps_df <- analyze_acc(df, adj_gps_max_min = 10)

# Keep all original rows (raw ACC rows retained)
full_df <- analyze_acc(df, include_burst_rows = TRUE)

# Advanced metrics (requires: install.packages("moments"))
gps_df <- analyze_acc(df, advanced = TRUE)
```

**GPS–ACC matching parameter:**

| Parameter | Description | Default |
|-----------|-------------|---------|
| `adj_gps_max_min` | Max minutes the GPS fix can be before **or** after the burst start time (GPS must be exactly one row before `ACC_START`) | `2` |


