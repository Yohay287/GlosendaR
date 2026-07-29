# =============================================================================
# test-analysis-functions.R
#
# End-to-end tests for the six analysis functions on a small synthetic dataset.
# Runs in a couple of seconds — designed to surface hidden bugs quickly.
#
# Run standalone with:
#   source("tests/testthat/test-analysis-functions.R")
# or as part of the suite with:
#   devtools::test()
# =============================================================================

# ---------------------------------------------------------------------------
# Synthetic data generator
# ---------------------------------------------------------------------------
# Structure per bird, repeated every `cycle_min` minutes:
#   - GPS burst : `gps_burst` fixes, 1 second apart          (datatype "GPS")
#   - ACC burst : START + middle rows + END at 10 Hz          (SEN_ACC_10Hz*)
# The last GPS fix sits exactly one row before ACC_START, which is the
# condition analyze_acc() uses to attach a fix to a burst.
make_test_data <- function(birds     = c("Test Bird A", "Test Bird B"),
                           n_cycles  = 30,
                           gps_burst = 5,
                           acc_n     = 20,
                           cycle_min = 30,
                           start_utc = "2024-06-01 00:00:00",
                           seed      = 42) {
  set.seed(seed)
  t0 <- as.POSIXct(start_utc, tz = "UTC")

  one_bird <- function(bird, dev_id, lat0, lon0, offset_days) {
    rows <- vector("list", n_cycles)
    for (i in seq_len(n_cycles)) {
      cyc_t <- t0 + offset_days * 86400 + (i - 1) * cycle_min * 60

      # --- GPS burst: gps_burst fixes 1 s apart -----------------------------
      g_ts  <- cyc_t + seq_len(gps_burst) - 1
      drift <- (i - 1) * 0.002
      gps <- data.frame(
        device_id    = dev_id,
        tag_name     = bird,
        UTC_datetime = g_ts,
        UTC_date     = format(g_ts, "%Y-%m-%d", tz = "UTC"),
        UTC_time     = format(g_ts, "%H:%M:%S", tz = "UTC"),
        datatype     = "GPS",
        Latitude     = round(lat0 + drift + rnorm(gps_burst, 0, 1e-5), 5),
        Longitude    = round(lon0 + drift + rnorm(gps_burst, 0, 1e-5), 5),
        altitude_m   = round(300 + rnorm(gps_burst, 0, 5)),
        speed_kmh    = round(abs(rnorm(gps_burst, 2, 1)), 1),
        acc_x        = NA_real_, acc_y = NA_real_, acc_z = NA_real_,
        milliseconds = c(200L, rep(0L, gps_burst - 1L)),
        stringsAsFactors = FALSE
      )
      # GPS timestamp lands fractionally AFTER the burst start (acquisition lag)
      gps$UTC_timestamp <- format(g_ts + gps$milliseconds / 1000,
                                  "%Y-%m-%d %H:%M:%OS3", tz = "UTC")

      # --- ACC burst at 10 Hz, starting 0.1 s before the last GPS fix -------
      a_start <- g_ts[gps_burst] - 0.1
      a_ts    <- a_start + (seq_len(acc_n) - 1) * 0.1
      a_type  <- c("SEN_ACC_10Hz_START",
                   rep("SEN_ACC_10Hz", acc_n - 2L),
                   "SEN_ACC_10Hz_END")
      acc <- data.frame(
        device_id    = dev_id,
        tag_name     = bird,
        UTC_datetime = as.POSIXct(trunc(as.numeric(a_ts)),
                                  origin = "1970-01-01", tz = "UTC"),
        UTC_date     = format(a_ts, "%Y-%m-%d", tz = "UTC"),
        UTC_time     = format(a_ts, "%H:%M:%S", tz = "UTC"),
        datatype     = a_type,
        Latitude     = NA_real_, Longitude = NA_real_,
        altitude_m   = NA_real_, speed_kmh = NA_real_,
        acc_x        = round(rnorm(acc_n,   20, 40)),
        acc_y        = round(rnorm(acc_n,  -80, 40)),
        acc_z        = round(rnorm(acc_n, 1000, 40)),
        milliseconds = round((as.numeric(a_ts) %% 1) * 1000),
        stringsAsFactors = FALSE
      )
      acc$UTC_timestamp <- format(a_ts, "%Y-%m-%d %H:%M:%OS3", tz = "UTC")

      rows[[i]] <- rbind(gps, acc)
    }
    do.call(rbind, rows)
  }

  out <- do.call(rbind, lapply(seq_along(birds), function(k)
    one_bird(birds[k], as.character(216400 + k),
             30.70 + k * 0.05, 34.45 + k * 0.05, offset_days = 0)))
  rownames(out) <- NULL
  out
}

# Inject awkward-but-real edge cases
add_edge_cases <- function(df) {
  bird <- df$tag_name[1]; dev <- df$device_id[1]
  base <- max(as.POSIXct(df$UTC_datetime[df$tag_name == bird], tz = "UTC")) + 3600

  mk <- function(ts, type, ax = NA, ay = NA, az = NA, lat = NA, lon = NA) {
    data.frame(
      device_id = dev, tag_name = bird, UTC_datetime = ts,
      UTC_date  = format(ts, "%Y-%m-%d", tz = "UTC"),
      UTC_time  = format(ts, "%H:%M:%S", tz = "UTC"),
      datatype  = type, Latitude = lat, Longitude = lon,
      altitude_m = NA_real_, speed_kmh = NA_real_,
      acc_x = ax, acc_y = ay, acc_z = az,
      milliseconds = round((as.numeric(ts) %% 1) * 1000),
      UTC_timestamp = format(ts, "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
      stringsAsFactors = FALSE)
  }

  # (a) orphan ACC burst — no GPS fix immediately before it
  o_ts <- base + (0:9) * 0.1
  orphan <- do.call(rbind, lapply(seq_along(o_ts), function(i)
    mk(o_ts[i],
       if (i == 1) "SEN_ACC_10Hz_START" else if (i == 10) "SEN_ACC_10Hz_END"
       else "SEN_ACC_10Hz",
       ax = round(rnorm(1, 20, 40)), ay = round(rnorm(1, -80, 40)),
       az = round(rnorm(1, 1000, 40)))))

  # (b) truncated ACC burst — START with no END
  t_ts <- base + 600 + (0:5) * 0.1
  trunc_b <- do.call(rbind, lapply(seq_along(t_ts), function(i)
    mk(t_ts[i], if (i == 1) "SEN_ACC_10Hz_START" else "SEN_ACC_10Hz",
       ax = round(rnorm(1, 20, 40)), ay = round(rnorm(1, -80, 40)),
       az = round(rnorm(1, 1000, 40)))))

  # (c) GPSF fix followed by its own ACC burst (should be a separate event)
  f_ts   <- base + 1200
  gpsf   <- mk(f_ts, "GPSF", lat = 30.75, lon = 34.50)
  fa_ts  <- f_ts + 0.5 + (0:9) * 0.1
  gpsf_a <- do.call(rbind, lapply(seq_along(fa_ts), function(i)
    mk(fa_ts[i],
       if (i == 1) "SEN_ACC_10Hz_START" else if (i == 10) "SEN_ACC_10Hz_END"
       else "SEN_ACC_10Hz",
       ax = round(rnorm(1, 20, 40)), ay = round(rnorm(1, -80, 40)),
       az = round(rnorm(1, 1000, 40)))))

  # Keep the portal's row layout: the GPS block precedes the ACC block of the
  # same acquisition even though the GPS timestamp can be fractionally later.
  # Sorting globally on sub-second time would interleave ACC_START into the GPS
  # burst, which never happens in real downloads.
  out <- rbind(df, orphan, trunc_b, gpsf, gpsf_a)
  out <- out[order(match(out$tag_name, unique(out$tag_name))), ]
  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------
BIRDS  <- c("Test Bird A", "Test Bird B")
N_CYC  <- 30
GPS_B  <- 5
ACC_N  <- 20
raw    <- add_edge_cases(make_test_data(BIRDS, N_CYC, GPS_B, ACC_N))
n_gps_bursts_total <- length(BIRDS) * N_CYC


# ===========================================================================
test_that("synthetic fixture has the expected shape", {
  expect_equal(nrow(raw),
               length(BIRDS) * N_CYC * (GPS_B + ACC_N) + 10 + 6 + 1 + 10)
  expect_setequal(unique(raw$tag_name), BIRDS)
  expect_true(all(c("GPS", "GPSF", "SEN_ACC_10Hz_START",
                    "SEN_ACC_10Hz_END") %in% raw$datatype))
})


# ===========================================================================
test_that("detect_gps_burst finds the planted burst size", {
  info <- detect_gps_burst(raw, verbose = FALSE)
  expect_false(is.null(info))
  expect_equal(info$burst_size, GPS_B)
  expect_equal(info$n_bursts, n_gps_bursts_total)
  expect_true(is.data.frame(info$excluded))
  expect_true(all(c("length","count","likely","row_starts","row_ends")
                  %in% names(info$excluded)))
})

test_that("detect_gps_burst does not merge bursts across individuals", {
  # Two birds each contribute N_CYC bursts; a cross-tag merge would change
  # the count or the dominant size.
  one <- detect_gps_burst(raw[raw$tag_name == BIRDS[1], ], verbose = FALSE)
  expect_equal(one$burst_size, GPS_B)
  expect_equal(one$n_bursts, N_CYC)
})


# ===========================================================================
test_that("collapse_gps_burst reduces each burst to one row", {
  for (m in c("first", "last", "mean")) {
    got <- collapse_gps_burst(raw, burst_size = GPS_B, method = m,
                              verbose = FALSE)
    expect_equal(nrow(got),
                 nrow(raw) - n_gps_bursts_total * (GPS_B - 1L),
                 info = m)
  }
})

test_that("collapse_gps_burst(method='mean') keeps numeric columns numeric", {
  got <- collapse_gps_burst(raw, burst_size = GPS_B, method = "mean",
                            verbose = FALSE)
  expect_type(got$Latitude,  "double")
  expect_type(got$Longitude, "double")
  expect_false(is.character(got$Latitude))
})

test_that("collapse_gps_burst 'first' and 'last' pick different fixes", {
  f <- collapse_gps_burst(raw, burst_size = GPS_B, method = "first", verbose = FALSE)
  l <- collapse_gps_burst(raw, burst_size = GPS_B, method = "last",  verbose = FALSE)
  ff <- f$UTC_timestamp[f$datatype == "GPS"][1]
  ll <- l$UTC_timestamp[l$datatype == "GPS"][1]
  expect_false(identical(ff, ll))
})


# ===========================================================================
test_that("analyze_acc attaches burst stats to the preceding GPS fix", {
  got <- analyze_acc(raw, verbose = FALSE)

  expect_true("GPS_ACC" %in% got$datatype)
  expect_true(all(c("mean_x","sd_x","mean_y","sd_y","mean_z","sd_z",
                    "acc_odba","acc_burst_n","acc_freq_hz",
                    "acc_duration_sec","acc_burst_type",
                    "gps_to_burst_sec") %in% names(got)))

  # raw ACC rows removed by default
  expect_false(any(startsWith(got$datatype, "SEN_ACC_")))

  # every regular cycle burst should have found its GPS fix
  expect_equal(sum(got$datatype == "GPS_ACC"), n_gps_bursts_total + 1L)

  # frequency parsed from the type string
  expect_equal(unique(stats::na.omit(got$acc_freq_hz)), 10)

  # burst n matches the planted burst length
  expect_equal(unique(stats::na.omit(got$acc_burst_n[got$datatype == "GPS_ACC" &
                                                       got$acc_burst_n == ACC_N])),
               ACC_N)
})

test_that("analyze_acc records the negative acquisition lag", {
  got <- analyze_acc(raw, verbose = FALSE)
  lag <- stats::na.omit(got$gps_to_burst_sec)
  expect_true(length(lag) > 0)
  # GPS stamped 0.2 s after the fix second, burst starts 0.1 s before it
  expect_true(any(lag < 0))
  expect_true(all(abs(lag) <= 2 * 60))
})

test_that("analyze_acc creates ACC_SUMMARY rows for orphan bursts", {
  got <- analyze_acc(raw, verbose = FALSE)
  expect_true("ACC_SUMMARY" %in% got$datatype)
  orph <- got[got$datatype == "ACC_SUMMARY", ]
  expect_true(all(is.na(orph$gps_to_burst_sec)))
  expect_true(all(!is.na(orph$mean_x)))
})

test_that("analyze_acc output stays in chronological order", {
  got <- analyze_acc(raw, verbose = FALSE)
  for (b in BIRDS) {
    ts <- as.numeric(as.POSIXct(got$UTC_timestamp[got$tag_name == b], tz = "UTC"))
    expect_false(is.unsorted(ts, na.rm = TRUE), info = b)
  }
})

test_that("analyze_acc is re-entrant (GPS_ACC still recognised as GPS)", {
  once  <- analyze_acc(raw,  verbose = FALSE)
  twice <- analyze_acc(once, verbose = FALSE)
  # second pass has no ACC rows left, so nothing may be lost
  expect_equal(nrow(twice), nrow(once))
  expect_equal(sum(twice$datatype == "GPS_ACC"),
               sum(once$datatype == "GPS_ACC"))
})

test_that("analyze_acc keeps raw rows when asked", {
  got <- analyze_acc(raw, include_burst_rows = TRUE, verbose = FALSE)
  expect_true(any(startsWith(got$datatype, "SEN_ACC_")))
  expect_gte(nrow(got), nrow(raw))
})

test_that("analyze_acc handles truncated bursts without error", {
  got <- analyze_acc(raw, verbose = FALSE)
  expect_true(nrow(got) > 0)
  expect_false(any(is.infinite(stats::na.omit(got$acc_duration_sec))))
})

test_that("analyze_acc never attaches one bird's GPS to another's burst", {
  got <- analyze_acc(raw, include_burst_rows = TRUE, verbose = FALSE)
  ga  <- got[got$datatype == "GPS_ACC", ]
  # A cross-tag match would produce a gps_to_burst_sec far outside the window
  expect_true(all(abs(stats::na.omit(ga$gps_to_burst_sec)) <= 2 * 60))
})

test_that("analyze_acc advanced mode adds the extended columns", {
  skip_if_not_installed("moments")
  got <- analyze_acc(raw, advanced = TRUE, verbose = FALSE)
  expect_true(all(c("range_x","q25_x","q50_x","q75_x","skewness_x",
                    "kurtosis_x","cov_x_y","cor_x_y","mean_diff_x_y",
                    "sd_diff_x_y","mean_amplitude_x","norm_x")
                  %in% names(got)))
  # no acc_ prefix on the advanced columns
  expect_false("acc_x_range" %in% names(got))
  expect_true(any(!is.na(got$range_x)))
})

test_that("analyze_acc basic stats match a direct calculation", {
  got <- analyze_acc(raw, include_burst_rows = TRUE, verbose = FALSE)
  # first burst of bird A
  acc_rows <- which(startsWith(raw$datatype, "SEN_ACC_") &
                      raw$tag_name == BIRDS[1])
  first_burst <- acc_rows[1:ACC_N]
  expect_equal(mean(raw$acc_x[first_burst]),
               got$mean_x[got$datatype == "GPS_ACC"][1], tolerance = 1e-3)
  expect_equal(stats::sd(raw$acc_z[first_burst]),
               got$sd_z[got$datatype == "GPS_ACC"][1], tolerance = 1e-3)
  ox <- raw$acc_x[first_burst]; oy <- raw$acc_y[first_burst]
  oz <- raw$acc_z[first_burst]
  odba_ref <- mean(abs(ox - mean(ox)) + abs(oy - mean(oy)) + abs(oz - mean(oz)))
  expect_equal(odba_ref, got$acc_odba[got$datatype == "GPS_ACC"][1],
               tolerance = 1e-3)
})


# ===========================================================================
test_that("add_event_id groups a GPS burst with the ACC burst that follows", {
  acc <- analyze_acc(raw, include_burst_rows = TRUE, verbose = FALSE)
  ev  <- add_event_id(acc, verbose = FALSE)

  expect_true("Event_ID" %in% names(ev))
  expect_false("Event_count" %in% names(ev))   # FALSE by default

  # Each cycle = 1 event; edge cases add a few more
  expect_gte(length(unique(ev$Event_ID)), n_gps_bursts_total)

  # Event_ID format "<tag>_<YYYY-MM-DD HH:MM:SS>"
  expect_true(all(grepl("^.+_\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$",
                        ev$Event_ID)))
})

test_that("add_event_id starts a new event when GPS follows ACC", {
  # Run on the raw frame so the GPSF row still carries its original label
  # (analyze_acc relabels a matched GPSF to GPS_ACC).
  ev <- add_event_id(raw, verbose = FALSE)
  gpsf_row <- which(ev$datatype == "GPSF")
  expect_true(length(gpsf_row) > 0)
  prev <- gpsf_row[1] - 1L
  expect_true(prev >= 1L)
  expect_true(startsWith(ev$datatype[prev], "SEN_ACC_"))
  # the GPSF row must open its own event, not continue the ACC burst before it
  expect_false(ev$Event_ID[gpsf_row[1]] == ev$Event_ID[prev])
})

test_that("add_event_id never spans two individuals", {
  acc <- analyze_acc(raw, include_burst_rows = TRUE, verbose = FALSE)
  ev  <- add_event_id(acc, verbose = FALSE)
  per_event_tags <- tapply(ev$tag_name, ev$Event_ID, function(x) length(unique(x)))
  expect_true(all(per_event_tags == 1L))
})

test_that("add_event_count is optional and constant within an event", {
  acc <- analyze_acc(raw, include_burst_rows = TRUE, verbose = FALSE)
  ev  <- add_event_id(acc, add_event_count = TRUE, verbose = FALSE)
  expect_true("Event_count" %in% names(ev))
  chk <- tapply(ev$Event_count, ev$Event_ID, function(x) length(unique(x)))
  expect_true(all(chk == 1L))
  # the recorded count must equal the actual number of rows in that event
  sizes  <- tapply(ev$Event_count, ev$Event_ID, `[`, 1)
  counts <- as.integer(table(ev$Event_ID))
  expect_equal(sort(as.integer(sizes)), sort(counts))
  expect_equal(sum(as.integer(sizes)), nrow(ev))
})


# ===========================================================================
test_that("add_day_id assigns solar days and preserves all input columns", {
  skip_if_not_installed("suncalc")
  acc <- analyze_acc(raw, include_burst_rows = TRUE, verbose = FALSE)
  ev  <- add_event_id(acc, verbose = FALSE)
  d   <- add_day_id(ev, verbose = FALSE)

  expect_true("Day_ID" %in% names(d))
  expect_true(all(names(ev) %in% names(d)))      # nothing dropped
  expect_equal(nrow(d), nrow(ev))
  expect_true(all(grepl("^.+_\\d{8}$", d$Day_ID[nzchar(d$Day_ID)])))
})

test_that("add_day_id propagates Day_ID to ACC rows via Event_ID", {
  skip_if_not_installed("suncalc")
  acc <- analyze_acc(raw, include_burst_rows = TRUE, verbose = FALSE)
  ev  <- add_event_id(acc, verbose = FALSE)
  d   <- add_day_id(ev, verbose = FALSE)

  # ACC rows have no coordinates but must still get a Day_ID
  acc_rows <- startsWith(d$datatype, "SEN_ACC_")
  expect_true(any(acc_rows))
  expect_true(all(nzchar(d$Day_ID[acc_rows]) & !is.na(d$Day_ID[acc_rows])))

  # and it must equal the Day_ID of the GPS row in the same event
  chk <- tapply(d$Day_ID, d$Event_ID, function(x) length(unique(x[!is.na(x)])))
  expect_true(all(chk == 1L))
})

test_that("add_day_id works for nocturnal and diurnal, with day_progress", {
  skip_if_not_installed("suncalc")
  acc <- analyze_acc(raw, include_burst_rows = TRUE, verbose = FALSE)
  ev  <- add_event_id(acc, verbose = FALSE)

  for (ty in c("diurnal", "nocturnal")) {
    d <- add_day_id(ev, type = ty, day_progress = TRUE, verbose = FALSE)
    expect_true("day_progress" %in% names(d), info = ty)
    dp <- stats::na.omit(d$day_progress)
    expect_true(length(dp) > 0, info = ty)
    expect_true(all(dp >= 0 & dp <= 200), info = ty)
  }
})

test_that("add_day_id adds sun angle in degrees", {
  skip_if_not_installed("suncalc")
  acc <- analyze_acc(raw, verbose = FALSE)
  ev  <- add_event_id(acc, verbose = FALSE)
  d   <- add_day_id(ev, add_sun_angle = TRUE, verbose = FALSE)

  expect_true("sun_angle_deg" %in% names(d))
  sa <- stats::na.omit(d$sun_angle_deg)
  expect_true(length(sa) > 0)
  expect_true(all(sa >= -90 & sa <= 90))
})

test_that("add_day_id keeps individuals separate", {
  skip_if_not_installed("suncalc")
  acc <- analyze_acc(raw, verbose = FALSE)
  ev  <- add_event_id(acc, verbose = FALSE)
  d   <- add_day_id(ev, verbose = FALSE)
  # Day_ID is prefixed with the tag name, so a leak would show up here
  ok <- mapply(function(id, tg) startsWith(id, tg), d$Day_ID, d$tag_name)
  expect_true(all(ok))
})


# ===========================================================================
test_that("unsorted input raises a warning rather than failing silently", {
  shuffled <- raw[sample(nrow(raw)), ]
  rownames(shuffled) <- NULL
  # Shuffling breaks both tag contiguity and time order; either is reportable
  expect_warning(analyze_acc(shuffled, verbose = FALSE), "Sort")
  expect_warning(add_event_id(shuffled, verbose = FALSE), "Sort")
})

test_that("out-of-order time within one tag is reported", {
  one <- raw[raw$tag_name == BIRDS[1], ]
  set.seed(7)
  one <- one[sample(nrow(one)), ]        # tag stays contiguous, time does not
  rownames(one) <- NULL
  expect_warning(analyze_acc(one, verbose = FALSE), "backwards in time")
})

test_that("normal GPS/ACC acquisition lag does NOT trigger the sort warning", {
  # The portal emits the GPS row before ACC_START even though the GPS timestamp
  # can be several seconds later. That must never be reported as unsorted.
  base <- as.POSIXct("2026-02-25 13:00:00", tz = "UTC")
  for (lag in c(0.03, 0.9, 5, 10)) {
    d <- data.frame(
      tag_name = "B",
      datatype = c("GPS", "SEN_ACC_10Hz_START",
                   rep("SEN_ACC_10Hz", 8), "SEN_ACC_10Hz_END"),
      UTC_datetime = c(base + lag, base + (0:9) * 0.1),
      stringsAsFactors = FALSE)
    res <- withCallingHandlers(
      .gl_check_order(d, as.numeric(d$UTC_datetime), "test"),
      warning = function(w) stop("unexpected warning at lag ", lag))
    expect_true(res)
  }
})

test_that("functions accept data.table / tibble-like inputs", {
  fake <- raw
  class(fake) <- c("tbl_df", "tbl", "data.frame")

  a <- analyze_acc(fake, verbose = FALSE)
  expect_true(is.data.frame(a))
  expect_true("GPS_ACC" %in% a$datatype)

  e <- add_event_id(fake, verbose = FALSE)
  expect_true(is.data.frame(e))
  expect_true("Event_ID" %in% names(e))

  # output is a plain data.frame regardless of input subclass
  expect_equal(class(a), "data.frame")
})

test_that("empty and single-row inputs are rejected cleanly", {
  expect_error(analyze_acc(raw[0, ], verbose = FALSE), "zero rows")
  expect_error(add_event_id(raw[0, ], verbose = FALSE), "zero rows")
})
