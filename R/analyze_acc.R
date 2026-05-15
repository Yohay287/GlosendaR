# ==============================================================================
#' Analyze accelerometer bursts and attach summary statistics to GPS rows
#'
#' Processes a data frame from \code{\link{glosendas_download}} and computes
#' per-burst statistics for each accelerometer axis (X, Y, Z). Works with
#' all six portal formats (V1 and V2).
#'
#' \strong{Format handling:}
#' \itemize{
#'   \item \strong{V2 formats} use typed burst markers:
#'     \code{SEN_ACC_<N>Hz_START}, \code{SEN_ACC_<N>Hz}, \code{SEN_ACC_<N>Hz_END}.
#'     Duration is computed with sub-second precision from
#'     \code{UTC_date} + \code{UTC_time} + \code{milliseconds}.
#'   \item \strong{V1 formats} use plain \code{SENSORS} rows with no
#'     START/END markers. Consecutive SENSORS rows are grouped into bursts
#'     using a time-gap threshold (\code{v1_burst_gap_sec}). Duration is
#'     computed from \code{UTC_time} (second precision).
#' }
#'
#' In both cases the function attaches burst statistics to the GPS row that
#' immediately precedes the burst (within \code{gps_window_sec} seconds).
#' If no GPS row is found, a new \code{ACC_SUMMARY} row is inserted.
#'
#' @param df Data frame from \code{glosendas_download()}.
#' @param gps_window_sec Numeric. Max seconds between GPS fix and burst start
#'   for the GPS row to be considered the burst owner. Default: \code{10}.
#' @param include_burst_rows Logical. Keep raw ACC rows in the output.
#'   Default: \code{FALSE}.
#' @param advanced Logical. Compute extended metrics (range, quantiles,
#'   skewness, kurtosis, covariance, correlation, axis differences, mean
#'   amplitude). Requires the \pkg{moments} package. Default: \code{FALSE}.
#' @param v1_burst_gap_sec Numeric. For V1 \code{SENSORS} rows only: maximum
#'   seconds between consecutive rows that are still considered the same
#'   burst. Default: \code{5}.
#' @param verbose Logical. Print summary. Default: \code{TRUE}.
#'
#' @return Data frame with ACC rows optionally removed and new columns added
#'   to GPS / ACC_SUMMARY rows. See \strong{Basic columns} and
#'   \strong{Advanced columns} in the package README.
#'
#' @examples
#' \dontrun{
#' df     <- glosendas_download("user", "pass", filter_word = "Houbara")
#' gps_df <- analyze_acc(df)
#' gps_df <- analyze_acc(df, advanced = TRUE)
#' }
#'
#' @export
analyze_acc <- function(df,
                        gps_window_sec     = 10,
                        include_burst_rows = FALSE,
                        advanced           = FALSE,
                        v1_burst_gap_sec   = 5,
                        verbose            = TRUE) {

  # ── guards ─────────────────────────────────────────────────────────────────
  if (!is.data.frame(df))        stop("`df` must be a data frame.")
  if (nrow(df) == 0)             stop("`df` has zero rows.")
  if (!is.numeric(gps_window_sec) || gps_window_sec < 0)
    stop("`gps_window_sec` must be a non-negative number.")
  if (!is.numeric(v1_burst_gap_sec) || v1_burst_gap_sec < 0)
    stop("`v1_burst_gap_sec` must be a non-negative number.")

  required <- c("datatype", "acc_x", "acc_y", "acc_z")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0)
    stop("Missing required columns: ", paste(missing, collapse = ", "))

  if (advanced && !requireNamespace("moments", quietly = TRUE))
    stop("Package 'moments' is needed for advanced = TRUE.\n",
         "Install with: install.packages('moments')")

  # ── drop gap rows (blank lines from portal become all-NA rows) ─────────────
  # This protects against CSVs saved before the blank-line fix was applied,
  # or any file loaded directly with read.csv() without pre-cleaning.
  if ("device_id" %in% names(df)) {
    gap_mask <- is.na(df$device_id) | trimws(as.character(df$device_id)) == ""
    if (any(gap_mask)) {
      if (verbose)
        message(sprintf("  Removed %d gap row(s) before ACC analysis",
                        sum(gap_mask)))
      df <- df[!gap_mask, ]
      rownames(df) <- NULL
    }
  }

  # ── coerce ACC to numeric ───────────────────────────────────────────────────
  df$acc_x <- suppressWarnings(as.numeric(df$acc_x))
  df$acc_y <- suppressWarnings(as.numeric(df$acc_y))
  df$acc_z <- suppressWarnings(as.numeric(df$acc_z))

  # ── detect format: V2 (SEN_ACC_*) or V1 (SENSORS) ─────────────────────────
  has_v2_markers <- any(grepl("^SEN_ACC_[0-9]+[Hh]z", df$datatype))
  has_v1_sensors <- any(df$datatype == "SENSORS")

  if (!has_v2_markers && !has_v1_sensors) {
    if (verbose) message("No ACC burst rows found (no SEN_ACC_* or SENSORS rows).")
    if (include_burst_rows) return(df) else return(df)
  }

  # ── build full-precision timestamp ─────────────────────────────────────────
  # V2: UTC_date + UTC_time (HH:MM:SS) + milliseconds → sub-second precision
  # V1: UTC_date + UTC_time (HH:MM:SS) → second precision
  # Fallback: UTC_datetime (minute precision only — cannot compute duration)
  has_precise_cols <- all(c("UTC_date", "UTC_time") %in% names(df))

  df$UTC_precise <- NA_real_
  class(df$UTC_precise) <- c("POSIXct", "POSIXt")
  attr(df$UTC_precise, "tzone") <- "UTC"

  if (has_precise_cols) {
    base_ts <- suppressWarnings(
      as.POSIXct(paste(df$UTC_date, df$UTC_time),
                 format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
    # try d/m/Y if Y-m-d failed
    bad <- is.na(base_ts)
    if (any(bad)) {
      base_ts[bad] <- suppressWarnings(
        as.POSIXct(paste(df$UTC_date[bad], df$UTC_time[bad]),
                   format = "%d/%m/%Y %H:%M:%S", tz = "UTC"))
    }
    # add milliseconds if available (V2 only)
    if ("milliseconds" %in% names(df)) {
      ms <- suppressWarnings(as.numeric(df$milliseconds))
      ms[is.na(ms)] <- 0
      df$UTC_precise <- base_ts + ms / 1000
    } else {
      df$UTC_precise <- base_ts
    }
  } else if ("UTC_datetime" %in% names(df)) {
    if (verbose)
      message("Note: UTC_date/UTC_time not found — using UTC_datetime ",
              "(minute precision; burst duration may be 0).")
    df$UTC_precise <- suppressWarnings(
      as.POSIXct(df$UTC_datetime, tz = "UTC"))
  }

  # ensure UTC_datetime is POSIXct
  if ("UTC_datetime" %in% names(df) &&
      !inherits(df$UTC_datetime, "POSIXct")) {
    df$UTC_datetime <- suppressWarnings(
      as.POSIXct(df$UTC_datetime, tz = "UTC"))
  }

  # ── dispatch to the appropriate burst detector ─────────────────────────────
  if (has_v2_markers) {
    bursts <- .acc_bursts_v2(df)
  } else {
    bursts <- .acc_bursts_v1(df, v1_burst_gap_sec)
  }

  if (length(bursts) == 0) {
    if (verbose) message("No valid ACC bursts found.")
    df$UTC_precise <- NULL
    if (include_burst_rows) return(df) else return(df)
  }

  is_acc <- if (has_v2_markers)
    grepl("^SEN_ACC_[0-9]+[Hh]z", df$datatype)
  else
    df$datatype == "SENSORS"

  is_gps   <- df$datatype %in% c("GPS", "GPSF")
  gps_idx  <- which(is_gps)

  if (verbose) {
    fmt <- if (has_v2_markers) "V2 (SEN_ACC_*Hz)" else "V1 (SENSORS)"
    message(sprintf("Found %d burst(s) — format: %s", length(bursts), fmt))
  }

  # ── column setup ───────────────────────────────────────────────────────────
  basic_cols <- c("acc_burst_n", "acc_freq_hz", "acc_duration_sec",
                  "acc_x_mean", "acc_x_sd",
                  "acc_y_mean", "acc_y_sd",
                  "acc_z_mean", "acc_z_sd",
                  "acc_odba")
  adv_cols <- c(
    "acc_x_range",  "acc_y_range",  "acc_z_range",
    "acc_x_max",    "acc_y_max",    "acc_z_max",
    "acc_x_min",    "acc_y_min",    "acc_z_min",
    "acc_x_norm",   "acc_y_norm",   "acc_z_norm",
    "acc_x_q25",    "acc_y_q25",    "acc_z_q25",
    "acc_x_q50",    "acc_y_q50",    "acc_z_q50",
    "acc_x_q75",    "acc_y_q75",    "acc_z_q75",
    "acc_x_skew",   "acc_y_skew",   "acc_z_skew",
    "acc_x_kurt",   "acc_y_kurt",   "acc_z_kurt",
    "acc_cov_xy",   "acc_cov_xz",   "acc_cov_yz",
    "acc_cor_xy",   "acc_cor_xz",   "acc_cor_yz",
    "acc_meandiff_xy", "acc_meandiff_xz", "acc_meandiff_yz",
    "acc_sddiff_xy",   "acc_sddiff_xz",   "acc_sddiff_yz",
    "acc_amp_x",    "acc_amp_y",    "acc_amp_z"
  )
  all_num_cols <- if (advanced) c(basic_cols, adv_cols) else basic_cols
  for (col in all_num_cols) df[[col]] <- NA_real_
  df$acc_burst_type <- NA_character_

  .amp <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 2) return(NA_real_)
    mean(abs(diff(x)))
  }

  n_attached <- 0L; n_new_row <- 0L; n_no_end <- 0L
  n_empty    <- 0L; orphans   <- list()

  # ── process each burst ─────────────────────────────────────────────────────
  for (b in seq_along(bursts)) {
    bst        <- bursts[[b]]
    s          <- bst$s
    e          <- bst$e
    burst_type <- bst$type
    truncated  <- isTRUE(bst$truncated)
    if (truncated) n_no_end <- n_no_end + 1L

    idx <- seq(s, e)
    ax  <- df$acc_x[idx]; ax <- ax[!is.na(ax)]
    ay  <- df$acc_y[idx]; ay <- ay[!is.na(ay)]
    az  <- df$acc_z[idx]; az <- az[!is.na(az)]

    if (any(c(length(ax), length(ay), length(az)) < 2L)) {
      n_empty <- n_empty + 1L
      next
    }

    freq_hz <- suppressWarnings(
      as.numeric(stringr::str_extract(burst_type, "[0-9]+(?=[Hh]z)")))

    dur_sec <- tryCatch({
      d <- as.numeric(difftime(df$UTC_precise[e], df$UTC_precise[s],
                               units = "secs"))
      if (!is.na(d) && d < 0) abs(d) else d
    }, error = function(e) NA_real_)

    # basic stats
    xm <- mean(ax); xsd <- stats::sd(ax)
    ym <- mean(ay); ysd <- stats::sd(ay)
    zm <- mean(az); zsd <- stats::sd(az)
    odba <- mean(abs(ax - xm) + abs(ay - ym) + abs(az - zm))

    sv <- list(
      acc_burst_n      = length(ax),
      acc_freq_hz      = freq_hz,
      acc_duration_sec = round(dur_sec, 2),
      acc_x_mean       = round(xm,   3), acc_x_sd = round(xsd, 3),
      acc_y_mean       = round(ym,   3), acc_y_sd = round(ysd, 3),
      acc_z_mean       = round(zm,   3), acc_z_sd = round(zsd, 3),
      acc_odba         = round(odba, 3),
      acc_burst_type   = burst_type
    )

    # advanced stats
    if (advanced) {
      sv_adv <- tryCatch({
        list(
          acc_x_range = round(max(ax) - min(ax), 3),
          acc_y_range = round(max(ay) - min(ay), 3),
          acc_z_range = round(max(az) - min(az), 3),
          acc_x_max   = round(max(ax), 3), acc_y_max = round(max(ay), 3),
          acc_z_max   = round(max(az), 3),
          acc_x_min   = round(min(ax), 3), acc_y_min = round(min(ay), 3),
          acc_z_min   = round(min(az), 3),
          acc_x_norm  = round(sqrt(sum(ax^2)), 3),
          acc_y_norm  = round(sqrt(sum(ay^2)), 3),
          acc_z_norm  = round(sqrt(sum(az^2)), 3),
          acc_x_q25   = round(stats::quantile(ax, .25), 3),
          acc_y_q25   = round(stats::quantile(ay, .25), 3),
          acc_z_q25   = round(stats::quantile(az, .25), 3),
          acc_x_q50   = round(stats::quantile(ax, .50), 3),
          acc_y_q50   = round(stats::quantile(ay, .50), 3),
          acc_z_q50   = round(stats::quantile(az, .50), 3),
          acc_x_q75   = round(stats::quantile(ax, .75), 3),
          acc_y_q75   = round(stats::quantile(ay, .75), 3),
          acc_z_q75   = round(stats::quantile(az, .75), 3),
          acc_x_skew  = round(moments::skewness(ax), 3),
          acc_y_skew  = round(moments::skewness(ay), 3),
          acc_z_skew  = round(moments::skewness(az), 3),
          acc_x_kurt  = round(moments::kurtosis(ax), 3),
          acc_y_kurt  = round(moments::kurtosis(ay), 3),
          acc_z_kurt  = round(moments::kurtosis(az), 3),
          acc_cov_xy  = round(stats::cov(ax, ay), 3),
          acc_cov_xz  = round(stats::cov(ax, az), 3),
          acc_cov_yz  = round(stats::cov(ay, az), 3),
          acc_cor_xy  = round(stats::cor(ax, ay), 3),
          acc_cor_xz  = round(stats::cor(ax, az), 3),
          acc_cor_yz  = round(stats::cor(ay, az), 3),
          acc_meandiff_xy = round(mean(ax - ay), 3),
          acc_meandiff_xz = round(mean(ax - az), 3),
          acc_meandiff_yz = round(mean(ay - az), 3),
          acc_sddiff_xy   = round(stats::sd(ax - ay), 3),
          acc_sddiff_xz   = round(stats::sd(ax - az), 3),
          acc_sddiff_yz   = round(stats::sd(ay - az), 3),
          acc_amp_x       = round(.amp(ax), 3),
          acc_amp_y       = round(.amp(ay), 3),
          acc_amp_z       = round(.amp(az), 3)
        )
      }, error = function(e) {
        warning("Advanced stats failed for burst ", b, ": ",
                conditionMessage(e), call. = FALSE)
        stats::setNames(as.list(rep(NA_real_, length(adv_cols))), adv_cols)
      })
      sv <- c(sv, sv_adv)
    }

    # find preceding GPS
    gps_before <- gps_idx[gps_idx < s]
    target     <- NA_integer_
    if (length(gps_before) > 0L) {
      last_gps  <- tail(gps_before, 1L)
      time_diff <- tryCatch(
        as.numeric(difftime(df$UTC_precise[s], df$UTC_precise[last_gps],
                            units = "secs")),
        error = function(e) NA_real_)
      if (!is.na(time_diff) && time_diff >= 0 &&
          time_diff <= gps_window_sec)
        target <- last_gps
    }

    if (!is.na(target)) {
      for (col in names(sv)) df[[col]][target] <- sv[[col]]
      n_attached <- n_attached + 1L
    } else {
      new_row          <- df[s, ]
      new_row$datatype <- "ACC_SUMMARY"
      for (col in names(sv)) new_row[[col]] <- sv[[col]]
      orphans[[length(orphans) + 1L]] <-
        list(before = s, row = new_row)
      n_new_row <- n_new_row + 1L
    }
  }

  # ── assemble output ─────────────────────────────────────────────────────────
  keep_mask <- if (include_burst_rows) rep(TRUE, nrow(df)) else !is_acc
  df$UTC_precise <- NULL
  out <- df[keep_mask, ]
  rownames(out) <- NULL

  if (length(orphans) > 0L) {
    before_vals <- vapply(orphans, function(x) x$before, integer(1))
    orphans     <- orphans[order(before_vals, decreasing = TRUE)]
    orig_idx    <- which(keep_mask)
    for (ins in orphans) {
      pos   <- sum(orig_idx < ins$before)
      ins_r <- ins$row[, names(out), drop = FALSE]
      ins_r$UTC_precise <- NULL
      if (pos == 0L) {
        out <- rbind(ins_r, out)
      } else if (pos >= nrow(out)) {
        out <- rbind(out, ins_r)
      } else {
        out <- rbind(out[seq_len(pos), ], ins_r,
                     out[(pos + 1L):nrow(out), ])
      }
      orig_idx <- c(orig_idx[seq_len(pos)], NA_integer_,
                    orig_idx[seq(pos + 1L, length(orig_idx))])
    }
    rownames(out) <- NULL
  }

  if (verbose) {
    message("\n--- ACC Burst Analysis Summary ---")
    message(sprintf("  Mode                 : %s",
                    if (advanced) "advanced" else "basic"))
    message(sprintf("  Bursts processed     : %d", length(bursts)))
    if (n_empty > 0L)
      message(sprintf("  Skipped (< 2 pts)    : %d", n_empty))
    message(sprintf("  Attached to GPS row  : %d", n_attached))
    message(sprintf("  New ACC_SUMMARY rows : %d", n_new_row))
    if (n_no_end > 0L)
      message(sprintf("  Truncated bursts     : %d (no END marker)", n_no_end))
    message(sprintf("  Output rows          : %d", nrow(out)))
    message(sprintf("  ACC columns added    : %d", length(all_num_cols) + 1L))
  }
  out
}


# ==============================================================================
# BURST DETECTORS
# ==============================================================================

#' @noRd
#' V2 burst detector: uses SEN_ACC_<N>Hz_START / _END markers.
.acc_bursts_v2 <- function(df) {
  is_start <- grepl("^SEN_ACC_[0-9]+[Hh]z_START$", df$datatype)
  is_end   <- grepl("^SEN_ACC_[0-9]+[Hh]z_END$",   df$datatype)
  is_acc   <- grepl("^SEN_ACC_[0-9]+[Hh]z",         df$datatype)
  start_idx <- which(is_start)
  end_idx   <- which(is_end)
  if (length(start_idx) == 0) return(list())
  bursts <- vector("list", length(start_idx))
  for (b in seq_along(start_idx)) {
    s      <- start_idx[b]
    next_s <- if (b < length(start_idx)) start_idx[b + 1L] else nrow(df) + 1L
    bt     <- sub("_START$", "", df$datatype[s], ignore.case = TRUE)
    e_cand <- end_idx[end_idx > s & end_idx < next_s]
    if (length(e_cand) > 0) {
      e         <- e_cand[1L]
      truncated <- FALSE
    } else {
      acc_win   <- which(is_acc & seq_len(nrow(df)) >= s &
                           seq_len(nrow(df)) < next_s)
      e         <- if (length(acc_win) > 0) max(acc_win) else s
      truncated <- TRUE
    }
    bursts[[b]] <- list(s = s, e = e, type = bt, truncated = truncated)
  }
  bursts
}


#' @noRd
#' V1 burst detector: groups consecutive SENSORS rows by time gap.
#' Rows separated by more than v1_burst_gap_sec seconds start a new burst.
#' acc_freq_hz is estimated from the median inter-row time interval.
.acc_bursts_v1 <- function(df, gap_sec = 5) {
  sensor_idx <- which(df$datatype == "SENSORS")
  if (length(sensor_idx) == 0) return(list())

  ts <- df$UTC_precise[sensor_idx]

  # group by time gap
  bursts    <- list()
  grp_start <- 1L

  for (i in seq_along(sensor_idx)) {
    is_last <- i == length(sensor_idx)
    new_grp <- if (!is_last) {
      gap <- as.numeric(difftime(ts[i + 1L], ts[i], units = "secs"))
      !is.na(gap) && gap > gap_sec
    } else TRUE

    if (new_grp) {
      s_row <- sensor_idx[grp_start]
      e_row <- sensor_idx[i]
      grp_ts <- ts[grp_start:i]

      # estimate frequency from median interval
      intervals <- diff(as.numeric(grp_ts))
      intervals <- intervals[intervals > 0 & !is.na(intervals)]
      freq_hz <- if (length(intervals) > 0)
        round(1 / stats::median(intervals), 0)
      else
        NA_real_

      burst_type <- if (!is.na(freq_hz))
        paste0("SEN_ACC_", freq_hz, "Hz")
      else
        "SEN_ACC_SENSORS"

      bursts[[length(bursts) + 1L]] <- list(
        s = s_row, e = e_row, type = burst_type, truncated = FALSE
      )
      grp_start <- i + 1L
    }
  }
  bursts
}
