# ==============================================================================
# Internal shared constants and helpers
# ==============================================================================

#' @noRd
#' Row types that represent a GPS location fix.
#' \code{GPS_ACC} is produced by \code{analyze_acc()} and must be recognised as
#' a GPS row by every downstream function (and by analyze_acc itself if it is
#' run a second time).
.GL_GPS_TYPES <- c("GPS", "GPS_ACC", "GPSF")

#' @noRd
#' Prefix identifying V2 accelerometer rows (SEN_ACC_10Hz, SEN_ACC_10Hz_START, ...).
.GL_ACC_PREFIX <- "SEN_ACC_"

#' @noRd
#' Classify a datatype vector once and reuse the result.
#' Uses startsWith()/endsWith() rather than regular expressions: on multi-million
#' row vectors this is several times faster and allocates less.
#'
#' @return list with logical vectors: gps, acc_any, acc_start, acc_end, sensors
.gl_classify <- function(datatype) {
  dt <- if (is.character(datatype)) datatype else as.character(datatype)
  acc_any <- startsWith(dt, .GL_ACC_PREFIX)
  list(
    dt        = dt,
    gps       = dt %in% .GL_GPS_TYPES,
    acc_any   = acc_any,
    acc_start = acc_any & endsWith(dt, "_START"),
    acc_end   = acc_any & endsWith(dt, "_END"),
    sensors   = dt == "SENSORS"
  )
}

#' @noRd
#' Convert a vector to POSIXct (UTC), accepting POSIXct, numeric epoch seconds,
#' or character in any of the formats emitted by the portal.
#' Fractional seconds (\%OS) are preserved.
#'
#' This is the single timestamp parser used across the package; keeping one
#' implementation prevents the same value parsing in one function and failing
#' in another.
.gl_to_posix <- function(x) {
  if (inherits(x, "POSIXct"))
    return(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"))
  if (inherits(x, "Date"))
    return(as.POSIXct(as.numeric(x) * 86400, origin = "1970-01-01", tz = "UTC"))
  if (is.numeric(x))
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))

  x <- as.character(x)
  fmts <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%OS",
    "%d/%m/%Y %H:%M"
  )
  out       <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01",
                          tz = "UTC")
  remaining <- which(!is.na(x) & nzchar(x))
  for (fmt in fmts) {
    if (!length(remaining)) break
    parsed <- suppressWarnings(
      as.POSIXct(x[remaining], format = fmt, tz = "UTC"))
    ok <- !is.na(parsed)
    if (any(ok)) out[remaining[ok]] <- parsed[ok]
    remaining <- remaining[!ok]
  }
  out
}

#' @noRd
#' Best available timestamp for a data frame, preferring sub-second precision.
#'
#' Order of preference:
#'   1. \code{UTC_timestamp}  — already carries milliseconds
#'   2. \code{UTC_datetime} (+ \code{milliseconds} when present)
#'   3. \code{UTC_date} + \code{UTC_time} (+ \code{milliseconds})
#'
#' Preferring an existing single column avoids building a multi-million element
#' pasted string just to parse it again.
.gl_best_timestamp <- function(df) {
  has_ms <- "milliseconds" %in% names(df)
  ms_vec <- function() {
    m <- suppressWarnings(as.numeric(df$milliseconds))
    m[is.na(m)] <- 0
    m / 1000
  }

  if ("UTC_timestamp" %in% names(df)) {
    ts <- .gl_to_posix(df$UTC_timestamp)
    if (!all(is.na(ts))) {
      # Already sub-second if the source string carried milliseconds.
      if (has_ms && all(as.numeric(ts) %% 1 == 0, na.rm = TRUE))
        ts <- ts + ms_vec()
      return(ts)
    }
  }

  if ("UTC_datetime" %in% names(df)) {
    ts <- .gl_to_posix(df$UTC_datetime)
    if (!all(is.na(ts))) {
      if (has_ms) ts <- ts + ms_vec()
      return(ts)
    }
  }

  if (all(c("UTC_date", "UTC_time") %in% names(df))) {
    ts <- .gl_to_posix(paste(df$UTC_date, df$UTC_time))
    if (has_ms) ts <- ts + ms_vec()
    return(ts)
  }

  as.POSIXct(rep(NA_real_, nrow(df)), origin = "1970-01-01", tz = "UTC")
}

#' @noRd
#' Coerce a column to numeric only when it is not already numeric.
#' Avoids allocating a full copy of multi-million row columns on every call.
.gl_as_num <- function(x) {
  if (is.numeric(x)) x else suppressWarnings(as.numeric(x))
}

#' @noRd
#' Warn when rows are not ordered by tag then time.
#'
#' Every burst/event/day function relies on consecutive rows being consecutive
#' in time within an individual. Unsorted input produces silently wrong results,
#' so it is flagged rather than left to corrupt the output.
#'
#' A small negative gap is normal and is NOT reported: the portal emits the GPS
#' row before the ACC_START row of the same acquisition even though the GPS
#' timestamp can be a fraction of a second later (acquisition lag). Only
#' inversions larger than \code{tol_sec} indicate genuinely unsorted data.
#'
#' @return TRUE when correctly ordered.
.gl_check_order <- function(df, ts_num, fn = "this function", tol_sec = 5) {
  if (nrow(df) < 2L) return(TRUE)

  ok <- TRUE
  if ("tag_name" %in% names(df)) {
    tags <- as.character(df$tag_name)
    # Each tag must occupy one contiguous block
    r <- rle(tags)
    if (anyDuplicated(r$values)) ok <- FALSE
    same_tag <- tags[-1] == tags[-length(tags)]
  } else {
    same_tag <- rep(TRUE, nrow(df) - 1L)
  }

  if (ok) {
    d <- diff(ts_num)
    if (any(!is.na(d) & d < -abs(tol_sec) & same_tag)) ok <- FALSE
  }

  if (!ok)
    warning(fn, ": rows are not sorted by tag_name and time. ",
            "Burst, event and day detection assume consecutive rows are ",
            "consecutive in time within an individual — results will be ",
            "unreliable. Sort first, e.g.:\n",
            "  df <- df[order(df$tag_name, df$UTC_datetime), ]",
            call. = FALSE)
  ok
}
