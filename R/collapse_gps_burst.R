# ==============================================================================
#' Collapse GPS bursts into single representative rows
#'
#' Identifies GPS bursts — runs of exactly \code{burst_size} consecutive GPS
#' fixes each separated by at most \code{max_gap_sec} seconds — and collapses
#' each burst into one representative row. All other rows (non-GPS, ACC bursts,
#' flight detection sequences of a different length) are left completely
#' untouched.
#'
#' \strong{Burst detection rule:} a run of consecutive GPS rows qualifies as a
#' burst only when its length equals \code{burst_size} exactly. Longer runs
#' (e.g. flight detection sequences) are never collapsed.
#'
#' \strong{Mean rounding:} when \code{method = "mean"}, each numeric column is
#' rounded to the same number of decimal places found in the raw data for that
#' column, so coordinate precision is preserved.
#'
#' \strong{Datetime on mean rows:} the first timestamp of the burst is used
#' (averaging timestamps is meaningless).
#'
#' @param df A data frame containing GPS tracking data, as returned by
#'   \code{\link{glosendas_download}} or loaded from a Glosendas CSV.
#'   Must contain columns \code{datatype} and \code{UTC_datetime}.
#' @param burst_size Integer >= 2. The exact number of fixes that constitute
#'   one GPS burst. This is a device setting defined by the user and is
#'   constant across the dataset. Default: \code{5}.
#' @param method Character. How to represent the collapsed burst:
#'   \itemize{
#'     \item \code{"mean"}  — mean of all numeric columns, rounded to the
#'       same decimal places as the raw data (default)
#'     \item \code{"first"} — first fix of the burst
#'     \item \code{"last"}  — last fix of the burst
#'   }
#' @param max_gap_sec Numeric. Maximum seconds between consecutive GPS fixes
#'   that are still considered part of the same burst. Default: \code{2}.
#' @param verbose Logical. Print a processing summary. Default: \code{TRUE}.
#'
#' @return A data frame with the same columns as \code{df}. Each GPS burst of
#'   exactly \code{burst_size} rows is replaced by a single row. The number of
#'   rows removed equals \code{n_bursts * (burst_size - 1)}.
#'
#' @examples
#' \dontrun{
#' df <- glosendas_download("myuser", "mypass", filter_word = "Houbara")
#'
#' # Collapse 5-fix bursts using the mean (default)
#' df_c <- collapse_gps_burst(df, burst_size = 5)
#'
#' # Use the first fix instead
#' df_c <- collapse_gps_burst(df, burst_size = 5, method = "first")
#'
#' # Then continue with ACC analysis on the collapsed data
#' gps_df <- analyze_acc(df_c)
#' }
#'
#' @export
collapse_gps_burst <- function(df,
                               burst_size  = 5,
                               method      = "mean",
                               max_gap_sec = 2,
                               verbose     = TRUE) {

  # ── input guards ────────────────────────────────────────────────────────────
  if (!is.data.frame(df))
    stop("`df` must be a data frame.")
  if (nrow(df) == 0)
    stop("`df` has zero rows.")
  if (!is.numeric(burst_size) || length(burst_size) != 1 ||
      burst_size < 2 || burst_size != round(burst_size))
    stop("`burst_size` must be a single integer >= 2.")
  if (!method %in% c("first", "last", "mean"))
    stop('`method` must be "first", "last", or "mean".')
  if (!is.numeric(max_gap_sec) || length(max_gap_sec) != 1 ||
      max_gap_sec < 0)
    stop("`max_gap_sec` must be a non-negative number.")

  required <- c("datatype", "UTC_datetime")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0)
    stop("Missing required columns: ", paste(missing, collapse = ", "))

  burst_size <- as.integer(burst_size)

  # ── parse datetime ───────────────────────────────────────────────────────────
  # Try multiple formats robustly
  dt <- .parse_dt_multi(df$UTC_datetime)
  if (sum(!is.na(dt)) == 0)
    stop("Could not parse `UTC_datetime`. Supported formats: ",
         "YYYY-MM-DDTHH:MM:SSZ, YYYY-MM-DD HH:MM:SS, DD/MM/YYYY HH:MM:SS.")

  is_gps <- !is.na(df$datatype) & df$datatype == "GPS"

  if (!any(is_gps)) {
    if (verbose) message("No GPS rows found — returning df unchanged.")
    return(df)
  }

  # ── detect decimal places per column (from GPS rows only) ───────────────────
  # Apply to a sample of GPS rows for speed on large datasets
  gps_sample <- which(is_gps)
  if (length(gps_sample) > 500L)
    gps_sample <- gps_sample[seq(1L, length(gps_sample), length.out = 500L)]

  col_dec <- vapply(names(df), function(cn) {
    .max_decimals(df[[cn]][gps_sample])
  }, integer(1L))
  names(col_dec) <- names(df)

  # ── identify burst membership ─────────────────────────────────────────────
  n        <- nrow(df)
  in_burst <- logical(n)
  i        <- 1L

  while (i <= n) {
    if (!is_gps[i] || is.na(dt[i])) { i <- i + 1L; next }

    # Extend run of GPS fixes within max_gap_sec
    run <- i
    j   <- i + 1L
    while (j <= n && is_gps[j] && !is.na(dt[j]) &&
           as.numeric(difftime(dt[j], dt[run[length(run)]],
                               units = "secs")) <= max_gap_sec) {
      run <- c(run, j)
      j   <- j + 1L
    }

    # Only mark as burst if EXACTLY burst_size
    if (length(run) == burst_size) {
      in_burst[run] <- TRUE
      i <- j
    } else {
      i <- i + 1L
    }
  }

  n_bursts <- sum(in_burst) / burst_size

  if (n_bursts == 0L) {
    if (verbose)
      message(sprintf(
        "No GPS bursts of exactly %d fixes found — returning df unchanged.",
        burst_size))
    return(df)
  }

  # ── datetime columns to always take from first fix (never average) ──────────
  dt_col_names <- intersect(
    c("UTC_datetime", "UTC_date", "UTC_time", "UTC_timestamp", "UTC_precise"),
    names(df)
  )

  # ── build output ─────────────────────────────────────────────────────────────
  # Pre-allocate index vector: which row index to keep for each output row
  keep_rows  <- integer(n - as.integer(n_bursts) * (burst_size - 1L))
  out_count  <- 0L

  # For mean method, store replacement values per burst
  mean_patches <- list()   # list of list(row_idx, col, value)

  i <- 1L
  while (i <= n) {

    if (!in_burst[i]) {
      out_count          <- out_count + 1L
      keep_rows[out_count] <- i
      i <- i + 1L
      next
    }

    # Collect contiguous burst block
    end <- i
    while (end + 1L <= n && in_burst[end + 1L]) end <- end + 1L
    burst_idx <- seq(i, end)

    # The representative row index in the ORIGINAL df
    rep_idx <- switch(method,
      first = i,
      last  = end,
      mean  = i        # placeholder; patched below
    )

    out_count            <- out_count + 1L
    keep_rows[out_count] <- rep_idx

    # For mean, compute replacements without touching df yet
    if (method == "mean") {
      burst_sub <- df[burst_idx, , drop = FALSE]
      patches   <- list()

      for (cn in names(df)) {
        if (cn %in% dt_col_names) next   # datetime: keep first, no patching

        vals <- suppressWarnings(as.numeric(burst_sub[[cn]]))
        if (all(is.na(vals))) next       # all NA or non-numeric: keep first

        m   <- mean(vals, na.rm = TRUE)
        dec <- col_dec[cn]

        val_out <- if (dec == 0L) {
          as.character(round(m))
        } else {
          format(round(m, dec), nsmall = dec, trim = TRUE)
        }
        patches[[cn]] <- val_out
      }
      mean_patches[[length(mean_patches) + 1L]] <-
        list(out_row = out_count, patches = patches)
    }

    i <- end + 1L
  }

  # ── slice df to keep rows ────────────────────────────────────────────────────
  out           <- df[keep_rows[seq_len(out_count)], , drop = FALSE]
  rownames(out) <- NULL

  # ── apply mean patches ────────────────────────────────────────────────────────
  if (method == "mean" && length(mean_patches) > 0L) {
    for (p in mean_patches) {
      r <- p$out_row
      for (cn in names(p$patches)) {
        out[[cn]][r] <- p$patches[[cn]]
      }
    }
  }

  if (verbose) {
    message("\n--- GPS Burst Collapse Summary ---")
    message(sprintf("  Method        : %s", method))
    message(sprintf("  Burst size    : %d fixes", burst_size))
    message(sprintf("  Max gap       : %g seconds", max_gap_sec))
    message(sprintf("  Bursts found  : %d", as.integer(n_bursts)))
    message(sprintf("  Rows removed  : %d",
                    as.integer(n_bursts) * (burst_size - 1L)))
    message(sprintf("  Input rows    : %d", nrow(df)))
    message(sprintf("  Output rows   : %d", nrow(out)))
  }

  out
}


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

#' @noRd
#' Parse datetime strings trying multiple formats, return POSIXct UTC.
.parse_dt_multi <- function(x) {
  fmts <- c(
    "%Y-%m-%dT%H:%M:%SZ",   # ISO 8601 with Z
    "%Y-%m-%dT%H:%M:%S",    # ISO 8601 without Z
    "%Y-%m-%d %H:%M:%S",    # standard
    "%Y-%m-%d %H:%M",       # minute precision
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M"
  )
  result    <- rep(as.POSIXct(NA_real_, tz = "UTC"), length(x))
  remaining <- seq_along(x)
  for (fmt in fmts) {
    if (length(remaining) == 0L) break
    parsed <- suppressWarnings(
      as.POSIXct(x[remaining], format = fmt, tz = "UTC"))
    ok              <- !is.na(parsed)
    result[remaining[ok]] <- parsed[ok]
    remaining       <- remaining[!ok]
  }
  result
}


#' @noRd
#' Return the maximum number of decimal places seen in a character/numeric vector.
.max_decimals <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x) & grepl("\\.", x)]
  if (length(x) == 0L) return(0L)
  # cap sample for speed
  if (length(x) > 200L) x <- x[seq(1L, length(x), length.out = 200L)]
  dec <- nchar(sub("^[^.]*\\.", "", x))
  as.integer(max(dec, na.rm = TRUE))
}
