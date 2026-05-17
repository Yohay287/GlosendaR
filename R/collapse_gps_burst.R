# ==============================================================================
#' Collapse GPS bursts into single representative rows
#'
#' Identifies GPS bursts — runs of exactly \code{burst_size} consecutive GPS
#' fixes each separated by at most \code{max_gap_sec} seconds — and collapses
#' each burst into one representative row. All other rows (non-GPS, ACC bursts,
#' flight detection sequences of a different length) are left completely
#' untouched.
#'
#' The function is fully vectorised and runs efficiently on datasets of any
#' size.
#'
#' @param df A data frame containing GPS tracking data, as returned by
#'   \code{\link{glosendas_download}} or loaded from a Glosendas CSV.
#'   Must contain columns \code{datatype} and \code{UTC_datetime}.
#' @param burst_size Integer >= 2. The exact number of fixes that constitute
#'   one GPS burst. Default: \code{5}.
#' @param method Character. How to represent the collapsed burst:
#'   \itemize{
#'     \item \code{"mean"}  — mean of all numeric columns, rounded to the
#'       same decimal places as the raw data
#'     \item \code{"first"} — first fix of the burst (default)
#'     \item \code{"last"}  — last fix of the burst
#'   }
#' @param max_gap_sec Numeric. Maximum seconds between consecutive GPS fixes
#'   still considered part of the same burst. Default: \code{2}.
#' @param verbose Logical. Print a processing summary. Default: \code{TRUE}.
#'
#' @return A data frame with the same columns as \code{df}. Each GPS burst of
#'   exactly \code{burst_size} rows is replaced by a single representative row.
#'
#' @examples
#' \dontrun{
#' df <- glosendas_download("myuser", "mypass", filter_word = "Houbara")
#'
#' # Detect burst size first
#' info <- detect_gps_burst(df)
#'
#' # Collapse using the detected size
#' df_c <- collapse_gps_burst(df, burst_size = info$burst_size)
#'
#' # Use first fix instead of mean
#' df_c <- collapse_gps_burst(df, burst_size = 5, method = "first")
#' }
#'
#' @export
collapse_gps_burst <- function(df,
                               burst_size  = 5,
                               method      = "first",
                               max_gap_sec = 2,
                               verbose     = TRUE) {

  # ── guards ───────────────────────────────────────────────────────────────────
  if (!is.data.frame(df))
    stop("`df` must be a data frame.")
  if (nrow(df) == 0)
    stop("`df` has zero rows.")
  # burst_size can be a vector (e.g. from detect_gps_burst()$collapse_sizes)
  # If multiple sizes supplied, apply largest first then descend
  burst_size <- as.integer(round(as.numeric(burst_size)))
  if (any(is.na(burst_size)) || any(burst_size < 2))
    stop("`burst_size` must contain integers >= 2.")

  if (length(burst_size) > 1L) {
    if (verbose)
      message(sprintf("Multiple burst sizes supplied: %s — applying largest first.",
                      paste(sort(burst_size, decreasing = TRUE), collapse = ", ")))
    for (sz in sort(burst_size, decreasing = TRUE)) {
      df <- collapse_gps_burst(df, burst_size = sz, method = method,
                               max_gap_sec = max_gap_sec, verbose = verbose)
    }
    return(df)
  }

  burst_size <- burst_size[1L]
  if (!method %in% c("first", "last", "mean"))
    stop('`method` must be "first", "last", or "mean".')
  if (!is.numeric(max_gap_sec) || length(max_gap_sec) != 1 || max_gap_sec < 0)
    stop("`max_gap_sec` must be a non-negative number.")

  miss <- setdiff(c("datatype", "UTC_datetime"), names(df))
  if (length(miss))
    stop("Missing required columns: ", paste(miss, collapse = ", "))

  burst_size <- as.integer(burst_size)

  # ── parse datetime ───────────────────────────────────────────────────────────
  dt <- .parse_dt_multi(df$UTC_datetime)

  # ── identify GPS rows ────────────────────────────────────────────────────────
  is_gps <- !is.na(df$datatype) & trimws(df$datatype) == "GPS"

  if (!any(is_gps)) {
    if (verbose) message("No GPS rows found — returning df unchanged.")
    return(df)
  }

  # ── vectorised burst detection ───────────────────────────────────────────────
  # Strategy: work on the full row index.
  # For each row, compute the gap to the NEXT row (in seconds).
  # A gap is a "burst gap" only when:
  #   - current row is GPS
  #   - next row is GPS
  #   - gap is between 1 and max_gap_sec seconds
  n <- nrow(df)

  # Gap vector aligned to row i = gap between row i and row i+1
  dt_num <- as.numeric(dt)   # seconds since epoch; NA for non-parseable

  gap_to_next <- c(dt_num[-1] - dt_num[-n], NA_real_)
  gap_rounded <- round(gap_to_next)

  both_gps <- is_gps & c(is_gps[-1], FALSE)   # row i AND row i+1 are GPS

  is_burst_gap <- both_gps &
    !is.na(gap_rounded) &
    gap_rounded >= 1L &
    gap_rounded <= max_gap_sec

  # RLE on the burst-gap indicator
  rle_bg    <- rle(is_burst_gap)
  rv        <- rle_bg$values
  rl        <- rle_bg$lengths

  # End position (row index) of each run
  run_ends   <- cumsum(rl)
  run_starts <- c(1L, run_ends[-length(run_ends)] + 1L)

  # TRUE runs of length k = k consecutive burst gaps = k+1 consecutive GPS fixes
  # A burst of exactly burst_size fixes has exactly burst_size-1 burst gaps
  burst_gap_count <- burst_size - 1L
  burst_run_idx   <- which(rv & rl == burst_gap_count)

  n_bursts <- length(burst_run_idx)

  if (n_bursts == 0L) {
    if (verbose)
      message(sprintf(
        "No GPS bursts of exactly %d fixes found — returning df unchanged.",
        burst_size))
    return(df)
  }

  # Row indices of the first and last fix of each burst
  burst_first_row <- run_starts[burst_run_idx]          # first gap starts here
  burst_last_row  <- run_ends[burst_run_idx] + 1L       # last fix is one beyond

  # ── detect decimal places per numeric column (vectorised, sampled) ───────────
  gps_rows_idx <- which(is_gps)
  sample_idx   <- if (length(gps_rows_idx) > 500L)
    gps_rows_idx[seq(1L, length(gps_rows_idx), length.out = 500L)]
  else gps_rows_idx

  col_dec <- vapply(names(df), function(cn)
    .max_decimals(df[[cn]][sample_idx]), integer(1L))
  names(col_dec) <- names(df)

  # Datetime columns: always take first fix, never average
  dt_col_names <- intersect(
    c("UTC_datetime", "UTC_date", "UTC_time", "UTC_timestamp", "UTC_precise"),
    names(df)
  )

  # ── build a logical mask: which rows to KEEP (fully vectorised) ──────────────
  # Generate all row indices to DROP across all bursts at once,
  # then set them FALSE in a single assignment.
  keep <- rep(TRUE, n)

  if (method == "last") {
    # Drop first (burst_size-1) rows of each burst
    drop_starts <- burst_first_row
    drop_ends   <- burst_last_row - 1L
  } else {
    # first or mean: drop rows 2..burst_size of each burst
    drop_starts <- burst_first_row + 1L
    drop_ends   <- burst_last_row
  }

  # Expand ranges to a single integer vector and set FALSE in one shot
  drop_idx <- unlist(Map(seq, drop_starts, drop_ends), use.names = FALSE)
  keep[drop_idx] <- FALSE

  # Slice df once
  out           <- df[keep, , drop = FALSE]
  rownames(out) <- NULL

  # ── apply mean patches ───────────────────────────────────────────────────────
  if (method == "mean" && n_bursts > 0L) {
    # Map original burst_first_row positions to output row positions
    # keep is a logical vector; cumsum(keep) gives the output row index
    keep_cumsum   <- cumsum(keep)
    out_row_of    <- keep_cumsum[burst_first_row]   # output row for each burst's first fix

    for (cn in names(df)) {
      if (cn %in% dt_col_names) next   # datetime: keep first row value

      # Try converting column to numeric
      col_vals <- suppressWarnings(as.numeric(df[[cn]]))
      if (all(is.na(col_vals[is_gps]))) next   # entirely non-numeric for GPS rows

      dec <- col_dec[cn]

      # Compute means for all bursts at once using matrix approach
      # Extract burst values into a matrix (burst_size rows x n_bursts cols)
      # then colMeans
      burst_indices <- outer(0L:(burst_size - 1L), burst_first_row, `+`)
      # burst_indices is burst_size x n_bursts matrix of original row indices
      burst_vals    <- matrix(col_vals[burst_indices], nrow = burst_size)
      burst_means   <- colMeans(burst_vals, na.rm = TRUE)

      # Round to original decimal precision
      rounded <- if (dec == 0L) {
        as.character(round(burst_means))
      } else {
        format(round(burst_means, dec), nsmall = dec, trim = TRUE)
      }

      # Assign to output rows (vectorised)
      out[[cn]][out_row_of] <- rounded
    }
  }

  if (verbose) {
    message("\n--- GPS Burst Collapse Summary ---")
    message(sprintf("  Method        : %s", method))
    message(sprintf("  Burst size    : %d fixes", burst_size))
    message(sprintf("  Max gap       : %g seconds", max_gap_sec))
    message(sprintf("  Bursts found  : %d", n_bursts))
    message(sprintf("  Rows removed  : %d", n_bursts * (burst_size - 1L)))
    message(sprintf("  Input rows    : %d", nrow(df)))
    message(sprintf("  Output rows   : %d", nrow(out)))
  }

  out
}


# ==============================================================================
# INTERNAL HELPERS (shared with detect_gps_burst.R)
# ==============================================================================

#' @noRd
.parse_dt_multi <- function(x) {
  fmts <- c(
    "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S",  "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%S",  "%d/%m/%Y %H:%M"
  )
  result    <- rep(as.POSIXct(NA_real_, tz = "UTC"), length(x))
  remaining <- seq_along(x)
  for (fmt in fmts) {
    if (!length(remaining)) break
    parsed <- suppressWarnings(as.POSIXct(x[remaining], format = fmt, tz = "UTC"))
    ok               <- !is.na(parsed)
    result[remaining[ok]] <- parsed[ok]
    remaining        <- remaining[!ok]
  }
  result
}


#' @noRd
.max_decimals <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x) & grepl("\\.", x)]
  if (!length(x)) return(0L)
  if (length(x) > 200L) x <- x[seq(1L, length(x), length.out = 200L)]
  as.integer(max(nchar(sub("^[^.]*\\.", "", x)), na.rm = TRUE))
}
