# ==============================================================================
#' Detect GPS burst settings from a tracking dataset
#'
#' Analyses the pattern of GPS fixes to automatically detect programmed GPS
#' bursts — short sequences of fixes recorded every second at a regular
#' interval. Flight detection sequences are distinguished from bursts because
#' they appear only rarely (fewer than \code{min_sequences} times).
#'
#' \strong{Detection is fully vectorised} and runs in milliseconds regardless
#' of dataset size.
#'
#' \strong{Detection logic:}
#' \enumerate{
#'   \item Compute inter-fix gaps (seconds, rounded) between all consecutive
#'     GPS rows.
#'   \item Run-length-encode the gap vector to find all runs of consecutive
#'     1-second gaps.
#'   \item A run of \emph{k} consecutive 1-second gaps = a sequence of
#'     \emph{k+1} fixes; count how often each sequence length occurs.
#'   \item Any length appearing \code{>= min_sequences} times is a
#'     \emph{burst candidate}. The most frequent candidate is the
#'     \strong{dominant burst size}.
#'   \item Shorter candidates are flagged as \emph{truncated bursts}
#'     (partial sequences from missed fixes or dataset edges).
#'   \item Sequences appearing fewer than \code{min_sequences} times are
#'     reported separately as likely flight or noise events.
#' }
#'
#' @param df A data frame from \code{\link{glosendas_download}} or a loaded
#'   Glosendas CSV. Must contain \code{datatype} and \code{UTC_datetime}.
#' @param min_sequences Integer >= 1. Minimum number of times a run length
#'   must appear to be considered a valid burst size. Filters out flight
#'   sequences which typically appear only once or a few times. Default: 10.
#' @param max_gap_sec Numeric >= 1. Maximum seconds between consecutive fixes
#'   still considered part of the same burst (allows for occasional
#'   clock-rounding). Default: 2.
#' @param include_truncated Logical. If \code{TRUE}, truncated burst sizes
#'   (candidates smaller than the dominant size) are included in
#'   \code{collapse_sizes} for removal. Default: \code{FALSE}.
#' @param verbose Logical. Print the full detection report. Default: \code{TRUE}.
#'
#' @return A named list (invisibly):
#'   \itemize{
#'     \item \code{burst_size}        — dominant burst size in fixes
#'     \item \code{n_bursts}          — number of dominant bursts detected
#'     \item \code{burst_pct}         — \% of GPS fixes inside dominant bursts
#'     \item \code{interval_summary}  — list with median/mean/sd/min/max of
#'       inter-burst intervals in minutes
#'     \item \code{collapse_sizes}    — integer vector of burst sizes to pass
#'       to \code{\link{collapse_gps_burst}}; contains only the dominant size
#'       unless \code{include_truncated = TRUE}
#'     \item \code{include_truncated} — the value of the parameter used
#'     \item \code{all_candidates}    — data frame of all candidate sizes
#'     \item \code{all_run_lengths}   — data frame of every run length found
#'     \item \code{total_gps_fixes}   — total GPS fixes in the dataset
#'   }
#'
#' @examples
#' \dontrun{
#' df <- glosendas_download("myuser", "mypass", filter_word = "Houbara")
#'
#' # Detect dominant burst size
#' info <- detect_gps_burst(df)
#'
#' # Include truncated bursts in collapsing
#' info <- detect_gps_burst(df, include_truncated = TRUE)
#'
#' # Pipe result directly into collapse_gps_burst()
#' df_c <- collapse_gps_burst(df, burst_size = info$burst_size)
#'
#' # Collapse all sizes (dominant + truncated), largest first
#' for (sz in rev(info$collapse_sizes)) {
#'   df_c <- collapse_gps_burst(df_c, burst_size = sz)
#' }
#' }
#'
#' @export
detect_gps_burst <- function(df,
                             min_sequences     = 10,
                             max_gap_sec       = 2,
                             include_truncated = FALSE,
                             verbose           = TRUE) {

  # ── guards ───────────────────────────────────────────────────────────────────
  if (!is.data.frame(df))
    stop("`df` must be a data frame.")
  if (nrow(df) == 0)
    stop("`df` has zero rows.")
  if (!is.numeric(min_sequences) || length(min_sequences) != 1 ||
      min_sequences < 1 || is.na(min_sequences))
    stop("`min_sequences` must be a single positive number.")
  if (!is.numeric(max_gap_sec) || length(max_gap_sec) != 1 ||
      max_gap_sec < 1 || is.na(max_gap_sec))
    stop("`max_gap_sec` must be a single number >= 1.")
  if (!is.logical(include_truncated) || length(include_truncated) != 1 ||
      is.na(include_truncated))
    stop("`include_truncated` must be TRUE or FALSE.")

  miss <- setdiff(c("datatype", "UTC_datetime"), names(df))
  if (length(miss))
    stop("Missing required columns: ", paste(miss, collapse = ", "))

  # ── filter GPS rows ───────────────────────────────────────────────────────────
  is_gps <- !is.na(df$datatype) & trimws(df$datatype) == "GPS"
  if (!any(is_gps)) {
    if (verbose) message("No GPS rows found in `df`.")
    return(invisible(NULL))
  }

  gps_df <- df[is_gps, , drop = FALSE]

  # ── parse datetime ────────────────────────────────────────────────────────────
  dt <- .parse_dt_multi(gps_df$UTC_datetime)
  dt <- dt[!is.na(dt)]

  if (length(dt) < 4) {
    if (verbose)
      message("Fewer than 4 parseable GPS timestamps — cannot detect bursts.")
    return(invisible(NULL))
  }

  n_gps <- length(dt)

  # ── inter-fix gaps (fully vectorised) ────────────────────────────────────────
  gaps <- round(as.numeric(dt[-1] - dt[-length(dt)]))

  # ── run-length encoding of burst-gap indicator ────────────────────────────────
  # A gap is a "burst gap" if it is between 1 and max_gap_sec seconds.
  is_burst_gap <- !is.na(gaps) & gaps >= 1L & gaps <= max_gap_sec

  rle_gaps  <- rle(is_burst_gap)
  run_vals  <- rle_gaps$values
  run_lens  <- rle_gaps$lengths

  # Each TRUE run of length k = k consecutive 1-s gaps = k+1 fixes
  burst_fix_counts <- run_lens[run_vals] + 1L

  if (length(burst_fix_counts) == 0) {
    if (verbose) message("No consecutive 1-second GPS sequences found.")
    return(invisible(NULL))
  }

  # ── count occurrences of each burst size ──────────────────────────────────────
  len_table  <- sort(table(burst_fix_counts), decreasing = TRUE)
  candidates <- len_table[len_table >= min_sequences]

  if (length(candidates) == 0) {
    if (verbose) {
      message(sprintf(
        "No sequence length appears >= %d times.\n  All lengths found:", min_sequences))
      print(len_table)
    }
    return(invisible(NULL))
  }

  # Candidates data frame
  cand_df <- data.frame(
    burst_size  = as.integer(names(candidates)),
    n_sequences = as.integer(candidates),
    stringsAsFactors = FALSE
  )
  cand_df$fixes_in_bursts <- cand_df$burst_size * cand_df$n_sequences
  cand_df$pct_of_gps      <- round(100 * cand_df$fixes_in_bursts / n_gps, 1)
  cand_df <- cand_df[order(cand_df$n_sequences, decreasing = TRUE), ]
  rownames(cand_df) <- NULL

  dom_size  <- cand_df$burst_size[1]
  dom_count <- cand_df$n_sequences[1]
  dom_fixes <- dom_size * dom_count
  dom_pct   <- round(100 * dom_fixes / n_gps, 1)

  # ── inter-burst intervals (vectorised) ───────────────────────────────────────
  run_ends   <- cumsum(run_lens)
  run_starts <- c(1L, run_ends[-length(run_ends)] + 1L)

  dom_run_idx <- which(run_vals & (run_lens + 1L) == dom_size)

  interval_summary <- NULL
  if (length(dom_run_idx) >= 2) {
    burst_start_times <- dt[run_starts[dom_run_idx]]
    burst_end_times   <- dt[run_ends[dom_run_idx] + 1L]
    inter_sec <- as.numeric(
      burst_start_times[-1] - burst_end_times[-length(burst_end_times)])
    inter_sec <- inter_sec[!is.na(inter_sec) & inter_sec > 0]

    if (length(inter_sec) > 0) {
      interval_summary <- list(
        median_min = round(stats::median(inter_sec) / 60, 1),
        mean_min   = round(mean(inter_sec) / 60, 1),
        sd_min     = round(stats::sd(inter_sec) / 60, 1),
        min_min    = round(min(inter_sec) / 60, 1),
        max_min    = round(max(inter_sec) / 60, 1),
        n          = length(inter_sec)
      )
    }
  }

  # ── collapse_sizes ────────────────────────────────────────────────────────────
  truncated_sizes <- cand_df$burst_size[cand_df$burst_size < dom_size]
  collapse_sizes  <- if (include_truncated && length(truncated_sizes) > 0)
    sort(c(dom_size, truncated_sizes))
  else
    dom_size

  # ── result list ───────────────────────────────────────────────────────────────
  result <- list(
    burst_size        = dom_size,
    n_bursts          = dom_count,
    burst_pct         = dom_pct,
    interval_summary  = interval_summary,
    collapse_sizes    = collapse_sizes,
    include_truncated = include_truncated,
    all_candidates    = cand_df,
    all_run_lengths   = as.data.frame(len_table),
    total_gps_fixes   = n_gps
  )

  # ── print report ──────────────────────────────────────────────────────────────
  if (verbose) {
    message("\n========================================")
    message("  GPS Burst Detection Report")
    message("========================================")
    message(sprintf("  Total GPS fixes       : %d", n_gps))
    message(sprintf("  Min sequences filter  : >= %d occurrences", min_sequences))
    message("")

    message("  --- Dominant Burst ---")
    message(sprintf("  Burst size            : %d fixes (1 fix/second)", dom_size))
    message(sprintf("  Number of bursts      : %d", dom_count))
    message(sprintf("  Fixes in bursts       : %d  (%.1f%% of all GPS fixes)",
                    dom_fixes, dom_pct))

    if (!is.null(interval_summary)) {
      message("")
      message("  --- Inter-Burst Interval ---")
      message(sprintf("  Median   : %.1f min", interval_summary$median_min))
      message(sprintf("  Mean     : %.1f min", interval_summary$mean_min))
      message(sprintf("  SD       : %.1f min", interval_summary$sd_min))
      message(sprintf("  Range    : %.1f \u2013 %.1f min",
                      interval_summary$min_min, interval_summary$max_min))
      message(sprintf("  (n = %d intervals)", interval_summary$n))
    }

    if (nrow(cand_df) > 0) {
      message("")
      message("  --- All Candidate Burst Sizes ---")
      message(sprintf("  %-12s  %10s  %14s  %12s",
                      "burst_size", "n_sequences", "fixes_in_bursts", "pct_of_gps"))
      message("  ", strrep("-", 55))
      for (i in seq_len(nrow(cand_df))) {
        r    <- cand_df[i, ]
        flag <- if (r$burst_size == dom_size) " <- dominant" else
                if (r$burst_size < dom_size)  " (truncated?)" else " (longer?)"
        message(sprintf("  %-12d  %10d  %14d  %11.1f%%%s",
                        r$burst_size, r$n_sequences,
                        r$fixes_in_bursts, r$pct_of_gps, flag))
      }
      message("  ", strrep("-", 55))
    }

    excl <- len_table[as.integer(names(len_table)) > 1L &
                        len_table < min_sequences]
    if (length(excl) > 0) {
      message("")
      message(sprintf("  --- Excluded (appear < %d times) ---", min_sequences))
      message(sprintf("  %-12s  %8s  %s", "length", "count", "likely"))
      message("  ", strrep("-", 42))
      for (nm in names(excl)) {
        likely <- if (as.integer(nm) > dom_size * 3L)
          "flight / long event" else "noise / edge"
        message(sprintf("  %-12s  %8d  %s", nm, excl[[nm]], likely))
      }
    }

    message("")
    message("  --- Ready to Use ---")
    if (include_truncated && length(collapse_sizes) > 1) {
      message(sprintf("  Collapse sizes : %s (dominant + truncated)",
                      paste(collapse_sizes, collapse = ", ")))
      message("  Apply largest first:")
      for (sz in rev(collapse_sizes))
        message(sprintf("    df_c <- collapse_gps_burst(df_c, burst_size = %d)", sz))
    } else {
      message(sprintf("  df_c <- collapse_gps_burst(df, burst_size = %d)", dom_size))
    }
    message("========================================\n")
  }

  invisible(result)
}
