# ==============================================================================
#' Detect and repair corrupted timestamps in GPS sequences
#'
#' Some tags occasionally write a corrupted timestamp inside a GPS burst, so the
#' recorded time steps backwards even though the fixes themselves are sound.
#' A typical case looks like this (seconds only):
#'
#' \preformatted{
#'   19:20:08   lon 34.471325
#'   19:20:09   lon 34.471455
#'   19:20:10   lon 34.471592
#'   19:20:09   lon 34.471638   <- time goes back; position keeps advancing
#'   19:20:12   lon 34.471958
#' }
#'
#' The position moves smoothly throughout, so the row order is correct and only
#' the timestamp is wrong — here it should read \code{19:20:11}. This function
#' finds such rows, \strong{verifies from the coordinates} that the fix sequence
#' really is continuous, and then rewrites only the faulty timestamps. Rows are
#' never reordered and coordinates are never modified.
#'
#' Only \code{GPS} → \code{GPS} steps within one individual are considered. The
#' normal case of a GPS timestamp falling slightly after the following
#' \code{ACC_START} (acquisition lag) is not a fault and is ignored.
#'
#' @param df A data frame from \code{\link{glosendas_download}}.
#' @param max_jump_m Numeric. A repair is accepted only if the fix stays within
#'   this distance (metres) of its temporally adjacent neighbours. Cases that
#'   fail are reported but left untouched, because a position jump means the
#'   problem is not merely a clock fault. Distance discriminates far better than
#'   speed here: inside a 1 Hz burst a few tens of metres of ordinary GPS noise
#'   already implies a large apparent speed. Default: \code{1000}.
#' @param max_gap_sec Numeric. Only inspect inversions inside a fix sequence
#'   where neighbouring fixes are at most this many seconds apart, i.e. within a
#'   GPS burst rather than across a long gap. Default: \code{60}.
#' @param forward_only Logical. Only ever move a timestamp \emph{forward}.
#'   A corrupted reading arises when the clock's seconds field fails to
#'   increment, so the recorded time is always behind the true one and a
#'   correction can only be an advance. Leaving this \code{TRUE} (the default)
#'   guarantees no fix is ever moved to an earlier time. Default: \code{TRUE}.
#' @param update_cols Logical. Write the repaired times back into every time
#'   column present (\code{UTC_timestamp}, \code{UTC_datetime}, \code{UTC_date},
#'   \code{UTC_time}, \code{milliseconds}). When \code{FALSE} the data frame is
#'   returned unchanged and only the report is produced. Default: \code{TRUE}.
#' @param verbose Logical. Print a summary. Default: \code{TRUE}.
#'
#' @return The data frame, with repaired timestamps when \code{update_cols =
#'   TRUE}. A report of every case found is attached as the attribute
#'   \code{"gps_time_repairs"} and can be retrieved with
#'   \code{attr(x, "gps_time_repairs")}. The report has one row per case:
#'   \describe{
#'     \item{row}{row number in the input data frame}
#'     \item{tag_name}{individual}
#'     \item{old_time, new_time}{timestamp before and after repair}
#'     \item{shift_sec}{seconds added}
#'     \item{dist_prev_m, dist_next_m}{distance to the neighbouring fixes}
#'     \item{max_speed_kmh}{highest implied speed at this fix after repair}
#'     \item{repaired}{whether the repair was applied}
#'     \item{reason}{why it was rejected, when applicable}
#'   }
#'
#' @examples
#' \dontrun{
#' df <- glosendas_download("user", "pass", filter_word = "Ezuz")
#'
#' # Inspect without changing anything
#' invisible(fix_gps_time_order(df, update_cols = FALSE))
#'
#' # Repair, then look at what changed
#' df  <- fix_gps_time_order(df)
#' rep <- attr(df, "gps_time_repairs")
#' subset(rep, !repaired)     # cases needing a manual look
#' }
#'
#' @export
fix_gps_time_order <- function(df,
                               max_jump_m    = 1000,
                               max_gap_sec   = 60,
                               forward_only  = TRUE,
                               update_cols   = TRUE,
                               verbose       = TRUE) {

  # ── guards ──────────────────────────────────────────────────────────────────
  if (!inherits(df, "data.frame")) stop("`df` must be a data frame.")
  if (!identical(class(df), "data.frame")) df <- as.data.frame(df)
  if (nrow(df) == 0) stop("`df` has zero rows.")
  if (!"datatype" %in% names(df)) stop("Missing required column: datatype")

  lat_col <- if ("Latitude"  %in% names(df)) "Latitude"  else
             if ("latitude"  %in% names(df)) "latitude"  else NA_character_
  lon_col <- if ("Longitude" %in% names(df)) "Longitude" else
             if ("longitude" %in% names(df)) "longitude" else NA_character_
  has_xy  <- !is.na(lat_col) && !is.na(lon_col)

  ts     <- .gl_best_timestamp(df)
  ts_num <- as.numeric(ts)
  cls    <- .gl_classify(df$datatype)
  is_gps <- cls$gps
  n      <- nrow(df)

  tag <- if ("tag_name" %in% names(df)) as.character(df$tag_name)
         else rep("", n)

  lat <- if (has_xy) .gl_as_num(df[[lat_col]]) else rep(NA_real_, n)
  lon <- if (has_xy) .gl_as_num(df[[lon_col]]) else rep(NA_real_, n)

  # ── locate contiguous GPS runs belonging to one individual ─────────────────
  gps_idx <- which(is_gps)
  if (length(gps_idx) < 3L) {
    if (verbose) message("Fewer than 3 GPS rows — nothing to check.")
    attr(df, "gps_time_repairs") <- .gl_empty_repair_report()
    return(df)
  }

  # A run breaks when the rows are not adjacent, or the tag changes
  brk <- c(TRUE, diff(gps_idx) != 1L | tag[gps_idx[-1]] != tag[gps_idx[-length(gps_idx)]])
  run <- cumsum(brk)

  ts_fixed <- ts_num
  rep_rows <- integer(0); rep_old <- rep_new <- numeric(0)
  rep_dp <- rep_dn <- rep_spd <- numeric(0)
  rep_ok <- logical(0);  rep_why <- character(0); rep_how <- character(0)

  for (r in unique(run)) {
    ridx <- gps_idx[run == r]
    if (length(ridx) < 3L) next
    t <- ts_fixed[ridx]

    # Walk the run; a strictly decreasing step marks a corrupted timestamp
    j <- 2L
    while (j <= length(t)) {
      if (is.na(t[j]) || is.na(t[j - 1L]) || t[j] > t[j - 1L]) { j <- j + 1L; next }

      # ── 1. Can the recorded values simply be put back in order? ─────────────
      # A common fault is a transposition: the tag wrote two neighbouring
      # timestamps the wrong way round. The recorded values are then all
      # correct and only their assignment to rows is wrong, so reordering them
      # repairs the sequence without inventing any time. This is preferred
      # whenever it works, because it is the smaller and better-evidenced
      # correction.
      a      <- j - 1L
      method <- NA_character_
      win    <- integer(0); newv <- numeric(0)

      for (b in j:min(j + 3L, length(t))) {
        vals <- t[a:b]
        if (anyNA(vals)) next
        sv <- sort(vals)
        if (any(diff(sv) <= 0)) next                       # would leave ties
        if (a > 1L && !is.na(t[a - 1L]) && sv[1] <= t[a - 1L]) next
        if (b < length(t) && !is.na(t[b + 1L]) && sv[length(sv)] >= t[b + 1L]) next
        if (max(sv) - min(sv) > max_gap_sec) next          # not one sequence
        # A reordering necessarily moves at least one timestamp backwards.
        # Under forward_only that is not an admissible repair.
        if (forward_only && any(sv < vals)) next
        win <- a:b; newv <- sv; method <- "reordered"
        break
      }

      # ── 2. Otherwise infer the missing time ────────────────────────────────
      if (is.na(method)) {
        # Find the next timestamp that is genuinely ahead of t[j-1]
        k <- j + 1L
        while (k <= length(t) && (is.na(t[k]) || t[k] <= t[j - 1L])) k <- k + 1L

        lo <- t[j - 1L]
        # Typical sampling step of this run. Only gaps short enough to be
        # inside a burst count: a run can also contain long routine-schedule
        # gaps, and those would otherwise dominate the median.
        d_ok <- diff(t[seq_len(j - 1L)])
        d_ok <- d_ok[d_ok > 0 & d_ok <= max_gap_sec]
        step <- if (length(d_ok)) stats::median(d_ok, na.rm = TRUE) else NA_real_

        # Interpolate only when the next sound fix is close enough in time to
        # belong to the same sequence; otherwise carry on at the usual step.
        if (k <= length(t) && !is.na(t[k]) && (t[k] - lo) <= max_gap_sec) {
          hi   <- t[k]
          span <- k - (j - 1L)                       # steps to fill
          newv <- lo + (hi - lo) * seq_len(span - 1L) / span
        } else {
          k    <- min(k, length(t) + 1L)
          newv <- if (is.finite(step) && step > 0) lo + step * seq_len(k - j)
                  else rep(NA_real_, k - j)
        }
        win    <- j:(k - 1L)
        method <- "inferred"
      }

      changed <- which(newv != t[win] | (is.na(newv) & !is.na(t[win])))
      for (m in changed) {
        p   <- win[m]
        row <- ridx[p]
        nt  <- newv[m]

        # Verify from the coordinates that this really is only a clock fault.
        # A neighbouring fix is only informative about continuity if it is
        # close in time, so distant neighbours are not used as evidence.
        dp <- dn <- NA_real_; spd <- NA_real_; ok <- TRUE; why <- ""
        if (!is.finite(nt)) {
          ok  <- FALSE
          why <- "no neighbouring fix close enough to infer the time from"
        } else if (has_xy && !is.na(lat[row]) && !is.na(lon[row])) {
          prow  <- if (p > 1L) ridx[p - 1L] else NA_integer_
          nrow_ <- if (p < length(t)) ridx[p + 1L] else NA_integer_

          dt_p <- if (!is.na(prow)) nt - ts_fixed[prow] else NA_real_
          dt_n <- if (!is.na(nrow_)) ts_fixed[nrow_] - nt else NA_real_

          if (!is.na(prow) && !is.na(lat[prow]) &&
              !is.na(dt_p) && abs(dt_p) <= max_gap_sec)
            dp <- .gl_haversine_m(lat[prow], lon[prow], lat[row], lon[row])
          if (!is.na(nrow_) && !is.na(lat[nrow_]) &&
              !is.na(dt_n) && abs(dt_n) <= max_gap_sec)
            dn <- .gl_haversine_m(lat[row], lon[row], lat[nrow_], lon[nrow_])

          s1 <- if (!is.na(dp) && !is.na(dt_p) && dt_p > 0) dp / dt_p * 3.6 else NA_real_
          s2 <- if (!is.na(dn) && !is.na(dt_n) && dt_n > 0) dn / dt_n * 3.6 else NA_real_
          spd <- suppressWarnings(max(c(s1, s2), na.rm = TRUE))
          if (!is.finite(spd)) spd <- NA_real_

          worst <- suppressWarnings(max(c(dp, dn), na.rm = TRUE))
          if (is.finite(worst) && worst > max_jump_m) {
            ok  <- FALSE
            why <- sprintf("position jumps %.0f m — not a clock fault", worst)
          }
        }

        rep_rows <- c(rep_rows, row)
        rep_old  <- c(rep_old,  t[p])
        rep_new  <- c(rep_new,  nt)
        rep_dp   <- c(rep_dp,   dp)
        rep_dn   <- c(rep_dn,   dn)
        rep_spd  <- c(rep_spd,  spd)
        rep_ok   <- c(rep_ok,   ok)
        rep_why  <- c(rep_why,  why)
        rep_how  <- c(rep_how,  method)

        if (ok) { t[p] <- nt; ts_fixed[row] <- nt }
      }
      # Continue after the window just handled; always advance at least one row
      j <- max(win[length(win)] + 1L, j + 1L)
    }
  }

  report <- data.frame(
    row           = rep_rows,
    tag_name      = tag[rep_rows],
    old_time      = .gl_posix(rep_old),
    new_time      = .gl_posix(rep_new),
    shift_sec     = round(rep_new - rep_old, 3),
    dist_prev_m   = round(rep_dp, 1),
    dist_next_m   = round(rep_dn, 1),
    max_speed_kmh = round(rep_spd, 1),
    method        = rep_how,
    repaired      = rep_ok,
    reason        = rep_why,
    stringsAsFactors = FALSE
  )
  if (nrow(report) == 0) report <- .gl_empty_repair_report()
  rownames(report) <- NULL

  # ── write repaired times back ───────────────────────────────────────────────
  n_fixed <- sum(report$repaired)
  if (update_cols && n_fixed > 0L) {
    chg <- report$row[report$repaired]
    new <- .gl_posix(ts_fixed[chg])

    if ("UTC_timestamp" %in% names(df)) {
      if (inherits(df$UTC_timestamp, "POSIXct") &&
          is.numeric(unclass(df$UTC_timestamp))) {
        df$UTC_timestamp[chg] <- new
      } else {
        ms <- round((as.numeric(new) %% 1) * 1000)
        df$UTC_timestamp[chg] <- ifelse(
          ms == 0, format(new, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
          format(new, "%Y-%m-%d %H:%M:%OS3", tz = "UTC"))
      }
    }
    if ("UTC_datetime" %in% names(df)) {
      whole <- .gl_posix(floor(as.numeric(new)))
      if (inherits(df$UTC_datetime, "POSIXct") &&
          is.numeric(unclass(df$UTC_datetime))) {
        df$UTC_datetime[chg] <- whole
      } else {
        df$UTC_datetime[chg] <- format(whole, "%Y-%m-%d %H:%M:%S", tz = "UTC")
      }
    }
    if ("UTC_date" %in% names(df)) {
      dt <- as.Date(format(new, "%Y-%m-%d", tz = "UTC"))
      df$UTC_date[chg] <- if (inherits(df$UTC_date, "Date")) dt else as.character(dt)
    }
    if ("UTC_time" %in% names(df))
      df$UTC_time[chg] <- format(new, "%H:%M:%S", tz = "UTC")
    if ("milliseconds" %in% names(df))
      df$milliseconds[chg] <- round((as.numeric(new) %% 1) * 1000)
  }

  attr(df, "gps_time_repairs") <- report

  if (verbose) {
    message("\n--- GPS Timestamp Repair ---")
    message(sprintf("  GPS rows inspected   : %d", length(gps_idx)))
    message(sprintf("  Corrupted timestamps : %d", nrow(report)))
    if (nrow(report) > 0) {
      message(sprintf("  Repaired             : %d", n_fixed))
      if (any(!report$repaired))
        message(sprintf("  Left untouched       : %d (see attr(x, 'gps_time_repairs'))",
                        sum(!report$repaired)))
      if (!update_cols)
        message("  update_cols = FALSE  : data frame returned unchanged")
      sh <- report$shift_sec[report$repaired]
      if (length(sh))
        message(sprintf("  Shift applied        : %+.1f to %+.1f s (median %+.1f)",
                        min(sh), max(sh), stats::median(sh)))
      show <- utils::head(report, 5)
      message("\n  ", sprintf("%-9s %-21s %-21s %9s %8s",
                              "row", "old_time", "new_time", "shift_s", "ok"))
      for (i in seq_len(nrow(show)))
        message("  ", sprintf("%-9d %-21s %-21s %9.2f %8s",
                show$row[i],
                format(show$old_time[i], "%Y-%m-%d %H:%M:%OS3"),
                format(show$new_time[i], "%Y-%m-%d %H:%M:%OS3"),
                show$shift_sec[i], show$repaired[i]))
      if (nrow(report) > 5) message("  ... ", nrow(report) - 5, " more")
    }
  }
  df
}


#' @noRd
.gl_posix <- function(x) as.POSIXct(x, origin = "1970-01-01", tz = "UTC")

#' @noRd
.gl_empty_repair_report <- function() {
  data.frame(row = integer(), tag_name = character(),
             old_time = .gl_posix(numeric()), new_time = .gl_posix(numeric()),
             shift_sec = numeric(), dist_prev_m = numeric(),
             dist_next_m = numeric(), max_speed_kmh = numeric(),
             method = character(), repaired = logical(), reason = character(),
             stringsAsFactors = FALSE)
}

#' @noRd
#' Great-circle distance in metres.
.gl_haversine_m <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
       cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * 6371000 * asin(pmin(1, sqrt(a)))
}
