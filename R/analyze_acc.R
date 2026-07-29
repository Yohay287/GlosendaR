# ==============================================================================
#' Analyze accelerometer bursts and attach summary statistics to GPS rows
#'
#' Processes a data frame from \code{\link{glosendas_download}} and computes
#' per-burst statistics for each accelerometer axis (X, Y, Z). Works with
#' all six portal formats (V1 and V2). Fully vectorised for performance on
#' large datasets.
#'
#' Rows must be sorted by \code{tag_name} and time; a warning is issued if
#' they are not.
#'
#' @param df Data frame from \code{glosendas_download()}.
#' @param adj_gps_max_min Numeric. A GPS fix is assigned to a burst when it is
#'   exactly one row before \code{ACC_START} and its timestamp is within this
#'   many minutes before OR after the burst start time. Negative differences
#'   (GPS timestamp fractionally after burst start) are valid due to same-second
#'   acquisition lag. The exact gap is stored in \code{gps_to_burst_sec}.
#'   Default: \code{2}.
#' @param include_burst_rows Logical. Keep raw ACC rows. Default: \code{FALSE}.
#' @param advanced Logical. Compute extended metrics. Requires \pkg{moments}.
#'   Default: \code{FALSE}.
#' @param v1_burst_gap_sec Numeric. For V1 SENSORS rows: gap (seconds) that
#'   separates bursts. Default: \code{5}.
#' @param verbose Logical. Print summary. Default: \code{TRUE}.
#'
#' @return Data frame with ACC rows optionally removed and new columns added
#'   to GPS / ACC_SUMMARY rows.
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
                        adj_gps_max_min    = 2,
                        include_burst_rows = FALSE,
                        advanced           = FALSE,
                        v1_burst_gap_sec   = 5,
                        verbose            = TRUE) {

  # ── guards ───────────────────────────────────────────────────────────────────
  if (!inherits(df, "data.frame")) stop("`df` must be a data frame.")
  if (!identical(class(df), "data.frame")) df <- as.data.frame(df)
  if (nrow(df) == 0)             stop("`df` has zero rows.")
  if (adj_gps_max_min < 0)       stop("`adj_gps_max_min` must be >= 0.")
  if (v1_burst_gap_sec < 0)      stop("`v1_burst_gap_sec` must be >= 0.")
  miss <- setdiff(c("datatype","acc_x","acc_y","acc_z"), names(df))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse=", "))
  if (advanced && !requireNamespace("moments", quietly=TRUE))
    stop("Package 'moments' needed for advanced=TRUE.\n",
         "Install: install.packages('moments')")

  # ── drop gap rows ────────────────────────────────────────────────────────────
  if ("device_id" %in% names(df)) {
    bad <- is.na(df$device_id) | trimws(as.character(df$device_id)) == ""
    if (any(bad)) { df <- df[!bad,]; rownames(df) <- NULL }
  }

  # ── coerce ACC only when needed (avoids 3 full copies of large columns) ─────
  df$acc_x <- .gl_as_num(df$acc_x)
  df$acc_y <- .gl_as_num(df$acc_y)
  df$acc_z <- .gl_as_num(df$acc_z)

  # ── precise timestamp: prefer an existing sub-second column ────────────────
  ts_precise   <- .gl_best_timestamp(df)
  burst_ts_num <- as.numeric(ts_precise)

  if ("UTC_datetime" %in% names(df) && !inherits(df$UTC_datetime,"POSIXct"))
    df$UTC_datetime <- .gl_to_posix(df$UTC_datetime)

  .gl_check_order(df, burst_ts_num, "analyze_acc")

  # ── classify datatype once (startsWith/endsWith, no repeated regex) ────────
  cls    <- .gl_classify(df$datatype)
  has_v2 <- any(cls$acc_any)
  has_v1 <- any(cls$sensors)

  if (!has_v2 && !has_v1) {
    if (verbose) message("No ACC rows found.")
    return(df)
  }

  is_acc <- if (has_v2) cls$acc_any else cls$sensors
  is_gps <- cls$gps          # includes GPS_ACC, so re-running is safe

  if (verbose) {
    fmt <- if (has_v2) "V2 (SEN_ACC_*Hz)" else "V1 (SENSORS)"
    message(sprintf("Detecting ACC bursts — format: %s", fmt))
  }

  tag_vec <- if ("tag_name" %in% names(df)) as.character(df$tag_name) else NULL

  # ── find burst boundaries ───────────────────────────────────────────────────
  boundaries <- if (has_v2)
    .acc_bounds_v2(nrow(df), cls, tag_vec)
  else
    .acc_bounds_v1(which(cls$sensors), burst_ts_num, v1_burst_gap_sec)

  n_bursts_raw <- nrow(boundaries)
  if (n_bursts_raw == 0L) {
    if (verbose) message("No ACC bursts found.")
    if (include_burst_rows) return(df) else return(df[!is_acc,])
  }
  if (verbose) message(sprintf("Found %d burst(s)", n_bursts_raw))

  # ── initialise output columns ───────────────────────────────────────────────
  basic_cols <- c("acc_burst_n","acc_freq_hz","acc_duration_sec",
                  "mean_x","sd_x","mean_y","sd_y",
                  "mean_z","sd_z","acc_odba")
  adv_cols <- c(
    "range_x","range_y","range_z",
    "max_x","max_y","max_z",
    "min_x","min_y","min_z",
    "norm_x","norm_y","norm_z",
    "q25_x","q25_y","q25_z",
    "q50_x","q50_y","q50_z",
    "q75_x","q75_y","q75_z",
    "skewness_x","skewness_y","skewness_z",
    "kurtosis_x","kurtosis_y","kurtosis_z",
    "cov_x_y","cov_x_z","cov_y_z",
    "cor_x_y","cor_x_z","cor_y_z",
    "mean_diff_x_y","mean_diff_x_z","mean_diff_y_z",
    "sd_diff_x_y","sd_diff_x_z","sd_diff_y_z",
    "mean_amplitude_x","mean_amplitude_y","mean_amplitude_z"
  )
  all_num_cols <- if (advanced) c(basic_cols, adv_cols) else basic_cols
  for (col in all_num_cols) df[[col]] <- NA_real_
  df$acc_burst_type   <- NA_character_
  df$gps_to_burst_sec <- NA_real_

  gps_idx    <- which(is_gps)
  gps_ts_num <- burst_ts_num[gps_idx]

  # ── per-burst scalars (vectorised) ─────────────────────────────────────────
  n_trunc  <- sum(boundaries$truncated)
  freq_vec <- suppressWarnings(
    as.numeric(sub("^.*_([0-9]+)[Hh]z$", "\\1", boundaries$type)))
  dur_vec  <- abs(burst_ts_num[boundaries$e] - burst_ts_num[boundaries$s])

  # ── GPS matching: findInterval() — binary search for ALL bursts at once ────
  fi        <- findInterval(boundaries$s - 1L, gps_idx)
  has_prev  <- fi > 0L
  time_diff <- ifelse(has_prev,
                      burst_ts_num[boundaries$s] - gps_ts_num[pmax(fi,1L)],
                      NA_real_)

  # GPS fix must belong to the same individual as the burst
  if (!is.null(tag_vec)) {
    burst_tag <- tag_vec[boundaries$s]
    gps_tag   <- tag_vec[gps_idx[pmax(fi, 1L)]]
    has_prev  <- has_prev & (burst_tag == gps_tag)
    time_diff <- ifelse(has_prev, time_diff, NA_real_)
  }

  # Assigned when the GPS row is exactly one row before ACC_START and within
  # adj_gps_max_min minutes before OR after the burst start time. Negative
  # time_diff (GPS timestamp fractionally after burst start) is expected and
  # valid, caused by same-second acquisition lag.
  adj_gps_max_sec <- adj_gps_max_min * 60
  prev_row_is_gps <- has_prev & (gps_idx[pmax(fi,1L)] == boundaries$s - 1L)
  matched         <- prev_row_is_gps & !is.na(time_diff) &
                     time_diff >= -adj_gps_max_sec & time_diff <= adj_gps_max_sec
  target_gps      <- ifelse(matched, gps_idx[pmax(fi,1L)], NA_integer_)

  gps_to_burst_sec <- ifelse(matched, round(time_diff, 2), NA_real_)
  orphan_src       <- which(!matched)

  # ── assign burst ID to every ACC row ───────────────────────────────────────
  burst_len <- boundaries$e - boundaries$s + 1L
  burst_id  <- integer(nrow(df))
  burst_id[unlist(Map(seq.int, boundaries$s, boundaries$e),
                  use.names = FALSE)] <-
    rep.int(seq_len(n_bursts_raw), burst_len)

  # ── per-burst basic stats via rowsum (C-level; exact two-pass sd) ──────────
  acc_mask <- burst_id > 0L
  bids     <- burst_id[acc_mask]
  axv      <- df$acc_x[acc_mask]
  ayv      <- df$acc_y[acc_mask]
  azv      <- df$acc_z[acc_mask]

  ok_row <- !is.na(axv) & !is.na(ayv) & !is.na(azv)
  b_ok   <- bids[ok_row]
  ax     <- axv[ok_row]; ay <- ayv[ok_row]; az <- azv[ok_row]

  cnt   <- tabulate(b_ok, nbins = n_bursts_raw)
  blank <- function() rep(NA_real_, n_bursts_raw)
  xm  <- blank(); ym  <- blank(); zm  <- blank()
  xsd <- blank(); ysd <- blank(); zsd <- blank(); odba <- blank()

  if (length(b_ok)) {
    S       <- rowsum(cbind(ax, ay, az), b_ok, reorder = TRUE)
    present <- as.integer(rownames(S))
    np      <- cnt[present]
    xm[present] <- S[,1] / np
    ym[present] <- S[,2] / np
    zm[present] <- S[,3] / np

    # Second pass over deviations: numerically exact, and gives ODBA for free
    dx <- ax - xm[b_ok]; dy <- ay - ym[b_ok]; dz <- az - zm[b_ok]
    SS <- rowsum(cbind(dx*dx, dy*dy, dz*dz, abs(dx) + abs(dy) + abs(dz)),
                 b_ok, reorder = TRUE)
    denom <- pmax(np - 1L, 1L)
    xsd[present]  <- sqrt(SS[,1] / denom)
    ysd[present]  <- sqrt(SS[,2] / denom)
    zsd[present]  <- sqrt(SS[,3] / denom)
    odba[present] <- SS[,4] / np
  }

  valid <- cnt >= 2L
  xm[!valid]   <- NA_real_; ym[!valid]  <- NA_real_; zm[!valid]  <- NA_real_
  xsd[!valid]  <- NA_real_; ysd[!valid] <- NA_real_; zsd[!valid] <- NA_real_
  odba[!valid] <- NA_real_
  n_skip <- sum(!valid)

  stat_mat <- matrix(NA_real_, nrow = n_bursts_raw, ncol = length(all_num_cols))
  colnames(stat_mat) <- all_num_cols
  stat_mat[,"acc_burst_n"]      <- cnt
  stat_mat[,"acc_freq_hz"]      <- freq_vec
  stat_mat[,"acc_duration_sec"] <- round(dur_vec, 2)
  stat_mat[,"mean_x"]           <- round(xm,  3)
  stat_mat[,"sd_x"]             <- round(xsd, 3)
  stat_mat[,"mean_y"]           <- round(ym,  3)
  stat_mat[,"sd_y"]             <- round(ysd, 3)
  stat_mat[,"mean_z"]           <- round(zm,  3)
  stat_mat[,"sd_z"]             <- round(zsd, 3)
  stat_mat[,"acc_odba"]         <- round(odba, 3)
  type_vec <- boundaries$type

  # ── advanced stats ─────────────────────────────────────────────────────────
  # Row indices are grouped once with split(); the previous which(bids == b)
  # inside the loop rescanned every ACC row for every burst.
  if (advanced && length(b_ok)) {
    idx_by_burst <- split(seq_along(b_ok), b_ok)
    bnames       <- as.integer(names(idx_by_burst))
    for (k in seq_along(idx_by_burst)) {
      b <- bnames[k]
      if (!valid[b]) next
      idx <- idx_by_burst[[k]]
      ax2 <- ax[idx]; ay2 <- ay[idx]; az2 <- az[idx]
      if (length(ax2) < 2L) next
      adv_vals <- tryCatch(c(
        round(max(ax2)-min(ax2),3),round(max(ay2)-min(ay2),3),round(max(az2)-min(az2),3),
        round(max(ax2),3),round(max(ay2),3),round(max(az2),3),
        round(min(ax2),3),round(min(ay2),3),round(min(az2),3),
        round(sqrt(sum(ax2^2)),3),round(sqrt(sum(ay2^2)),3),round(sqrt(sum(az2^2)),3),
        round(stats::quantile(ax2,.25),3),round(stats::quantile(ay2,.25),3),
        round(stats::quantile(az2,.25),3),
        round(stats::quantile(ax2,.50),3),round(stats::quantile(ay2,.50),3),
        round(stats::quantile(az2,.50),3),
        round(stats::quantile(ax2,.75),3),round(stats::quantile(ay2,.75),3),
        round(stats::quantile(az2,.75),3),
        round(moments::skewness(ax2),3),round(moments::skewness(ay2),3),
        round(moments::skewness(az2),3),
        round(moments::kurtosis(ax2),3),round(moments::kurtosis(ay2),3),
        round(moments::kurtosis(az2),3),
        round(suppressWarnings(stats::cov(ax2,ay2)),3),
        round(suppressWarnings(stats::cov(ax2,az2)),3),
        round(suppressWarnings(stats::cov(ay2,az2)),3),
        round(suppressWarnings(stats::cor(ax2,ay2)),3),
        round(suppressWarnings(stats::cor(ax2,az2)),3),
        round(suppressWarnings(stats::cor(ay2,az2)),3),
        round(mean(ax2-ay2),3),round(mean(ax2-az2),3),round(mean(ay2-az2),3),
        round(stats::sd(ax2-ay2),3),round(stats::sd(ax2-az2),3),
        round(stats::sd(ay2-az2),3),
        round(mean(abs(diff(ax2))),3),
        round(mean(abs(diff(ay2))),3),
        round(mean(abs(diff(az2))),3)
      ), error=function(e) rep(NA_real_,length(adv_cols)))
      stat_mat[b, adv_cols] <- adv_vals
    }
  }

  # ── attach stats to GPS rows (single block assignment) ─────────────────────
  attached <- target_gps[!is.na(target_gps)]
  b_idx    <- which(!is.na(target_gps))
  if (length(attached)) {
    df[attached, all_num_cols]    <- stat_mat[b_idx, , drop = FALSE]
    df$acc_burst_type[attached]   <- type_vec[b_idx]
    df$gps_to_burst_sec[attached] <- gps_to_burst_sec[b_idx]
    df$datatype[attached]         <- "GPS_ACC"
  }

  n_attached <- length(attached)
  n_new_row  <- length(orphan_src)

  # ── build output ───────────────────────────────────────────────────────────
  keep_mask <- if (include_burst_rows) rep(TRUE, nrow(df)) else !is_acc
  out       <- df[keep_mask, , drop = FALSE]
  kept_orig <- which(keep_mask)

  # ── orphan ACC_SUMMARY rows: build all at once, then one rbind + order ─────
  # (previously one rbind per orphan, copying the whole frame each time)
  if (n_new_row > 0L) {
    if (verbose)
      message(sprintf("  Inserting %d ACC_SUMMARY orphan row(s)...", n_new_row))
    src <- boundaries$s[orphan_src]
    orphan_rows <- df[src, , drop = FALSE]
    orphan_rows$datatype        <- "ACC_SUMMARY"
    orphan_rows[, all_num_cols] <- stat_mat[orphan_src, , drop = FALSE]
    orphan_rows$acc_burst_type   <- type_vec[orphan_src]
    orphan_rows$gps_to_burst_sec <- NA_real_

    out <- rbind(out, orphan_rows)
    ord <- order(c(kept_orig, src), method = "radix")
    out <- out[ord, , drop = FALSE]
  }
  rownames(out) <- NULL

  if (verbose) {
    message("\n--- ACC Burst Analysis Summary ---")
    message(sprintf("  Mode                 : %s", if(advanced)"advanced" else "basic"))
    message(sprintf("  Bursts processed     : %d", n_bursts_raw))
    if(n_skip >0L) message(sprintf("  Skipped (< 2 pts)    : %d", n_skip))
    if(n_trunc>0L) message(sprintf("  Truncated bursts     : %d", n_trunc))
    message(sprintf("  GPS threshold        : prev row within +/- %g min", adj_gps_max_min))
    message(sprintf("  Attached to GPS row  : %d", n_attached))
    message(sprintf("  New ACC_SUMMARY rows : %d", n_new_row))
    message(sprintf("  Output rows          : %d", nrow(out)))
    message(sprintf("  ACC columns added    : %d", length(all_num_cols)+1L))
  }
  out
}


# ==============================================================================
# VECTORISED BURST BOUNDARY FINDERS
# ==============================================================================

#' @noRd
#' V2 boundaries. Each START is paired with the first END that follows it and
#' precedes the next START. findInterval() resolves all bursts in one binary
#' search instead of rescanning the END vector for every burst.
.acc_bounds_v2 <- function(n_row, cls, tag_vec = NULL) {
  start_idx <- which(cls$acc_start)
  end_idx   <- which(cls$acc_end)
  acc_idx   <- which(cls$acc_any)

  if (!length(start_idx))
    return(data.frame(s=integer(), e=integer(), type=character(),
                      truncated=logical(), stringsAsFactors=FALSE))

  n_b        <- length(start_idx)
  next_start <- c(start_idx[-1L], n_row + 1L)

  # First END strictly after each START
  pos      <- findInterval(start_idx, end_idx) + 1L
  in_range <- pos <= length(end_idx)
  cand_end <- rep(NA_integer_, n_b)
  if (any(in_range)) cand_end[in_range] <- end_idx[pos[in_range]]

  ok <- !is.na(cand_end) & cand_end < next_start
  if (!is.null(tag_vec) && any(ok))
    ok[ok] <- tag_vec[cand_end[ok]] == tag_vec[start_idx[ok]]

  e_vec     <- integer(n_b)
  e_vec[ok] <- cand_end[ok]

  # Truncated bursts (no END): last ACC row before the next START
  if (any(!ok)) {
    s_bad    <- start_idx[!ok]
    n_bad    <- next_start[!ok]
    k        <- findInterval(n_bad - 1L, acc_idx)
    last_acc <- ifelse(k > 0L, acc_idx[pmax(k, 1L)], s_bad)
    last_acc <- pmax(last_acc, s_bad)

    # Rare: last ACC row in the window belongs to another individual —
    # fall back to a bounded scan for just those bursts.
    if (!is.null(tag_vec)) {
      bad <- which(tag_vec[last_acc] != tag_vec[s_bad])
      for (j in bad) {
        w <- acc_idx[acc_idx >= s_bad[j] & acc_idx < n_bad[j] &
                       tag_vec[acc_idx] == tag_vec[s_bad[j]]]
        last_acc[j] <- if (length(w)) max(w) else s_bad[j]
      }
    }
    e_vec[!ok] <- last_acc
  }

  data.frame(
    s         = start_idx,
    e         = e_vec,
    type      = sub("_START$", "", cls$dt[start_idx]),
    truncated = !ok,
    stringsAsFactors = FALSE
  )
}


#' @noRd
#' V1 boundaries: consecutive SENSORS rows separated by more than gap_sec
#' start a new burst.
.acc_bounds_v1 <- function(sensor_idx, ts_num, gap_sec = 5) {
  if (!length(sensor_idx))
    return(data.frame(s=integer(), e=integer(), type=character(),
                      truncated=logical(), stringsAsFactors=FALSE))

  ts      <- ts_num[sensor_idx]
  gaps    <- c(diff(ts), Inf)
  grp_end <- which(gaps > gap_sec | is.na(gaps))
  grp_beg <- c(1L, grp_end[-length(grp_end)] + 1L)
  n_g     <- length(grp_end)

  types <- vapply(seq_len(n_g), function(i) {
    ivl <- diff(ts[grp_beg[i]:grp_end[i]])
    ivl <- ivl[ivl > 0 & !is.na(ivl)]
    hz  <- if (length(ivl)) round(1 / stats::median(ivl)) else NA_real_
    if (!is.na(hz)) paste0("SEN_ACC_", hz, "Hz") else "SEN_ACC_SENSORS"
  }, character(1))

  data.frame(s = sensor_idx[grp_beg], e = sensor_idx[grp_end],
             type = types, truncated = rep(FALSE, n_g),
             stringsAsFactors = FALSE)
}
