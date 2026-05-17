# ==============================================================================
#' Analyze accelerometer bursts and attach summary statistics to GPS rows
#'
#' Processes a data frame from \code{\link{glosendas_download}} and computes
#' per-burst statistics for each accelerometer axis (X, Y, Z). Works with
#' all six portal formats (V1 and V2). Fully vectorised for performance on
#' large datasets.
#'
#' @param df Data frame from \code{glosendas_download()}.
#' @param gps_window_sec Numeric. Max seconds between GPS fix and burst start.
#'   Default: \code{10}.
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
                        gps_window_sec     = 10,
                        include_burst_rows = FALSE,
                        advanced           = FALSE,
                        v1_burst_gap_sec   = 5,
                        verbose            = TRUE) {

  # ── guards ───────────────────────────────────────────────────────────────────
  if (!is.data.frame(df))        stop("`df` must be a data frame.")
  if (nrow(df) == 0)             stop("`df` has zero rows.")
  if (gps_window_sec < 0)        stop("`gps_window_sec` must be >= 0.")
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

  # ── coerce ACC ───────────────────────────────────────────────────────────────
  df$acc_x <- suppressWarnings(as.numeric(df$acc_x))
  df$acc_y <- suppressWarnings(as.numeric(df$acc_y))
  df$acc_z <- suppressWarnings(as.numeric(df$acc_z))

  # ── build precise timestamp ──────────────────────────────────────────────────
  has_precise <- all(c("UTC_date","UTC_time") %in% names(df))
  if (has_precise) {
    base_ts <- suppressWarnings(
      as.POSIXct(paste(df$UTC_date, df$UTC_time),
                 format="%Y-%m-%d %H:%M:%S", tz="UTC"))
    bad2 <- is.na(base_ts)
    if (any(bad2))
      base_ts[bad2] <- suppressWarnings(
        as.POSIXct(paste(df$UTC_date[bad2], df$UTC_time[bad2]),
                   format="%d/%m/%Y %H:%M:%S", tz="UTC"))
    if ("milliseconds" %in% names(df)) {
      ms <- suppressWarnings(as.numeric(df$milliseconds))
      ms[is.na(ms)] <- 0
      df$UTC_precise <- base_ts + ms/1000
    } else {
      df$UTC_precise <- base_ts
    }
  } else if ("UTC_datetime" %in% names(df)) {
    df$UTC_precise <- suppressWarnings(as.POSIXct(df$UTC_datetime, tz="UTC"))
  } else {
    df$UTC_precise <- as.POSIXct(NA_real_, tz="UTC")
  }

  if ("UTC_datetime" %in% names(df) && !inherits(df$UTC_datetime,"POSIXct"))
    df$UTC_datetime <- suppressWarnings(as.POSIXct(df$UTC_datetime, tz="UTC"))

  # ── detect format ────────────────────────────────────────────────────────────
  has_v2 <- any(grepl("^SEN_ACC_[0-9]+[Hh]z", df$datatype))
  has_v1 <- any(df$datatype == "SENSORS")

  if (!has_v2 && !has_v1) {
    if (verbose) message("No ACC rows found.")
    df$UTC_precise <- NULL
    if (include_burst_rows) return(df) else return(df)
  }

  is_acc <- if (has_v2) grepl("^SEN_ACC_[0-9]+[Hh]z", df$datatype)
            else        df$datatype == "SENSORS"
  is_gps <- df$datatype %in% c("GPS","GPSF")

  if (verbose) {
    fmt <- if (has_v2) "V2 (SEN_ACC_*Hz)" else "V1 (SENSORS)"
    message(sprintf("Detecting ACC bursts — format: %s", fmt))
  }

  # ── find burst boundaries ────────────────────────────────────────────────────
  boundaries <- if (has_v2) .acc_bounds_v2(df) else .acc_bounds_v1(df, v1_burst_gap_sec)

  n_bursts_raw <- nrow(boundaries)
  if (n_bursts_raw == 0L) {
    if (verbose) message("No ACC bursts found.")
    df$UTC_precise <- NULL
    if (include_burst_rows) return(df) else return(df[!is_acc,])
  }
  if (verbose) message(sprintf("Found %d burst(s)", n_bursts_raw))

  # ── initialise output columns ────────────────────────────────────────────────
  basic_cols <- c("acc_burst_n","acc_freq_hz","acc_duration_sec",
                  "acc_x_mean","acc_x_sd","acc_y_mean","acc_y_sd",
                  "acc_z_mean","acc_z_sd","acc_odba")
  adv_cols <- c(
    "acc_x_range","acc_y_range","acc_z_range",
    "acc_x_max","acc_y_max","acc_z_max",
    "acc_x_min","acc_y_min","acc_z_min",
    "acc_x_norm","acc_y_norm","acc_z_norm",
    "acc_x_q25","acc_y_q25","acc_z_q25",
    "acc_x_q50","acc_y_q50","acc_z_q50",
    "acc_x_q75","acc_y_q75","acc_z_q75",
    "acc_x_skew","acc_y_skew","acc_z_skew",
    "acc_x_kurt","acc_y_kurt","acc_z_kurt",
    "acc_cov_xy","acc_cov_xz","acc_cov_yz",
    "acc_cor_xy","acc_cor_xz","acc_cor_yz",
    "acc_meandiff_xy","acc_meandiff_xz","acc_meandiff_yz",
    "acc_sddiff_xy","acc_sddiff_xz","acc_sddiff_yz",
    "acc_amp_x","acc_amp_y","acc_amp_z"
  )
  all_num_cols <- if (advanced) c(basic_cols, adv_cols) else basic_cols
  for (col in all_num_cols) df[[col]] <- NA_real_
  df$acc_burst_type <- NA_character_

  gps_idx      <- which(is_gps)
  burst_ts_num <- as.numeric(df$UTC_precise)
  gps_ts_num   <- burst_ts_num[gps_idx]

  # ── pre-compute per-burst scalars (fully vectorised) ─────────────────────────
  n_trunc  <- sum(isTRUE(boundaries$truncated))
  freq_vec <- suppressWarnings(
    as.numeric(stringr::str_extract(boundaries$type, "[0-9]+(?=[Hh]z)")))
  dur_vec  <- abs(burst_ts_num[boundaries$e] - burst_ts_num[boundaries$s])

  # ── GPS matching: findInterval() — binary search for ALL bursts at once ───────
  fi         <- findInterval(boundaries$s - 1L, gps_idx)
  has_prev   <- fi > 0L
  time_diff  <- ifelse(has_prev,
                       burst_ts_num[boundaries$s] - gps_ts_num[pmax(fi,1L)],
                       NA_real_)
  in_window  <- has_prev & !is.na(time_diff) &
                time_diff >= 0 & time_diff <= gps_window_sec
  target_gps <- ifelse(in_window, gps_idx[pmax(fi,1L)], NA_integer_)
  orphan_src <- which(!in_window)

  # ── assign burst ID to every ACC row for tapply grouping ─────────────────────
  # Small loop over n_bursts (integer assignment only — fast even for 100k bursts)
  burst_id <- integer(nrow(df))
  for (b in seq_len(n_bursts_raw))
    burst_id[boundaries$s[b]:boundaries$e[b]] <- b

  # ── per-burst basic stats via tapply (O(n_acc_rows) total) ───────────────────
  acc_mask <- burst_id > 0L
  bids     <- burst_id[acc_mask]
  axv      <- df$acc_x[acc_mask]
  ayv      <- df$acc_y[acc_mask]
  azv      <- df$acc_z[acc_mask]

  .tap <- function(vals, FUN) {
    res <- tapply(vals, bids, FUN)
    out <- rep(NA_real_, n_bursts_raw)
    out[as.integer(names(res))] <- as.numeric(res)
    out
  }

  valid_fn <- function(x) length(x[!is.na(x)]) >= 2L

  xm  <- .tap(axv, function(x) if(valid_fn(x)) mean(x,na.rm=TRUE)       else NA_real_)
  ym  <- .tap(ayv, function(x) if(valid_fn(x)) mean(x,na.rm=TRUE)       else NA_real_)
  zm  <- .tap(azv, function(x) if(valid_fn(x)) mean(x,na.rm=TRUE)       else NA_real_)
  xsd <- .tap(axv, function(x) if(valid_fn(x)) stats::sd(x,na.rm=TRUE)  else NA_real_)
  ysd <- .tap(ayv, function(x) if(valid_fn(x)) stats::sd(x,na.rm=TRUE)  else NA_real_)
  zsd <- .tap(azv, function(x) if(valid_fn(x)) stats::sd(x,na.rm=TRUE)  else NA_real_)
  bn  <- .tap(axv, function(x) sum(!is.na(x)))

  # ODBA via tapply (needs per-burst mean subtraction)
  odba_res <- tapply(seq_along(bids), bids, function(idx) {
    ax2 <- axv[idx][!is.na(axv[idx])]
    ay2 <- ayv[idx][!is.na(ayv[idx])]
    az2 <- azv[idx][!is.na(azv[idx])]
    if (any(lengths(list(ax2,ay2,az2)) < 2L)) return(NA_real_)
    mean(abs(ax2-mean(ax2)) + abs(ay2-mean(ay2)) + abs(az2-mean(az2)))
  })
  odba <- rep(NA_real_, n_bursts_raw)
  odba[as.integer(names(odba_res))] <- as.numeric(odba_res)

  valid   <- !is.na(xm) & !is.na(ym) & !is.na(zm)
  n_skip  <- sum(!valid)

  # Fill stat matrix (vectorised column assignment)
  stat_mat <- matrix(NA_real_, nrow=n_bursts_raw, ncol=length(all_num_cols))
  colnames(stat_mat) <- all_num_cols
  stat_mat[,"acc_burst_n"]      <- bn
  stat_mat[,"acc_freq_hz"]      <- freq_vec
  stat_mat[,"acc_duration_sec"] <- round(dur_vec,2)
  stat_mat[,"acc_x_mean"]       <- round(xm, 3)
  stat_mat[,"acc_x_sd"]         <- round(xsd,3)
  stat_mat[,"acc_y_mean"]       <- round(ym, 3)
  stat_mat[,"acc_y_sd"]         <- round(ysd,3)
  stat_mat[,"acc_z_mean"]       <- round(zm, 3)
  stat_mat[,"acc_z_sd"]         <- round(zsd,3)
  stat_mat[,"acc_odba"]         <- round(odba,3)
  type_vec <- boundaries$type

  # ── advanced stats (loop only over valid bursts; no per-row work) ─────────────
  if (advanced) {
    for (b in which(valid)) {
      idx <- which(bids == b)
      ax2 <- axv[idx]; ay2 <- ayv[idx]; az2 <- azv[idx]
      ax2 <- ax2[!is.na(ax2)]; ay2 <- ay2[!is.na(ay2)]; az2 <- az2[!is.na(az2)]
      if (any(lengths(list(ax2,ay2,az2)) < 2L)) next
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
        round(stats::cov(ax2,ay2),3),round(stats::cov(ax2,az2),3),
        round(stats::cov(ay2,az2),3),
        round(stats::cor(ax2,ay2),3),round(stats::cor(ax2,az2),3),
        round(stats::cor(ay2,az2),3),
        round(mean(ax2-ay2),3),round(mean(ax2-az2),3),round(mean(ay2-az2),3),
        round(stats::sd(ax2-ay2),3),round(stats::sd(ax2-az2),3),
        round(stats::sd(ay2-az2),3),
        round(if(length(ax2)>1)mean(abs(diff(ax2)))else NA_real_,3),
        round(if(length(ay2)>1)mean(abs(diff(ay2)))else NA_real_,3),
        round(if(length(az2)>1)mean(abs(diff(az2)))else NA_real_,3)
      ),error=function(e) rep(NA_real_,length(adv_cols)))
      stat_mat[b,adv_cols] <- adv_vals
    }
  }

  # ── attach stats to GPS rows (vectorised assignment) ─────────────────────────
  attached <- target_gps[!is.na(target_gps)]
  b_idx    <- which(!is.na(target_gps))
  for (col in all_num_cols)
    df[[col]][attached] <- stat_mat[b_idx, col]
  df$acc_burst_type[attached] <- type_vec[b_idx]

  n_attached <- length(attached)
  n_new_row  <- length(orphan_src)

  # ── build output ─────────────────────────────────────────────────────────────
  keep_mask      <- if (include_burst_rows) rep(TRUE,nrow(df)) else !is_acc
  df$UTC_precise <- NULL
  out            <- df[keep_mask,,drop=FALSE]
  rownames(out)  <- NULL

  # insert orphan ACC_SUMMARY rows (rare)
  if (n_new_row > 0L) {
    orig_idx <- which(keep_mask)
    for (b in rev(orphan_src)) {
      s <- boundaries$s[b]
      pos <- sum(orig_idx < s)
      new_row          <- df[s,,drop=FALSE]
      new_row$datatype <- "ACC_SUMMARY"
      new_row$UTC_precise <- NULL
      for (col in all_num_cols) new_row[[col]] <- stat_mat[b,col]
      new_row$acc_burst_type <- type_vec[b]
      out <- if(pos==0L) rbind(new_row,out) else
             if(pos>=nrow(out)) rbind(out,new_row) else
             rbind(out[seq_len(pos),,drop=FALSE],new_row,
                   out[(pos+1L):nrow(out),,drop=FALSE])
      orig_idx <- c(orig_idx[seq_len(pos)],NA_integer_,
                    orig_idx[seq(pos+1L,length(orig_idx))])
    }
    rownames(out) <- NULL
  }

  if (verbose) {
    message("\n--- ACC Burst Analysis Summary ---")
    message(sprintf("  Mode                 : %s", if(advanced)"advanced" else "basic"))
    message(sprintf("  Bursts processed     : %d", n_bursts_raw))
    if(n_skip >0L) message(sprintf("  Skipped (< 2 pts)    : %d", n_skip))
    if(n_trunc>0L) message(sprintf("  Truncated bursts     : %d", n_trunc))
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
.acc_bounds_v2 <- function(df) {
  is_start <- grepl("^SEN_ACC_[0-9]+[Hh]z_START$", df$datatype)
  is_end   <- grepl("^SEN_ACC_[0-9]+[Hh]z_END$",   df$datatype)
  is_acc   <- grepl("^SEN_ACC_[0-9]+[Hh]z",         df$datatype)
  start_idx <- which(is_start)
  end_idx   <- which(is_end)
  if (!length(start_idx))
    return(data.frame(s=integer(),e=integer(),type=character(),
                      truncated=logical(),stringsAsFactors=FALSE))
  n_b       <- length(start_idx)
  next_start <- c(start_idx[-1L], nrow(df)+1L)
  e_vec  <- integer(n_b)
  trunc_v <- logical(n_b)
  for (b in seq_len(n_b)) {
    cands <- end_idx[end_idx > start_idx[b] & end_idx < next_start[b]]
    if (length(cands)) { e_vec[b] <- cands[1L]; trunc_v[b] <- FALSE
    } else {
      aw <- which(is_acc & seq_len(nrow(df)) >= start_idx[b] &
                    seq_len(nrow(df)) < next_start[b])
      e_vec[b]  <- if(length(aw)) max(aw) else start_idx[b]
      trunc_v[b] <- TRUE
    }
  }
  data.frame(s=start_idx, e=e_vec,
             type=sub("_START$","",df$datatype[start_idx],ignore.case=TRUE),
             truncated=trunc_v, stringsAsFactors=FALSE)
}


#' @noRd
.acc_bounds_v1 <- function(df, gap_sec=5) {
  sensor_idx <- which(df$datatype == "SENSORS")
  if (!length(sensor_idx))
    return(data.frame(s=integer(),e=integer(),type=character(),
                      truncated=logical(),stringsAsFactors=FALSE))
  ts_num  <- as.numeric(df$UTC_precise[sensor_idx])
  gaps    <- c(diff(ts_num), Inf)
  new_grp <- gaps > gap_sec | is.na(gaps)
  grp_end   <- which(new_grp)
  grp_start <- c(1L, grp_end[-length(grp_end)]+1L)
  n_g <- length(grp_end)
  types <- vapply(seq_len(n_g), function(i) {
    ivl <- diff(ts_num[grp_start[i]:grp_end[i]])
    ivl <- ivl[ivl>0 & !is.na(ivl)]
    hz  <- if(length(ivl)) round(1/stats::median(ivl)) else NA_real_
    if(!is.na(hz)) paste0("SEN_ACC_",hz,"Hz") else "SEN_ACC_SENSORS"
  }, character(1))
  data.frame(s=sensor_idx[grp_start], e=sensor_idx[grp_end],
             type=types, truncated=rep(FALSE,n_g), stringsAsFactors=FALSE)
}
