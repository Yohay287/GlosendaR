# ==============================================================================
#' Add Day_ID (and optionally sun angle and day progress) to a tracking data frame
#'
#' Assigns a unique day identifier to each row based on sunrise/sunset times
#' at the GPS location. For diurnal animals a day runs from sunrise to the
#' following sunrise; for nocturnal animals it runs from sunset to the following
#' sunset. The sun crossing time is defined by a user-specified solar altitude
#' angle (default: −6°, civil twilight).
#'
#' Sun crossing times are computed via a vectorised binary search using
#' \code{suncalc::getSunlightPosition()}, so all tag-date combinations are
#' processed simultaneously regardless of dataset size.
#'
#' Rows without GPS coordinates (e.g. ACC rows) inherit their Day_ID from
#' the GPS row in the same event when \code{Event_ID} is present.
#'
#' @param df A data frame from \code{\link{glosendas_download}} or
#'   \code{\link{analyze_acc}}. Must contain \code{tag_name},
#'   \code{UTC_datetime}, \code{Latitude}, and \code{Longitude}.
#' @param type Character. \code{"diurnal"} (default) — day starts at sunrise;
#'   \code{"nocturnal"} — day starts at sunset.
#' @param sun_angle Numeric. Solar altitude in degrees that defines the day
#'   boundary. Default: \code{-6} (civil twilight). Common values:
#'   \code{0} = geometric horizon, \code{-6} = civil twilight,
#'   \code{-12} = nautical twilight, \code{-18} = astronomical twilight.
#' @param lat_col Character. Latitude column name. Default: \code{"Latitude"}.
#' @param lon_col Character. Longitude column name. Default: \code{"Longitude"}.
#' @param timestamp_col Character. Timestamp column. Default: \code{"UTC_datetime"}.
#' @param add_sun_angle Logical. Add a \code{sun_angle_deg} column with the
#'   solar altitude in degrees (2 decimal places) for each row.
#'   Default: \code{FALSE}.
#' @param day_progress Logical. Add a \code{day_progress} column (0–200, 2
#'   decimal places):
#'   \itemize{
#'     \item \strong{Diurnal}: 0 = sunrise, 100 = sunset, 200 = next sunrise
#'     \item \strong{Nocturnal}: 0 = sunset, 100 = sunrise, 200 = next sunset
#'   }
#'   Default: \code{FALSE}.
#' @param verbose Logical. Print a summary. Default: \code{TRUE}.
#'
#' @return The input data frame with new columns:
#'   \itemize{
#'     \item \code{Day_ID} — format \code{"<tag_name>_<YYYYMMDD>"}
#'       e.g. \code{"Houbara Kelach 2_20260207"}
#'     \item \code{sun_angle_deg} — solar altitude in degrees
#'       (only when \code{add_sun_angle = TRUE})
#'     \item \code{day_progress} — day progress 0–200
#'       (only when \code{day_progress = TRUE})
#'   }
#'   All original columns are preserved unchanged.
#'
#' @examples
#' \dontrun{
#' df     <- glosendas_download("myuser", "mypass", filter_word = "Houbara")
#' df_acc <- analyze_acc(df)
#' df_ev  <- add_event_id(df_acc)   # must run before add_day_id
#'
#' # Basic Day_ID (diurnal, civil twilight)
#' df_day <- add_day_id(df_ev)
#'
#' # With sun angle and day progress columns
#' df_day <- add_day_id(df_ev, add_sun_angle = TRUE, day_progress = TRUE)
#'
#' # Nocturnal animals
#' df_day <- add_day_id(df_ev, type = "nocturnal")
#'
#' # Geometric sunrise (sun_angle = 0)
#' df_day <- add_day_id(df_ev, sun_angle = 0)
#' }
#'
#' @export
add_day_id <- function(df,
                       type          = "diurnal",
                       sun_angle     = -6,
                       lat_col       = "Latitude",
                       lon_col       = "Longitude",
                       timestamp_col = "UTC_datetime",
                       add_sun_angle = FALSE,
                       day_progress  = FALSE,
                       verbose       = TRUE) {

  # ── guards ────────────────────────────────────────────────────────────────
  if (!inherits(df, "data.frame")) stop("`df` must be a data frame.")
  df <- as.data.frame(df)
  if (nrow(df) == 0) stop("`df` has zero rows.")
  if (!type %in% c("diurnal", "nocturnal"))
    stop('`type` must be "diurnal" or "nocturnal".')
  if (!is.numeric(sun_angle) || length(sun_angle) != 1)
    stop("`sun_angle` must be a single number (degrees).")
  if (!requireNamespace("suncalc", quietly = TRUE))
    stop("Package 'suncalc' is required.\n",
         "Install with: install.packages('suncalc')")

  required <- c("tag_name", timestamp_col, lat_col, lon_col)
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0)
    stop("Missing required columns: ", paste(missing, collapse = ", "))

  # ── parse timestamps ───────────────────────────────────────────────────────

  ts     <- .gl_to_posix(df[[timestamp_col]])
  ts_num <- as.numeric(ts)

  .gl_check_order(df, ts_num, "add_day_id")

  lat <- suppressWarnings(as.numeric(df[[lat_col]]))
  lon <- suppressWarnings(as.numeric(df[[lon_col]]))

  # ── build representative coordinates per tag + date ───────────────────────
  # Use only rows with valid coordinates (GPS rows)
  utc_date  <- as.Date(ts, tz = "UTC")
  has_coord <- !is.na(lat) & !is.na(lon) & !is.na(ts)
  tag_date_key <- paste(df$tag_name, utc_date, sep = "__")

  # One representative coord per unique tag+date key (vectorised via tapply)
  unique_keys <- unique(tag_date_key[has_coord])

  med_lat <- tapply(lat[has_coord],  tag_date_key[has_coord], stats::median)
  med_lon <- tapply(lon[has_coord],  tag_date_key[has_coord], stats::median)

  # Parse tag and date back out of the key
  key_parts <- strsplit(unique_keys, "__", fixed = TRUE)
  agg_tag   <- vapply(key_parts, `[`, character(1), 1)
  agg_date  <- as.Date(vapply(key_parts, `[`, character(1), 2))
  agg_lat   <- as.numeric(med_lat[unique_keys])
  agg_lon   <- as.numeric(med_lon[unique_keys])

  n_combos <- length(unique_keys)
  if (verbose)
    message(sprintf("Computing sun crossing times for %d tag-date combination(s)...",
                    n_combos))

  # ── binary search for sun crossing (vectorised across all combos) ──────────
  # For each combo, binary-search the exact second the sun crosses sun_angle.
  # We run all combos simultaneously: each iteration updates only the combos
  # that haven't converged yet.
  #
  # Direction:
  #   diurnal   -> sun RISING through angle  -> search 00:00 to 14:00 UTC
  #   nocturnal -> sun SETTING through angle -> search 10:00 to 24:00 UTC

  sun_angle_rad <- sun_angle * pi / 180
  direction     <- if (type == "diurnal") "rise" else "set"

  # Initial search window per combo (epoch seconds)
  # Day start in epoch seconds; "24:00:00" is not portable across platforms,
  # so end-of-day is expressed as midnight + 86400.
  day0 <- as.numeric(as.POSIXct(paste(agg_date, "00:00:00"), tz = "UTC"))

  if (direction == "rise") {
    t_lo <- day0
    t_hi <- day0 + 14 * 3600
  } else {
    t_lo <- day0 + 10 * 3600
    t_hi <- day0 + 86400
  }

  converged <- rep(FALSE, n_combos)

  for (iter in seq_len(35)) {   # 35 iterations -> ~1 second precision from 24h
    active <- which(!converged)
    if (length(active) == 0) break

    t_mid <- (t_lo[active] + t_hi[active]) / 2

    # getSunlightPosition vectorised API: pass all rows as a data frame
    pos_data <- data.frame(
      date = as.POSIXct(t_mid, origin = "1970-01-01", tz = "UTC"),
      lat  = agg_lat[active],
      lon  = agg_lon[active]
    )
    pos <- suncalc::getSunlightPosition(data = pos_data)
    alt <- pos$altitude   # radians

    if (direction == "rise") {
      below <- alt < sun_angle_rad
      t_lo[active[below]]  <- t_mid[below]
      t_hi[active[!below]] <- t_mid[!below]
    } else {
      above <- alt > sun_angle_rad
      t_lo[active[above]]  <- t_mid[above]
      t_hi[active[!above]] <- t_mid[!above]
    }

    converged[active] <- (t_hi[active] - t_lo[active]) < 1
  }

  crossing_epoch <- (t_lo + t_hi) / 2   # final midpoint = crossing time
  names(crossing_epoch) <- unique_keys

  # ── compute opposite crossing for day_progress ────────────────────────────
  # For diurnal: also need sunset (sun setting through angle) = midday boundary
  # For nocturnal: also need sunrise (sun rising through angle) = midday boundary
  if (day_progress) {
    opp_direction <- if (direction == "rise") "set" else "rise"

    if (opp_direction == "set") {
      opp_lo <- day0 + 10 * 3600
      opp_hi <- day0 + 86400
    } else {
      opp_lo <- day0
      opp_hi <- day0 + 14 * 3600
    }

    opp_converged <- rep(FALSE, n_combos)
    for (iter in seq_len(35)) {
      active <- which(!opp_converged)
      if (length(active) == 0) break
      t_mid <- (opp_lo[active] + opp_hi[active]) / 2
      pos_data <- data.frame(
        date = as.POSIXct(t_mid, origin = "1970-01-01", tz = "UTC"),
        lat  = agg_lat[active], lon = agg_lon[active]
      )
      pos <- suncalc::getSunlightPosition(data = pos_data)
      alt <- pos$altitude
      if (opp_direction == "set") {
        above <- alt > sun_angle_rad
        opp_lo[active[above]]  <- t_mid[above]
        opp_hi[active[!above]] <- t_mid[!above]
      } else {
        below <- alt < sun_angle_rad
        opp_lo[active[below]]  <- t_mid[below]
        opp_hi[active[!below]] <- t_mid[!below]
      }
      opp_converged[active] <- (opp_hi[active] - opp_lo[active]) < 1
    }
    opp_epoch <- (opp_lo + opp_hi) / 2
    names(opp_epoch) <- unique_keys
  }

  # Build crossing lookup: tag+date -> crossing epoch second

  # ── assign Day_ID to every row (vectorised) ───────────────────────────────
  # For each row, find which crossing period it belongs to.
  # Strategy:
  #   1. Sort crossings by epoch time
  #   2. Use findInterval() to find the last crossing <= each row's timestamp
  #   3. Look up the date of that crossing

  # Build a sorted frame of all crossings across all tags
  cross_frame <- data.frame(
    tag     = agg_tag,
    date    = agg_date,
    epoch   = crossing_epoch,
    key     = unique_keys,
    stringsAsFactors = FALSE
  )
  cross_frame <- cross_frame[order(cross_frame$tag, cross_frame$epoch), ]

  # Assign Day_ID per row using findInterval (one call per tag)
  day_ids <- character(nrow(df))

  # One pass to group row indices by tag instead of a full scan per tag
  rows_by_tag  <- split(seq_len(nrow(df)), df$tag_name)
  cross_by_tag <- split(cross_frame, cross_frame$tag)

  # Process each tag separately (tags have independent crossing schedules)
  for (tg in names(rows_by_tag)) {
    row_idx    <- rows_by_tag[[tg]]
    tag_ts     <- ts_num[row_idx]
    tag_cross  <- cross_by_tag[[tg]]
    if (is.null(tag_cross)) tag_cross <- cross_frame[0, ]

    if (nrow(tag_cross) == 0) {
      # No coordinate data for this tag — fall back to UTC date
      day_ids[row_idx] <- paste0(tg, "_",
                                  format(as.Date(ts[row_idx], tz = "UTC"), "%Y%m%d"))
      next
    }

    # findInterval: for each row timestamp, find index of last crossing <= ts
    fi <- findInterval(tag_ts, tag_cross$epoch)

    # Format each crossing date once, then index — rather than formatting
    # one Date per row (millions of format() calls on large datasets).
    lbl        <- paste0(tg, "_", format(tag_cross$date, "%Y%m%d"))
    lbl_before <- paste0(tg, "_", format(tag_cross$date[1] - 1, "%Y%m%d"))

    # fi == 0 means before the first crossing — belongs to the previous day
    day_ids[row_idx] <- ifelse(fi == 0L, lbl_before, lbl[pmax(fi, 1L)])
  }

  df$Day_ID <- day_ids

  # ── add optional sun angle column ────────────────────────────────────────
  if (add_sun_angle) {
    if (verbose) message("Computing sun angles for all rows...")
    valid_rows <- !is.na(lat) & !is.na(lon) & !is.na(ts_num)
    sun_alt_deg <- rep(NA_real_, nrow(df))

    if (any(valid_rows)) {
      pos_data <- data.frame(
        date = as.POSIXct(ts_num[valid_rows], origin = "1970-01-01", tz = "UTC"),
        lat  = lat[valid_rows],
        lon  = lon[valid_rows]
      )
      pos <- suncalc::getSunlightPosition(data = pos_data)
      sun_alt_deg[valid_rows] <- round(pos$altitude * 180 / pi, 2)
    }
    df$sun_angle_deg <- sun_alt_deg
  }

  # ── add optional day progress column (0-200) ──────────────────────────────
  if (day_progress) {
    day_prog <- rep(NA_real_, nrow(df))

    for (tg in names(rows_by_tag)) {
      row_idx   <- rows_by_tag[[tg]]
      tag_ts    <- ts_num[row_idx]
      tag_cross <- cross_by_tag[[tg]]
      if (is.null(tag_cross) || nrow(tag_cross) == 0) next

      cross_times <- tag_cross$epoch
      opp_times   <- as.numeric(opp_epoch[tag_cross$key])
      n_cross     <- length(cross_times)

      # Next day start for each crossing (last one falls back to +24 h)
      next_times <- c(cross_times[-1L], cross_times[n_cross] + 86400)

      fi <- findInterval(tag_ts, cross_times)
      ok <- fi > 0L

      # Fully vectorised: no per-row loop
      idx     <- pmax(fi, 1L)
      t_start <- cross_times[idx]
      t_opp   <- opp_times[idx]
      t_end   <- next_times[idx]

      ok <- ok & !is.na(t_opp) & tag_ts >= t_start

      first_half <- ok & tag_ts <= t_opp
      second_half <- ok & tag_ts > t_opp

      prog <- rep(NA_real_, length(row_idx))
      prog[first_half] <- 100 * (tag_ts[first_half] - t_start[first_half]) /
        pmax(1, t_opp[first_half] - t_start[first_half])
      prog[second_half] <- 100 + 100 *
        (tag_ts[second_half] - t_opp[second_half]) /
        pmax(1, t_end[second_half] - t_opp[second_half])

      day_prog[row_idx] <- round(prog, 2)
    }
    df$day_progress <- day_prog
  }

  # ── propagate Day_ID within Event_ID groups ───────────────────────────────
  # ACC rows (and other non-GPS rows) share an Event_ID with their GPS row
  # but have no coordinates, so their Day_ID must come from the GPS row
  # in the same event. We take the first non-empty Day_ID per Event_ID.
  if ("Event_ID" %in% names(df)) {
    needs_fill <- !nzchar(df$Day_ID) | is.na(df$Day_ID)

    .prop_col <- function(col) {
      ev_val <- tapply(df[[col]], df$Event_ID, function(x) {
        good <- x[!is.na(x)]
        if (length(good) > 0) good[1] else NA
      })
      df[[col]][needs_fill] <<- ev_val[df$Event_ID[needs_fill]]
    }

    if (any(needs_fill)) {
      .prop_col("Day_ID")
      if (add_sun_angle && "sun_angle_deg" %in% names(df))
        .prop_col("sun_angle_deg")
      if (day_progress && "day_progress" %in% names(df))
        .prop_col("day_progress")
      if (verbose)
        message(sprintf("  Propagated columns to %d rows via Event_ID",
                        sum(needs_fill)))
    }
  }

  if (verbose) {
    n_days <- length(unique(day_ids[nzchar(day_ids)]))
    message(sprintf("\n--- Day_ID Summary ---"))
    message(sprintf("  Type          : %s", type))
    message(sprintf("  Sun angle     : %g degrees", sun_angle))
    message(sprintf("  Day boundary  : %s", if(type=="diurnal") "sunrise" else "sunset"))
    message(sprintf("  Unique Day_IDs: %d", n_days))
    message("  Example Day_IDs:")
    for (id in head(unique(day_ids[nzchar(day_ids)]), 5))
      message("    ", id)
    message(sprintf("  add_sun_angle : %s", add_sun_angle))
    message(sprintf("  day_progress  : %s", day_progress))
  }

  df
}