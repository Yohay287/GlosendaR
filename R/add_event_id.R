# ==============================================================================
#' Add Event_ID (and optionally Event_count) to a Glosendas data frame
#'
#' Groups consecutive rows into events and assigns each a unique identifier
#' in the format \code{"<tag_name>_<UTC_timestamp>"}.
#'
#' \strong{What counts as one event:}
#' \itemize{
#'   \item A GPS or GPSF fix (possibly a GPS burst of several fixes)
#'   \item An ACC burst following the GPS/GPSF fix
#'   \item A GPS/GPSF burst immediately followed by an ACC burst is
#'     \emph{one} event — they belong to the same activity
#' }
#'
#' \strong{Event boundary rules — a new event starts when:}
#' \enumerate{
#'   \item It is the first row, OR
#'   \item The tag name changes, OR
#'   \item The time gap from the previous row exceeds \code{max_gap_sec}, OR
#'   \item A GPS/GPSF row appears after a non-GPS row (e.g. after
#'     \code{ACC_END}) — this always signals the start of a new fix/event
#' }
#'
#' @param df A data frame from \code{\link{glosendas_download}} or
#'   \code{\link{analyze_acc}}. Must contain columns \code{datatype},
#'   \code{tag_name}, and \code{UTC_datetime}.
#' @param max_gap_sec Numeric. Maximum seconds between consecutive rows
#'   that are still considered the same event. Default: \code{30}.
#' @param timestamp_col Character. Column to use for the timestamp portion
#'   of the Event_ID. Default: \code{"UTC_datetime"}.
#' @param add_event_count Logical. Add an \code{Event_count} column with
#'   the number of rows in each event. Default: \code{FALSE}.
#' @param verbose Logical. Print a summary. Default: \code{TRUE}.
#'
#' @return The input data frame with one or two new columns:
#'   \itemize{
#'     \item \code{Event_ID} — unique event identifier, format
#'       \code{"<tag_name>_<YYYY-MM-DD HH:MM:SS>"}
#'     \item \code{Event_count} — number of rows in the event
#'       (only when \code{add_event_count = TRUE})
#'   }
#'
#' @examples
#' \dontrun{
#' df     <- glosendas_download("myuser", "mypass", filter_word = "Houbara")
#' df_acc <- analyze_acc(df)
#'
#' # Add Event_ID only
#' df_ev  <- add_event_id(df_acc)
#'
#' # Add Event_ID and row count per event
#' df_ev  <- add_event_id(df_acc, add_event_count = TRUE)
#'
#' # Filter to a specific event
#' one_event <- df_ev[df_ev$Event_ID == "Houbara Kelach 2_2026-02-07 03:27:55", ]
#'
#' # Filter to large events only (e.g. flights)
#' df_ev  <- add_event_id(df_acc, add_event_count = TRUE)
#' flights <- df_ev[df_ev$Event_count > 60, ]
#' }
#'
#' @export
add_event_id <- function(df,
                         max_gap_sec     = 30,
                         timestamp_col   = "UTC_datetime",
                         add_event_count = FALSE,
                         verbose         = TRUE) {

  # ── guards ────────────────────────────────────────────────────────────────
  if (!inherits(df, "data.frame")) stop("`df` must be a data frame.")
  df <- as.data.frame(df)

  # Coerce data.table / tibble to plain data.frame
  if (!identical(class(df), "data.frame")) df <- as.data.frame(df)
  if (nrow(df) == 0) stop("`df` has zero rows.")
  if (!is.numeric(max_gap_sec) || max_gap_sec < 0)
    stop("`max_gap_sec` must be a non-negative number.")

  required <- c("datatype", "tag_name", timestamp_col)
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0)
    stop("Missing required columns: ", paste(missing, collapse = ", "))

  # ── parse timestamps ──────────────────────────────────────────────────────
  # Prefer UTC_timestamp (sub-second precision) when available.
  # Handle POSIXct, numeric (epoch seconds), and character formats robustly.

  if ("UTC_timestamp" %in% names(df)) {
    ts_raw <- df$UTC_timestamp
    ts     <- .gl_to_posix(ts_raw)
    bad    <- is.na(ts)
    if (any(bad))
      ts[bad] <- .gl_to_posix(df[[timestamp_col]][bad])
  } else {
    ts <- .gl_to_posix(df[[timestamp_col]])
  }

  ts_num <- as.numeric(ts)

  .gl_check_order(df, ts_num, "add_event_id")

  # ── classify row types ────────────────────────────────────────────────────
  is_gps <- df$datatype %in% c("GPS", "GPS_ACC", "GPSF")

  n <- nrow(df)

  # ── determine event boundaries (fully vectorised) ─────────────────────────
  prev_is_gps <- c(FALSE, is_gps[-n])
  tag_change  <- c(TRUE,  df$tag_name[-1] != df$tag_name[-n])
  time_gap    <- c(Inf,   diff(ts_num))
  gap_too_big <- is.na(time_gap) | time_gap > max_gap_sec

  # GPS/GPSF after any non-GPS row = new event (e.g. ACC_END -> GPSF)
  gps_after_acc <- is_gps & !prev_is_gps

  new_event    <- tag_change | gap_too_big | gps_after_acc
  new_event[1] <- TRUE
  event_num    <- cumsum(new_event)

  # ── build Event_ID from the first row of each event ───────────────────────
  event_first <- match(seq_len(max(event_num)), event_num)
  first_ts    <- ts[event_first]
  first_tag   <- df$tag_name[event_first]

  event_label <- paste0(first_tag, "_",
                        format(first_ts, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  df$Event_ID <- event_label[event_num]

  # ── optional Event_count ──────────────────────────────────────────────────
  if (add_event_count) {
    event_sizes    <- tabulate(event_num)
    df$Event_count <- event_sizes[event_num]
  }

  # ── verbose summary ───────────────────────────────────────────────────────
  if (verbose) {
    n_events    <- max(event_num)
    first_types <- df$datatype[event_first]
    n_gps_start <- sum(first_types %in% c("GPS", "GPSF", "GPS_ACC"))
    n_acc_start <- sum(grepl("^SEN_ACC_", first_types))
    n_oth_start <- n_events - n_gps_start - n_acc_start

    message("\n--- Event_ID Summary ---")
    message(sprintf("  Total events        : %d", n_events))
    message(sprintf("  Starting with GPS   : %d", n_gps_start))
    message(sprintf("  Starting with ACC   : %d", n_acc_start))
    message(sprintf("  Starting with other : %d", n_oth_start))
    message(sprintf("  max_gap_sec         : %g s", max_gap_sec))
    message("  Example Event_IDs:")
    for (id in head(unique(df$Event_ID), 5))
      message("    ", id)
  }

  df
}
