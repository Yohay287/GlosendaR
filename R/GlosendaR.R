#' @keywords internal
"_PACKAGE"

# Portal constants
.glosendas_env <- new.env(parent = emptyenv())
.glosendas_env$BASE_URL   <- "https://cpanel.glosendas.net"
.glosendas_env$LOGIN_URL  <- "https://cpanel.glosendas.net/"
.glosendas_env$POST_URL   <- "https://cpanel.glosendas.net/post.php"
.glosendas_env$DEVICE_URL <- "https://cpanel.glosendas.net/device.php"
.glosendas_env$FORMAT_LABELS <- c(
  "0" = "GPS+SENSORS", "1" = "GPS",      "2" = "SENSORS",
  "3" = "GPS+SENSORS_V2", "4" = "GPS_V2", "5" = "SENSORS_V2"
)


# ==============================================================================
#' Download GPS tracking data from the Glosendas OrniTrack portal
#'
#' Logs in, auto-discovers matching devices, downloads the requested format
#' for the given date range, and returns a tidy data frame. Optionally saves
#' a CSV backup.
#'
#' Column names are normalised across V1 and V2 formats so that downstream
#' code (including \code{\link{analyze_acc}}) works identically regardless of
#' which format was chosen. Blank lines emitted by V1 formats are stripped
#' automatically.
#'
#' @param username Character. Portal username.
#' @param password Character. Portal password.
#' @param filter_word Character. Case-insensitive keyword to filter devices by
#'   name. Use \code{""} to download all devices. Default: \code{""}.
#' @param tag_numbers Character or numeric vector of specific tag S/N numbers
#'   to download. \code{NULL} means no S/N filter. Default: \code{NULL}.
#' @param from_dt Character. Start of date range, UTC, \code{"YYYY-MM-DD HH:MM"}.
#'   Default: 7 days ago.
#' @param to_dt Character. End of date range, UTC, \code{"YYYY-MM-DD HH:MM"}.
#'   Default: now.
#' @param format_code Integer \code{0}–\code{5}:
#'   \code{0} GPS+SENSORS, \code{1} GPS, \code{2} SENSORS,
#'   \code{3} GPS+SENSORS_V2 (default), \code{4} GPS_V2, \code{5} SENSORS_V2.
#' @param drop_empty_cols Logical. Drop columns that are entirely \code{NA}
#'   (e.g. \code{depth_m}). Default: \code{TRUE}.
#' @param verbose Logical. Print progress messages. Default: \code{TRUE}.
#'
#' @return A tidy data frame. Column names are normalised across formats:
#'   \code{altitude_m}, \code{speed_kmh}, \code{temperature_C} are always
#'   present regardless of V1/V2 format.
#'
#' @examples
#' \dontrun{
#' df <- glosendas_download("myuser", "mypass", filter_word = "Houbara")
#'
#' df <- glosendas_download("myuser", "mypass",
#'   tag_numbers = c("216417", "223083"),
#'   format_code = 3, save_csv = FALSE)
#' }
#'
#' @export
glosendas_download <- function(username,
                               password,
                               filter_word     = "",
                               tag_numbers     = NULL,
                               from_dt         = format(Sys.time() - 7 * 86400,
                                                        "%Y-%m-%d 00:00"),
                               to_dt           = format(Sys.time(),
                                                        "%Y-%m-%d %H:%M"),
                               format_code     = 3,
                               save_csv        = FALSE,
                               output_dir      = "glosendas_data",
                               drop_empty_cols = TRUE,
                               verbose         = TRUE) {

  if (!as.character(format_code) %in% names(.glosendas_env$FORMAT_LABELS))
    stop("Invalid format_code. Choose 0-5:\n",
         paste(sprintf("  %s = %s",
                       names(.glosendas_env$FORMAT_LABELS),
                       .glosendas_env$FORMAT_LABELS), collapse = "\n"))

  if (!is.null(tag_numbers)) tag_numbers <- as.character(tag_numbers)

  login_result <- .gl_login(username, password, verbose)
  h        <- login_result$h
  page_txt <- login_result$page_txt

  registry <- .gl_discover(h, page_txt, filter_word, tag_numbers, verbose)

  df <- .gl_download_all(h, registry, from_dt, to_dt,
                         format_code, save_csv, output_dir,
                         drop_empty_cols, verbose)

  if (verbose && !is.null(df)) message("\nDone.")
  invisible(df)
}


# ==============================================================================
#' List all devices visible on the portal
#'
#' @param username Character. Portal username.
#' @param password Character. Portal password.
#' @param filter_word Character. Optional name keyword filter. Default: \code{""}.
#' @param tag_numbers Character or numeric vector of S/N numbers. Default: \code{NULL}.
#'
#' @return Data frame with columns \code{sn}, \code{name}, \code{imei}.
#'
#' @examples
#' \dontrun{
#' glosendas_list_devices("myuser", "mypass", filter_word = "Houbara")
#' }
#'
#' @export
glosendas_list_devices <- function(username, password,
                                   filter_word = "",
                                   tag_numbers = NULL) {
  if (!is.null(tag_numbers)) tag_numbers <- as.character(tag_numbers)
  login_result <- .gl_login(username, password, verbose = TRUE)
  registry     <- .gl_discover(login_result$h, login_result$page_txt,
                                filter_word, tag_numbers, verbose = TRUE)
  df <- data.frame(
    sn   = names(registry),
    name = sapply(registry, function(x) x[["name"]]),
    imei = sapply(registry, function(x) x[["imei"]]),
    stringsAsFactors = FALSE, row.names = NULL
  )
  df[order(df$name), ]
}


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

#' @noRd
.gl_login <- function(username, password, verbose = TRUE) {
  if (verbose) message("Logging in as '", username, "' ...")
  h <- httr::handle(.glosendas_env$BASE_URL)
  if (httr::status_code(httr::GET(.glosendas_env$LOGIN_URL, handle = h)) != 200)
    stop("Cannot reach portal.")
  body <- paste0(
    "username=", curl::curl_escape(username),
    "&password=", curl::curl_escape(password),
    "&login=Login&resx=1920&resy=1080&resax=1920&resay=937&reso=-90"
  )
  resp <- httr::POST(.glosendas_env$POST_URL, body = body, encode = "raw",
                     handle = h,
                     httr::add_headers(
                       `Content-Type` = "application/x-www-form-urlencoded"))
  if (httr::status_code(resp) != 200) stop("Login POST failed.")
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  if (grepl('name="login"', txt, fixed = TRUE))
    stop("Login failed — check username and password.")
  if (verbose) message("  Login successful.")
  list(h = h, page_txt = txt)
}


#' @noRd
.gl_discover <- function(h, page_txt, filter_word = "",
                          tag_numbers = NULL, verbose = TRUE) {
  if (verbose) {
    filters <- c(
      if (nzchar(trimws(filter_word)))
        paste0("name contains '", filter_word, "'"),
      if (!is.null(tag_numbers))
        paste0("S/N in [", paste(tag_numbers, collapse = ", "), "]")
    )
    if (length(filters) == 0)
      message("Discovering all devices from portal ...")
    else
      message("Discovering devices where ",
              paste(filters, collapse = " AND "), " ...")
  }
  txt <- gsub("\r", "", page_txt, fixed = TRUE)
  m <- stringr::str_match_all(
    txt, "title: '(\\d+): ([^\\\\]+)\\\\n[^']*',\nid: '(\\d+)'")[[1]]
  if (nrow(m) == 0)
    m <- stringr::str_match_all(
      txt,
      "title: '(\\d+): ([^\\\\]+)\\\\n[^']*'[^\n]{0,50}id: '(\\d+)'")[[1]]
  if (nrow(m) == 0)
    stop("No devices found on portal page. Page structure may have changed.")

  sns   <- m[, 2]; nms <- trimws(m[, 3]); imeis <- m[, 4]

  if (nzchar(trimws(filter_word))) {
    keep  <- grepl(filter_word, nms, ignore.case = TRUE)
    sns   <- sns[keep]; nms <- nms[keep]; imeis <- imeis[keep]
  }
  if (!is.null(tag_numbers)) {
    keep    <- sns %in% tag_numbers
    missing <- setdiff(tag_numbers, sns[keep])
    if (length(missing) > 0)
      warning("Tag S/Ns not found on portal: ",
              paste(missing, collapse = ", "), call. = FALSE)
    sns <- sns[keep]; nms <- nms[keep]; imeis <- imeis[keep]
  }
  if (length(sns) == 0)
    stop("No devices matched the given filters. (",
         nrow(m), " total on portal.)")

  registry <- list()
  for (i in seq_along(sns)) {
    sn <- sns[i]
    if (!sn %in% names(registry))
      registry[[sn]] <- list(imei = imeis[i], name = nms[i])
  }
  if (verbose)
    message("  Found ", length(registry), " matching device(s) out of ",
            nrow(m), " total on portal.")
  registry
}


#' @noRd
.gl_get_token <- function(h, imei) {
  body <- paste0("devid=", imei, "&devopt=0&t=",
                 format(as.numeric(Sys.time()) * 1000, scientific = FALSE))
  resp <- httr::POST(.glosendas_env$DEVICE_URL, body = body, encode = "raw",
                     handle = h,
                     httr::add_headers(
                       `Content-Type`     = "application/x-www-form-urlencoded",
                       `X-Requested-With` = "XMLHttpRequest",
                       Referer            = .glosendas_env$LOGIN_URL))
  if (httr::status_code(resp) != 200) return(NULL)
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  if (nchar(txt) < 10 || txt == "Login failed!") return(NULL)
  for (part in strsplit(txt, "\x1e", fixed = TRUE)[[1]]) {
    m <- stringr::str_match(part, 'name="dl([0-9a-zA-Z]+)cc"')
    if (!is.na(m[1, 2])) return(m[1, 2])
  }
  NULL
}


#' @noRd
.gl_download_one <- function(h, imei, from_dt, to_dt, format_code) {
  token <- .gl_get_token(h, imei)
  if (is.null(token)) return(list(status = "no_token", lines = NULL))
  body <- paste0(
    "dnlfromdt=", curl::curl_escape(from_dt),
    "&dnltodt=",  curl::curl_escape(to_dt),
    "&dnlselcc=", format_code,
    "&dl", token, "cc="
  )
  resp <- httr::POST(.glosendas_env$POST_URL, body = body, encode = "raw",
                     handle = h,
                     httr::add_headers(
                       `Content-Type` = "application/x-www-form-urlencoded",
                       Referer        = .glosendas_env$LOGIN_URL,
                       Origin         = .glosendas_env$BASE_URL))
  if (httr::status_code(resp) != 200)
    return(list(status = "http_error", lines = NULL))
  raw <- httr::content(resp, "raw")
  if (length(raw) < 10) return(list(status = "empty", lines = NULL))
  peek <- rawToChar(raw[seq_len(min(200, length(raw)))])
  if (grepl("<!DOCTYPE|<html", peek, ignore.case = TRUE))
    return(list(status = "html_error", lines = NULL))
  # The portal uses \r\r\n as line endings (carriage return + carriage return + newline).
  # We normalise to plain \n before splitting, which eliminates all blank lines.
  txt_raw <- rawToChar(raw)
  txt_raw <- gsub("\r\r\n", "\n", txt_raw, fixed = TRUE)  # portal-specific: \r\r\n -> \n
  txt_raw <- gsub("\r\n",   "\n", txt_raw, fixed = TRUE)  # standard Windows: \r\n -> \n
  txt_raw <- gsub("\r",     "\n", txt_raw, fixed = TRUE)  # bare \r -> \n
  lines   <- strsplit(txt_raw, "\n", fixed = TRUE)[[1]]
  lines   <- lines[nzchar(trimws(lines))]
  if (length(lines) < 2) return(list(status = "empty_csv", lines = NULL))
  list(status = "ok", lines = lines)
}


#' @noRd
#' Normalise column names that differ between V1 and V2 formats.
#' V1 uses:  Altitude_m, speed_km_h, temperature_C
#' V2 uses:  MSL_altitude_m, speed_km/h, int_temperature_C
#' We rename to a single canonical set so all downstream code is format-agnostic.
.gl_normalise_cols <- function(df) {
  renames <- c(
    "MSL_altitude_m"     = "altitude_m",
    "Altitude_m"         = "altitude_m",
    "speed_km/h"         = "speed_kmh",
    "speed_km_h"         = "speed_kmh",
    "int_temperature_C"  = "temperature_C",
    "UTC_timestamp"      = "UTC_timestamp"   # keep as-is, just ensure present
  )
  for (old_name in names(renames)) {
    new_name <- renames[[old_name]]
    if (old_name %in% names(df) && !new_name %in% names(df))
      names(df)[names(df) == old_name] <- new_name
  }
  # Remove the trailing empty column V1 formats sometimes append
  df <- df[, !grepl("^\\s*$", names(df)), drop = FALSE]
  df
}


#' @noRd
.gl_download_all <- function(h, registry, from_dt, to_dt,
                              format_code, save_csv, output_dir,
                              drop_empty_cols, verbose) {
  sns          <- names(registry)
  format_label <- .glosendas_env$FORMAT_LABELS[as.character(format_code)]

  if (verbose) {
    message("\nDownloading ", format_label, " data ...")
    message("  From    : ", from_dt)
    message("  To      : ", to_dt)
    message("  Devices : ", length(sns))
    message("")
  }

  all_lines  <- list()
  header_row <- NULL
  skipped    <- character(0)

  for (i in seq_along(sns)) {
    sn   <- sns[i]
    imei <- registry[[sn]]$imei
    name <- registry[[sn]]$name
    if (verbose)
      message(sprintf("  [%2d/%d] %-35s (S/N %s) ... ",
                      i, length(sns), name, sn), appendLF = FALSE)
    result <- .gl_download_one(h, imei, from_dt, to_dt, format_code)
    if (result$status != "ok") {
      if (verbose) message("SKIP (", result$status, ")")
      skipped <- c(skipped, sn)
      Sys.sleep(0.5)
      next
    }
    lines <- result$lines
    if (is.null(header_row)) {
      header_row      <- lines[1]
      all_lines[[sn]] <- lines[-1]
    } else {
      all_lines[[sn]] <- lines[-1]
    }
    if (verbose) message(length(lines) - 1, " rows")
    Sys.sleep(0.4)
  }

  if (length(all_lines) == 0) {
    if (verbose) message("\nNo data downloaded for any device.")
    return(invisible(NULL))
  }

  combined <- c(header_row, unlist(all_lines))

  # Build data frame
  df <- tryCatch(
    utils::read.csv(text = paste(combined, collapse = "\n"),
                    stringsAsFactors = FALSE, check.names = FALSE,
                    na.strings = c("NA", "", "na", "N/A")),
    error = function(e) stop("Failed to parse downloaded CSV: ", conditionMessage(e))
  )

  # Drop gap rows — the portal emits a blank line after every data row;
  # these become all-NA rows after read.csv(). Any row without a device_id is a gap.
  if ("device_id" %in% names(df)) {
    n_gap <- sum(is.na(df$device_id) | trimws(as.character(df$device_id)) == "")
    if (n_gap > 0) {
      if (verbose)
        message(sprintf("  Removed %d gap row(s) (blank lines from portal)", n_gap))
      df <- df[!is.na(df$device_id) & trimws(as.character(df$device_id)) != "", ]
      rownames(df) <- NULL
    }
  }

  # Normalise column names across V1/V2 formats
  df <- .gl_normalise_cols(df)

  df$device_id <- as.character(df$device_id)

  # Add tag_name
  name_lookup <- sapply(registry, function(x) x[["name"]])
  df$tag_name <- name_lookup[df$device_id]

  # Parse UTC_datetime (try multiple formats)
  dt_col <- grep("^UTC_datetime$", names(df), value = TRUE)[1]
  if (!is.na(dt_col)) {
    df[[dt_col]] <- .gl_parse_datetime(df[[dt_col]])
  }

  # Put tag_name immediately after device_id
  id_pos    <- which(names(df) == "device_id")
  col_order <- c(names(df)[seq_len(id_pos)], "tag_name",
                 setdiff(names(df)[(id_pos + 1):ncol(df)], "tag_name"))
  df <- df[, col_order]

  # Drop entirely-NA columns
  if (drop_empty_cols) {
    all_na    <- vapply(df, function(x) all(is.na(x)), logical(1))
    n_dropped <- sum(all_na)
    if (n_dropped > 0 && verbose)
      message(sprintf("  Dropped %d empty column(s): %s",
                      n_dropped, paste(names(df)[all_na], collapse = ", ")))
    df <- df[, !all_na, drop = FALSE]
  }

  # Save clean CSV (written from the R dataframe — no gap rows)
  if (save_csv) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    ts       <- format(Sys.time(), "%Y%m%d_%H%M%S")
    out_file <- file.path(output_dir,
                          paste0("glosendas_",
                                 tolower(gsub("[^a-zA-Z0-9]", "_",
                                              format_label)),
                                 "_", ts, ".csv"))
    utils::write.csv(df, out_file, row.names = FALSE, na = "NA")
    if (verbose)
      message(sprintf("  Saved : %s (%.1f KB)",
                      out_file, file.size(out_file) / 1024))
  }

  if (verbose) {
    message(sprintf("  Data frame ready : %d rows x %d columns",
                    nrow(df), ncol(df)))
    .gl_preview(df, skipped, registry)
  }
  df
}


#' @noRd
#' Try multiple datetime formats (V1 and V2 differ in precision).
.gl_parse_datetime <- function(x) {
  formats <- c(
    "%Y-%m-%d %H:%M:%S",   # V2: "2026-05-12 00:06:12"
    "%Y-%m-%d %H:%M",      # fallback minute precision
    "%d/%m/%Y %H:%M:%S",   # alternative locale
    "%d/%m/%Y %H:%M"
  )
  result <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
  remaining <- seq_along(x)
  for (fmt in formats) {
    if (length(remaining) == 0) break
    parsed <- suppressWarnings(
      as.POSIXct(x[remaining], format = fmt, tz = "UTC"))
    ok <- !is.na(parsed)
    result[remaining[ok]] <- parsed[ok]
    remaining <- remaining[!ok]
  }
  result
}


#' @noRd
.gl_preview <- function(df, skipped, registry) {
  message("\n--- Downloaded Data Preview ---")
  dt_col <- grep("^UTC_datetime$", names(df), value = TRUE)[1]
  smry <- do.call(rbind, lapply(split(df, df$device_id), function(sub) {
    if (!is.na(dt_col) && inherits(df[[dt_col]], "POSIXct"))
      data.frame(sn = sub$device_id[1], name = sub$tag_name[1],
                 rows = nrow(sub),
                 first_fix = format(min(sub[[dt_col]], na.rm = TRUE),
                                    "%Y-%m-%d %H:%M"),
                 last_fix  = format(max(sub[[dt_col]], na.rm = TRUE),
                                    "%Y-%m-%d %H:%M"),
                 stringsAsFactors = FALSE)
    else
      data.frame(sn = sub$device_id[1], name = sub$tag_name[1],
                 rows = nrow(sub), stringsAsFactors = FALSE)
  }))
  rownames(smry) <- NULL
  smry <- smry[order(smry$sn), ]
  message(sprintf("  Total rows : %d  |  Tags with data: %d  |  No data: %d",
                  nrow(df), nrow(smry), length(skipped)))
  message("")
  message(sprintf("  %-8s  %-35s  %6s  %-16s  %-16s",
                  "S/N", "Name", "rows", "first_fix (UTC)", "last_fix (UTC)"))
  message("  ", strrep("-", 92))
  for (i in seq_len(nrow(smry))) {
    r <- smry[i, ]
    if ("first_fix" %in% names(r))
      message(sprintf("  %-8s  %-35s  %6d  %-16s  %-16s",
                      r$sn, r$name, r$rows, r$first_fix, r$last_fix))
    else
      message(sprintf("  %-8s  %-35s  %6d", r$sn, r$name, r$rows))
  }
  if (length(skipped) > 0) {
    message("  ", strrep("-", 92))
    message("  No data in this period:")
    for (sn in skipped)
      message(sprintf("    %-8s  %s", sn, registry[[sn]]$name))
  }
  message("  ", strrep("-", 92))
  invisible(smry)
}



# ==============================================================================
#' Save a Glosendas data frame to a clean CSV file
#'
#' Exports a data frame returned by \code{\link{glosendas_download}} (or
#' processed by \code{\link{analyze_acc}}) to a CSV. Because the export is
#' from the already-clean R object, the output contains no gap rows — unlike
#' saving directly from the portal which includes a blank line after every row.
#'
#' @param df Data frame to save.
#' @param output_dir Character. Folder to save into. Created automatically if
#'   it does not exist. Default: \code{"glosendas_data"}.
#' @param filename Character. Custom filename (without path). If \code{NULL}
#'   (default) a timestamped name is generated automatically.
#' @param verbose Logical. Print the saved path. Default: \code{TRUE}.
#'
#' @return The full file path of the saved CSV, invisibly.
#'
#' @examples
#' \dontrun{
#' df <- glosendas_download("myuser", "mypass", filter_word = "Houbara")
#' glosendas_save(df)
#' glosendas_save(df, output_dir = "C:/MyData", filename = "houbara_may2026.csv")
#'
#' gps_df <- analyze_acc(df)
#' glosendas_save(gps_df, output_dir = "C:/MyData")
#' }
#'
#' @export
glosendas_save <- function(df,
                           output_dir = "glosendas_data",
                           filename   = NULL,
                           verbose    = TRUE) {

  if (!is.data.frame(df)) stop("`df` must be a data frame.")
  if (nrow(df) == 0)      stop("`df` has zero rows.")

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  if (is.null(filename)) {
    ts       <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("glosendas_", ts, ".csv")
  }

  out_file <- file.path(output_dir, filename)
  utils::write.csv(df, out_file, row.names = FALSE, na = "NA")

  fsize <- file.size(out_file)
  if (verbose)
    message(sprintf("Saved: %s (%.1f KB, %d rows x %d cols)",
                    out_file, fsize / 1024, nrow(df), ncol(df)))

  invisible(out_file)
}

# ==============================================================================
#' Remove gap rows from a Glosendas data frame
#'
#' The Glosendas portal emits one blank line after every data row. When a CSV
#' is loaded directly with \code{read.csv()}, these become rows of all
#' \code{NA}s. \code{glosendas_clean()} removes them. Data frames returned by
#' \code{glosendas_download()} are already cleaned automatically; use this
#' function when loading a previously-saved CSV file.
#'
#' @param df A data frame loaded from a Glosendas CSV file.
#' @param verbose Logical. Report how many rows were removed. Default: \code{TRUE}.
#'
#' @return The data frame with gap rows removed.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("glosendas_gps_sensors_v2_20260515.csv",
#'               stringsAsFactors = FALSE, check.names = FALSE)
#' df <- glosendas_clean(df)
#' }
#'
#' @export
glosendas_clean <- function(df, verbose = TRUE) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.")
  if (!"device_id" %in% names(df))
    stop("`df` must have a `device_id` column.")

  gap_mask <- is.na(df$device_id) |
              trimws(as.character(df$device_id)) == ""

  n_gap <- sum(gap_mask)
  if (n_gap == 0) {
    if (verbose) message("No gap rows found.")
    return(df)
  }

  df <- df[!gap_mask, ]
  rownames(df) <- NULL

  if (verbose)
    message(sprintf("Removed %d gap row(s). %d rows remaining.", n_gap, nrow(df)))

  df
}
