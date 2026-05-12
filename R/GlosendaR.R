#' @keywords internal
"_PACKAGE"

# Portal constants — not exported, used internally by all functions
.glosendas_env <- new.env(parent = emptyenv())
.glosendas_env$BASE_URL   <- "https://cpanel.glosendas.net"
.glosendas_env$LOGIN_URL  <- "https://cpanel.glosendas.net/"
.glosendas_env$POST_URL   <- "https://cpanel.glosendas.net/post.php"
.glosendas_env$DEVICE_URL <- "https://cpanel.glosendas.net/device.php"

.glosendas_env$FORMAT_LABELS <- c(
  "0" = "GPS+SENSORS",
  "1" = "GPS",
  "2" = "SENSORS",
  "3" = "GPS+SENSORS_V2",
  "4" = "GPS_V2",
  "5" = "SENSORS_V2"
)


# ==============================================================================
#' Download GPS tracking data from the Glosendas OrniTrack portal
#'
#' The main function of the package. Logs in, auto-discovers all matching
#' devices, downloads the requested data format for the given date range,
#' saves a CSV file, and returns a tidy data frame.
#'
#' @param username Character. Your Glosendas portal username.
#' @param password Character. Your Glosendas portal password.
#' @param filter_word Character. Case-insensitive keyword to filter devices by
#'   name. Only devices whose name contains this string are downloaded.
#'   Use `""` (empty string) to download all devices on the account.
#'   Default: `""`.
#' @param from_dt Character. Start of the date range in UTC, format
#'   `"YYYY-MM-DD HH:MM"`. Default: 7 days ago at midnight.
#' @param to_dt Character. End of the date range in UTC, format
#'   `"YYYY-MM-DD HH:MM"`. Default: now.
#' @param format_code Integer. Data format to download:
#'   \itemize{
#'     \item `0` = GPS+SENSORS (V1)
#'     \item `1` = GPS only (V1)
#'     \item `2` = SENSORS only (V1)
#'     \item `3` = GPS+SENSORS_V2 (default, recommended)
#'     \item `4` = GPS only (V2)
#'     \item `5` = SENSORS only (V2)
#'   }
#' @param output_dir Character. Directory where the combined CSV file is saved.
#'   Created automatically if it does not exist. Default: `"glosendas_data"`.
#' @param verbose Logical. Print progress messages. Default: `TRUE`.
#'
#' @return A data frame with all downloaded GPS/sensor records. Each row is one
#'   fix. Columns include `device_id`, `tag_name` (human-readable nickname),
#'   `UTC_datetime` (parsed as `POSIXct`), and all sensor fields provided by
#'   the portal. Returns `NULL` invisibly if no data was found.
#'
#' @examples
#' \dontrun{
#' # Download last 7 days of GPS+SENSORS_V2 data for all Houbara devices
#' df <- glosendas_download(
#'   username    = "myuser",
#'   password    = "mypass",
#'   filter_word = "Houbara"
#' )
#'
#' # Black Eagles, GPS only, custom date range
#' df <- glosendas_download(
#'   username    = "myuser",
#'   password    = "mypass",
#'   filter_word = "BE 20",
#'   format_code = 1,
#'   from_dt     = "2026-01-01 00:00",
#'   to_dt       = "2026-05-12 23:59"
#' )
#'
#' # All devices on the account
#' df <- glosendas_download(
#'   username    = "myuser",
#'   password    = "mypass",
#'   filter_word = ""
#' )
#' }
#'
#' @export
glosendas_download <- function(username,
                               password,
                               filter_word = "",
                               from_dt     = format(Sys.time() - 7 * 86400,
                                                    "%Y-%m-%d 00:00"),
                               to_dt       = format(Sys.time(),
                                                    "%Y-%m-%d %H:%M"),
                               format_code = 3,
                               output_dir  = "glosendas_data",
                               verbose     = TRUE) {

  if (!as.character(format_code) %in%
      names(.glosendas_env$FORMAT_LABELS)) {
    stop("Invalid format_code. Choose 0-5:\n",
         paste(sprintf("  %s = %s",
                       names(.glosendas_env$FORMAT_LABELS),
                       .glosendas_env$FORMAT_LABELS),
               collapse = "\n"))
  }

  login_result <- .gl_login(username, password, verbose)
  h        <- login_result$h
  page_txt <- login_result$page_txt

  registry <- .gl_discover(h, page_txt, filter_word, verbose)

  df <- .gl_download_all(h, registry, from_dt, to_dt,
                         format_code, output_dir, verbose)

  if (verbose && !is.null(df))
    message("\nDone.  Data frame returned invisibly — assign to a variable:\n",
            "  df <- glosendas_download(...)")

  invisible(df)
}


# ==============================================================================
#' List all devices visible on the portal
#'
#' Logs in and returns a data frame of every device on the account, with
#' S/N, name, and IMEI. Useful for exploring what is available before
#' downloading.
#'
#' @param username Character. Portal username.
#' @param password Character. Portal password.
#' @param filter_word Character. Optional keyword filter (case-insensitive).
#'   Default: `""` (return all devices).
#'
#' @return A data frame with columns `sn` (tag S/N), `name` (device nickname),
#'   and `imei`.
#'
#' @examples
#' \dontrun{
#' # See everything on the account
#' all_devices <- glosendas_list_devices("myuser", "mypass")
#'
#' # See only Houbara devices
#' houbara <- glosendas_list_devices("myuser", "mypass", "Houbara")
#' }
#'
#' @export
glosendas_list_devices <- function(username, password, filter_word = "") {

  login_result <- .gl_login(username, password, verbose = TRUE)
  registry     <- .gl_discover(login_result$h, login_result$page_txt,
                                filter_word, verbose = TRUE)

  df <- data.frame(
    sn   = names(registry),
    name = sapply(registry, `[[`, "name"),
    imei = sapply(registry, `[[`, "imei"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  df[order(df$name), ]
}


# ==============================================================================
# INTERNAL FUNCTIONS  (not exported, prefixed with .gl_)
# ==============================================================================

#' @noRd
.gl_login <- function(username, password, verbose = TRUE) {

  if (verbose) message("Logging in as '", username, "' ...")

  h <- httr::handle(.glosendas_env$BASE_URL)

  if (httr::status_code(httr::GET(.glosendas_env$LOGIN_URL,
                                   httr::handle(h))) != 200)
    stop("Cannot reach portal.")

  body <- paste0(
    "username=", curl::curl_escape(username),
    "&password=", curl::curl_escape(password),
    "&login=Login&resx=1920&resy=1080&resax=1920&resay=937&reso=-90"
  )
  resp <- httr::POST(
    .glosendas_env$POST_URL, body = body, encode = "raw",
    httr::handle(h),
    httr::add_headers(`Content-Type` = "application/x-www-form-urlencoded")
  )

  if (httr::status_code(resp) != 200) stop("Login POST failed.")
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  if (grepl('name="login"', txt, fixed = TRUE))
    stop("Login failed — check username and password.")

  if (verbose) message("  Login successful.")
  list(h = h, page_txt = txt)
}


#' @noRd
.gl_discover <- function(h, page_txt, filter_word = "", verbose = TRUE) {

  if (verbose) {
    if (nzchar(trimws(filter_word)))
      message("Discovering devices matching '", filter_word, "' from portal ...")
    else
      message("Discovering all devices from portal ...")
  }

  txt <- gsub("\r", "", page_txt, fixed = TRUE)

  m <- stringr::str_match_all(
    txt, "title: '(\\d+): ([^\\\\]+)\\\\n[^']*',\nid: '(\\d+)'")[[1]]

  if (nrow(m) == 0)
    m <- stringr::str_match_all(
      txt,
      "title: '(\\d+): ([^\\\\]+)\\\\n[^']*'[^\n]{0,50}id: '(\\d+)'")[[1]]

  if (nrow(m) == 0)
    stop("No devices found on portal page. ",
         "The page structure may have changed — please file an issue.")

  sns   <- m[, 2]
  nms   <- trimws(m[, 3])
  imeis <- m[, 4]

  if (nzchar(trimws(filter_word))) {
    keep <- grepl(filter_word, nms, ignore.case = TRUE)
    if (!any(keep))
      stop("No devices matching '", filter_word, "' found. (",
           nrow(m), " total devices on portal.)")
    sns   <- sns[keep]
    nms   <- nms[keep]
    imeis <- imeis[keep]
  }

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

  resp <- httr::POST(
    .glosendas_env$DEVICE_URL, body = body, encode = "raw",
    httr::handle(h),
    httr::add_headers(
      `Content-Type`     = "application/x-www-form-urlencoded",
      `X-Requested-With` = "XMLHttpRequest",
      Referer            = .glosendas_env$LOGIN_URL
    )
  )

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

  resp <- httr::POST(
    .glosendas_env$POST_URL, body = body, encode = "raw",
    httr::handle(h),
    httr::add_headers(
      `Content-Type` = "application/x-www-form-urlencoded",
      Referer        = .glosendas_env$LOGIN_URL,
      Origin         = .glosendas_env$BASE_URL
    )
  )

  if (httr::status_code(resp) != 200)
    return(list(status = "http_error", lines = NULL))

  raw <- httr::content(resp, "raw")
  if (length(raw) < 10) return(list(status = "empty", lines = NULL))

  peek <- rawToChar(raw[seq_len(min(200, length(raw)))])
  if (grepl("<!DOCTYPE|<html", peek, ignore.case = TRUE))
    return(list(status = "html_error", lines = NULL))

  lines <- strsplit(rawToChar(raw), "\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) < 2) return(list(status = "empty_csv", lines = NULL))

  list(status = "ok", lines = lines)
}


#' @noRd
.gl_download_all <- function(h, registry, from_dt, to_dt,
                              format_code, output_dir, verbose) {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
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

  # Save CSV
  ts       <- format(Sys.time(), "%Y%m%d_%H%M%S")
  out_file <- file.path(output_dir,
                        paste0("glosendas_", tolower(format_label),
                               "_", ts, ".csv"))
  writeLines(c(header_row, unlist(all_lines)), out_file)
  if (verbose)
    message(sprintf("\n  Saved : %s (%.1f KB)",
                    out_file, file.size(out_file) / 1024))

  # Build data frame
  df           <- utils::read.csv(out_file, stringsAsFactors = FALSE,
                                  check.names = FALSE)
  df           <- df[, !grepl("^\\s*$", names(df)), drop = FALSE]
  df$device_id <- as.character(df$device_id)

  name_lookup  <- sapply(registry, `[[`, "name")
  df$tag_name  <- name_lookup[df$device_id]

  dt_col <- grep("UTC_datetime|datetime|timestamp", names(df),
                 ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(dt_col))
    df[[dt_col]] <- as.POSIXct(df[[dt_col]], format = "%Y-%m-%d %H:%M:%S",
                                tz = "UTC")

  id_pos    <- which(names(df) == "device_id")
  col_order <- c(names(df)[seq_len(id_pos)], "tag_name",
                 setdiff(names(df)[(id_pos + 1):ncol(df)], "tag_name"))
  df <- df[, col_order]

  if (verbose) {
    message(sprintf("  `glosendas_data` ready : %d rows x %d columns",
                    nrow(df), ncol(df)))
    .gl_preview(df, skipped, registry)
  }

  df
}


#' @noRd
.gl_preview <- function(df, skipped, registry) {

  message("\n--- Downloaded Data Preview ---")

  dt_col <- grep("UTC_datetime|datetime|timestamp", names(df),
                 ignore.case = TRUE, value = TRUE)[1]

  smry <- do.call(rbind, lapply(split(df, df$device_id), function(sub) {
    if (!is.na(dt_col) && inherits(df[[dt_col]], "POSIXct"))
      data.frame(
        sn        = sub$device_id[1],
        name      = sub$tag_name[1],
        rows      = nrow(sub),
        first_fix = format(min(sub[[dt_col]], na.rm = TRUE), "%Y-%m-%d %H:%M"),
        last_fix  = format(max(sub[[dt_col]], na.rm = TRUE), "%Y-%m-%d %H:%M"),
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
