test_that("format_code validation works", {
  expect_error(
    glosendas_download("u", "p", format_code = 99),
    "Invalid format_code"
  )
})

test_that("format labels are correct", {
  expect_equal(.glosendas_env$FORMAT_LABELS[["3"]], "GPS+SENSORS_V2")
  expect_equal(.glosendas_env$FORMAT_LABELS[["1"]], "GPS")
  expect_equal(length(.glosendas_env$FORMAT_LABELS), 6L)
})

test_that("default date range is in UTC and sensible", {
  from <- format(as.POSIXct(Sys.time(), tz = "UTC") - 7 * 86400,
                 "%Y-%m-%d 00:00", tz = "UTC")
  to   <- format(as.POSIXct(Sys.time(), tz = "UTC"),
                 "%Y-%m-%d %H:%M", tz = "UTC")
  expect_true(as.POSIXct(from, tz = "UTC") < as.POSIXct(to, tz = "UTC"))
})

test_that(".gl_to_utc converts labelled timezones to UTC", {
  # plain string is taken as UTC and truncated to minutes
  expect_equal(.gl_to_utc("2023-03-06 18:47:51"), "2023-03-06 18:47")
  # to_dt rounds up so no data at the end of the window is missed
  expect_equal(.gl_to_utc("2023-03-06 18:47:51", round_up = TRUE),
               "2023-03-06 18:48")
  # POSIXct in another timezone is converted, not relabelled
  p <- as.POSIXct("2023-03-06 18:47:00", tz = "UTC")
  expect_equal(.gl_to_utc(p), "2023-03-06 18:47")
})

test_that(".gl_to_posix handles POSIXct, numeric and character input", {
  target <- as.POSIXct("2024-06-01 12:34:56", tz = "UTC")
  expect_equal(as.numeric(.gl_to_posix(target)), as.numeric(target))
  expect_equal(as.numeric(.gl_to_posix(as.numeric(target))), as.numeric(target))
  expect_equal(as.numeric(.gl_to_posix("2024-06-01 12:34:56")), as.numeric(target))
  # fractional seconds are preserved
  frac <- .gl_to_posix("2024-06-01 12:34:56.250")
  expect_equal(as.numeric(frac) - as.numeric(target), 0.25, tolerance = 1e-6)
  # alternative locale format
  expect_equal(as.numeric(.gl_to_posix("01/06/2024 12:34:56")), as.numeric(target))
})

test_that(".gl_classify recognises every row type", {
  cls <- .gl_classify(c("GPS", "GPSF", "GPS_ACC", "SENSORS",
                        "SEN_ACC_10Hz_START", "SEN_ACC_10Hz",
                        "SEN_ACC_10Hz_END"))
  expect_equal(cls$gps,       c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(cls$sensors,   c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(cls$acc_any,   c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(cls$acc_start, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(cls$acc_end,   c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
})
