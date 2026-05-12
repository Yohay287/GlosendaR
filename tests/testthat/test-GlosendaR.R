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

test_that("default date range is sensible", {
  from <- format(Sys.time() - 7 * 86400, "%Y-%m-%d 00:00")
  to   <- format(Sys.time(), "%Y-%m-%d %H:%M")
  expect_true(as.POSIXct(from) < as.POSIXct(to))
})
