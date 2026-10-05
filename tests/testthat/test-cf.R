context("test-cf")

dt <- tsibble::tsibble(idx = 1:25, y = rnorm(25), x = rnorm(25), index = idx)

test_that("ACF", {
  cf <- ACF(dt, y)
  expect_identical(
    cf$acf,
    as.numeric(stats::acf(dt$y, plot = FALSE)$acf)[-1]
  )

  expect_message(
    NROW(ACF(dt, lag_max = 5)),
    "Response variable not specified"
  ) %>%
    expect_identical(5L)

  expect_warning(
    ACF(dt, y, x),
    "ACF currently only supports one column"
  )
})

test_that("PACF", {
  cf <- PACF(dt, y)
  expect_identical(
    cf$pacf,
    as.numeric(stats::pacf(dt$y, plot = FALSE)$acf)
  )

  acf <- ACF(dt, y, type = "part") # Testing also partial matching of "partial"
  names(acf) <- c("lag", "pacf") # Overwrite names c("lag", "acf")
  expect_identical(
    as_tsibble(cf),
    acf
  )

  expect_message(
    NROW(PACF(dt, lag_max = 5)),
    "Response variable not specified"
  ) %>%
    expect_identical(5L)

  expect_warning(
    PACF(dt, y, x),
    "PACF currently only supports one column"
  )
})


test_that("CCF", {
  cf <- CCF(dt, y, x)
  expect_identical(
    cf$ccf,
    as.numeric(stats::ccf(dt$x, dt$y, plot = FALSE)$acf)
  )

  expect_message(
    NROW(CCF(dt, lag_max = 5)),
    "Response variable not specified"
  ) %>%
    expect_identical(11L)

  expect_warning(
    CCF(dt, y, x, x),
    "CCF currently only supports two columns"
  )
})


test_that("as_lag() supports mixtime granules", {
  skip_if_not_installed("mixtime")
  skip_if_not(
    !inherits(tsibble::interval(tsibble::tsibble(i = 1:3, index = i)), "interval"),
    "tsibble doesn't use mixtime granules for intervals"
  )
  mt <- tsibble::tsibble(
    i = mixtime::yearmonth(0:35), y = rnorm(36), index = i
  )
  cf <- ACF(mt, y, lag_max = 12)
  expect_equal(cf$acf, as.numeric(stats::acf(mt$y, lag.max = 12, plot = FALSE)$acf)[-1])
  expect_equal(length(format(cf$lag)), 12L)

  lag <- as_lag(tsibble::interval(mt))
  expect_s3_class(lag, "cf_lag")
})

test_that("cf_lag formats legacy tsibble intervals", {
  skip_if(
    !inherits(tsibble::interval(tsibble::tsibble(i = 1:3, index = i)), "interval"),
    "tsibble uses mixtime granules for intervals"
  )
  lag <- as_lag(tsibble::interval(tsibble::tsibble(i = tsibble::yearmonth(1:3), index = i)))
  expect_identical(format(lag * 12), "12M")
})
