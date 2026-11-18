context("test-seats")

skip_if(!is.null(safely(seasonal::checkX13)(fail = TRUE)$error))
skip_if(!.Platform$OS.type == "windows" && Sys.info()["sysname"] != "Linux")

tsbl_co2 <- as_tsibble(co2)
test_that("Bad inputs for seats decomposition", {
  expect_warning(
    tsibble::pedestrian %>%
      filter(Sensor == "Southern Cross Station",
             Date == as.Date("2015-01-01")) %>%
      model(feasts:::SEATS(Count)),
    "The X-13ARIMA-SEATS method only supports seasonal patterns"
  )

  expect_warning(
    tsbl_co2 %>%
      model(feasts:::SEATS(value ~ seq_along(value))),
    "Exogenous regressors are not supported for X-13ARIMA-SEATS decompositions"
  )

  expect_warning(
    tsbl_co2 %>%
      model(feasts:::SEATS(value, x11="")),
    "Use \\`X11\\(\\)\\` to perform an X11 decomposition"
  )
})

test_that("X-13ARIMA-SEATS decomposition", {
  dcmp <- tsbl_co2 %>% model(feasts:::SEATS(value)) %>% components()
  seas_dcmp <- seasonal::seas(co2)

  expect_equivalent(
    dcmp$trend,
    unclass(seas_dcmp$data[,"trend"])
  )
  expect_equivalent(
    dcmp$seasonal,
    unclass(seas_dcmp$data[,"adjustfac"])
  )
  expect_equivalent(
    dcmp$irregular,
    unclass(seas_dcmp$data[,"irregular"])
  )
  expect_equal(
    dcmp$value / dcmp$seasonal,
    dcmp$season_adjust
  )
})

test_that("X-13ARIMA-SEATS restores the index class", {
  x <- as.numeric(co2)[1:120]
  tsbl <- tsibble::tsibble(i = tsibble::yearmonth(0:119), y = x, index = i)
  fit <- tsbl %>% model(feasts:::SEATS(y))
  dcmp <- components(fit)
  expect_identical(class(dcmp$i), class(tsbl$i))
  expect_equal(dcmp$i, tsbl$i)
})

test_that("restore_index is class-agnostic with fallback", {
  idx <- tsibble::yearmonth(0:5)
  expect_identical(feasts:::restore_index(1:3, NULL), 1:3)
  expect_identical(feasts:::restore_index(idx, idx[0]), idx)

  skip_if_not_installed("mixtime")
  # Monthly mixtime restored from a legacy yearmonth
  mm <- mixtime::yearmonth(0:5)
  res <- feasts:::restore_index(idx, vctrs::vec_ptype(mm))
  expect_identical(class(res), class(mm))
  # Sub-daily datetimes keep their time of day
  dt <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + 3600 * (0:5)
  mt <- mixtime::as_mixtime(dt)
  res <- feasts:::restore_index(dt, vctrs::vec_ptype(mt))
  expect_identical(class(res), class(mt))
  expect_equal(as.POSIXct(res, tz = "UTC"), dt)
})
