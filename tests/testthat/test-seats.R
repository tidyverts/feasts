context("test-seats")

skip_if(!is.null(safely(seasonal::checkX13)(fail = TRUE)$error))

tsbl_co2 <- as_tsibble(co2)
test_that("Bad inputs for seats decomposition", {
  expect_error(
    tsibble::pedestrian %>%
      filter(Sensor == "Southern Cross Station",
             Date == as.Date("2015-01-01")) %>%
      feasts:::SEATS(Count),
    "The X-13ARIMA-SEATS method only supports seasonal patterns"
  )

  expect_error(
    tsbl_co2 %>%
      feasts:::SEATS(value ~ seq_along(value)),
    "Exogenous regressors are not supported for X-13ARIMA-SEATS decompositions"
  )

  expect_error(
    tsbl_co2 %>%
      feasts:::SEATS(value, x11=""),
    "Use \\`X11\\(\\)\\` to perform an X11 decomposition"
  )
})

test_that("X-13ARIMA-SEATS decomposition", {
  dcmp <- tsbl_co2 %>% feasts:::SEATS(value)
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
