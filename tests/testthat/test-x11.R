context("test-x11")

skip_if(!is.null(safely(seasonal::checkX13)(fail = TRUE)$error))

test_that("Bad inputs for X11 decomposition", {
  expect_error(
    tsibble::pedestrian %>%
      filter(Sensor == "Southern Cross Station",
             Date == as.Date("2015-01-01")) %>%
      feasts:::X11(Count),
    "The X11 method only supports monthly"
  )

  expect_error(
    as_tsibble(co2) %>%
      feasts:::X11(value ~ seq_along(value)),
    "Exogenous regressors are not supported for X11 decompositions"
  )
})

test_that("Additive X11 decomposition", {
  tsbl_co2 <- as_tsibble(co2)
  dcmp <- tsbl_co2 %>% feasts:::X11(value)
  seas_dcmp <- seasonal::seas(co2, x11="", x11.mode = "add",
                              transform.function = "none")

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
    dcmp$value - dcmp$seasonal,
    dcmp$season_adjust
  )
})

test_that("Multiplicative X11 decomposition", {
  tsbl_uad <- as_tsibble(USAccDeaths)
  dcmp <- tsbl_uad %>% feasts:::X11(value, type = "multiplicative")
  seas_dcmp <- seasonal::seas(USAccDeaths, x11="", x11.mode = "mult",
                              transform.function = "log")

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
