context("test-classical")

test_that("Additive classical decomposition", {
  tsbl_co2 <- as_tsibble(co2)
  dcmp <- tsbl_co2 %>% classical_decomposition(value)
  stats_dcmp <- stats::decompose(co2)

  expect_equivalent(
    dcmp$trend,
    unclass(stats_dcmp$trend)
  )
  expect_equivalent(
    dcmp$seasonal,
    unclass(stats_dcmp$seasonal)
  )
  expect_equivalent(
    dcmp$random,
    unclass(stats_dcmp$random)
  )
  expect_equal(
    dcmp$value - dcmp$seasonal,
    dcmp$season_adjust
  )
})

test_that("Multiplicative classical decomposition", {
  tsbl_uad <- as_tsibble(USAccDeaths)
  dcmp <- tsbl_uad %>% classical_decomposition(value, type = "multiplicative")
  stats_dcmp <- stats::decompose(USAccDeaths, type = "multiplicative")

  expect_equivalent(
    dcmp$trend,
    unclass(stats_dcmp$trend)
  )
  expect_equivalent(
    dcmp$seasonal,
    unclass(stats_dcmp$seasonal)
  )
  expect_equivalent(
    dcmp$random,
    unclass(stats_dcmp$random)
  )
  expect_equal(
    dcmp$value / dcmp$seasonal,
    dcmp$season_adjust
  )
})
