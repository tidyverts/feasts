context("test-stl")

test_that("Seasonal STL", {
  tsbl_uad <- as_tsibble(USAccDeaths)
  dcmp <- tsbl_uad %>% STL(value)
  stats_dcmp <- stats::stl(USAccDeaths, s.window = 13)

  expect_equivalent(
    dcmp$trend,
    unclass(stats_dcmp$time.series[,"trend"])
  )
  expect_equivalent(
    dcmp$season_year,
    unclass(stats_dcmp$time.series[,"seasonal"])
  )
  expect_equivalent(
    dcmp$remainder,
    unclass(stats_dcmp$time.series[,"remainder"])
  )
  expect_equal(
    dcmp$value - dcmp$season_year,
    dcmp$seas_adjust
  )
})


test_that("Non-seasonal STL", {
  tsbl_www <- as_tsibble(WWWusage)
  dcmp <- tsbl_www %>% STL(value)
  stats_dcmp <- stats::supsmu(seq_along(WWWusage), WWWusage)

  expect_equivalent(
    dcmp$trend,
    stats_dcmp$y
  )
  expect_equivalent(
    dcmp$remainder,
    unclass(WWWusage - stats_dcmp$y)
  )
  expect_equal(
    dcmp$value,
    dcmp$seas_adjust
  )
})


test_that("Multiple seasonality STL", {
  dt <- tsibble(idx = seq_len(100),
                y = rep(1:4, length.out = 100) + rep(1:7, length.out = 100),
                index = idx)
  dcmp <- dt %>% STL(y ~ season(4) + season(7))

  expect_equal(
    dcmp$trend,
    rep(6.5, 100),
    tolerance = 0.01
  )
  expect_equal(
    dcmp$remainder,
    rep(0, 100),
    tolerance = 0.01
  )
  expect_equal(
    dcmp$season_4,
    rep(1:4, length.out = 100) - 2.5,
    tolerance = 0.01
  )
  expect_equal(
    dcmp$season_7,
    rep(1:7, length.out = 100) - 4,
    tolerance = 0.01
  )
  expect_equal(
    dcmp$y - dcmp$season_4 - dcmp$season_7,
    dcmp$seas_adjust
  ) %>%
    expect_equal(
      dcmp$trend + dcmp$remainder
    )
})
