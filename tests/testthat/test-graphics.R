context("test-graphics")

tsbl_co2 <- as_tsibble(co2)
tsbl_ped <- tsibble::pedestrian %>%
  filter(Sensor == "Southern Cross Station",
         yearmonth(Date) == yearmonth("2015 Jan"))

test_that("gg_season() plots", {
  p <- gg_season(tsbl_co2, value)

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_co2$value
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(12, 39)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )

  p <- gg_season(tsbl_ped, Count, "day")

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_ped$Count
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(24, 31)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "Date_Time", y = "Count")
  )
})


test_that("gg_season() plot labels", {
  p <- gg_season(tsbl_co2, value, labels = "both")

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_co2$value
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(12, 39)
  )
  expect_equal(
    ggplot2::layer_data(p,2)$label,
    ordered(rep(1959:1997, each = 2))
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
})

test_that("gg_season() facets", {
  p <- gg_season(tsbl_co2, value, facet_period = "10 year", labels = "both")

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_co2$value
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(12, 39)
  )
  # expect_equal( Issue #49
  #   ggplot2::layer_data(p,2)$label,
  #   ordered(rep(1959:1997, each = 2))
  # )

  p_built <- ggplot2::ggplot_build(p)

  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
})

test_that("gg_subseries() plots", {
  p <- gg_subseries(tsbl_co2, value)

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_co2$value[order((seq_along(tsbl_co2$value) - 1)%%12)]
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$PANEL)),
    rep(39, 12)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )

  p <- gg_subseries(tsbl_ped, Count, "day")

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_ped$Count[order((seq_along(tsbl_ped$Count) - 1)%%24)]
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$PANEL)),
    rep(31, 24)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "Date_Time", y = "Count")
  )
})

