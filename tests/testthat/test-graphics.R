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

  expect_equivalent(
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

  expect_equivalent(
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
    c(ggplot2::layer_data(p,2)$label, ggplot2::layer_data(p,3)$label),
    ordered(rep(1959:1997, 2))
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equivalent(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
})

test_that("gg_season() facets", {
  p <- gg_season(tsbl_co2, value, facet_period = "10 year", labels = "both")

  # expect_equal(
  #   ggplot2::layer_data(p)$y,
  #   tsbl_co2$value
  # )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(12, 39)
  )
  expect_equal(
    c(ggplot2::layer_data(p,2)$label, ggplot2::layer_data(p,3)$label),
    ordered(rep(1959:1997, 2))
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equivalent(
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

  expect_equivalent(
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

  expect_equivalent(
    p_built$plot$labels[c("x", "y")],
    list(x = "Date_Time", y = "Count")
  )
})

test_that("gg_lag() plots", {
  p <- gg_lag(tsbl_co2, value)

  expect_equal(
    ggplot2::layer_data(p, 2)$x,
    do.call(c, map(seq_len(9), function(i)
      tsbl_co2$value[seq_len(length(tsbl_co2$value) - i)]))
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p, 2)$PANEL)),
    length(tsbl_co2$value) - seq_len(9)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equivalent(
    p_built$plot$labels[c("x", "y")],
    list(x = "lag(value, n)", y = "value")
  )

  p <- gg_lag(tsbl_co2, value, lags = c(1, 4, 9))

  expect_equal(
    ggplot2::layer_data(p, 2)$x,
    do.call(c, map(c(1, 4, 9),
                   function(i) tsbl_co2$value[seq_len(length(tsbl_co2$value) - i)]))
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p, 2)$PANEL)),
    length(tsbl_co2$value) - c(1, 4, 9)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equivalent(
    p_built$plot$labels[c("x", "y")],
    list(x = "lag(value, n)", y = "value")
  )
})

test_that("gg_tsdisplay() plots", {
  p <- gg_tsdisplay(tsbl_co2, value)

  expect_s3_class(
    p, "gg_tsensemble"
  )

  expect_equal(
    ggplot2::layer_data(p[[1]], 1)$y,
    tsbl_co2$value
  )

  p_built <- ggplot2::ggplot_build(p[[1]])

  expect_equivalent(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )

  p <- p + ggplot2::labs(x = "x", y = "y", title = "title")

  p_built <- ggplot2::ggplot_build(p[[1]])

  expect_equivalent(
    p_built$plot$labels[c("x", "y", "title")],
    list(x = "x", y = "y", title = "title")
  )

  p_built <- ggplot2::ggplot_build(p[[2]])

  expect_equivalent(
    p_built$plot$labels[c("x", "y")],
    list(x = "lag [1M]", y = "acf")
  )

  p_built <- ggplot2::ggplot_build(p[[3]])

  expect_equivalent(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )

  p <- gg_tsdisplay(tsbl_co2, value, plot_type = "histogram")

  p_built <- ggplot2::ggplot_build(p[[3]])

  expect_equivalent(
    p_built$plot$labels[c("x", "y")],
    list(x = "value", y = "count")
  )

  p <- gg_tsdisplay(tsbl_co2, value, plot_type = "scatter")

  expect_equal(
    ggplot2::layer_data(p[[3]], 1)$y,
    tsbl_co2$value[-1]
  )

  p_built <- ggplot2::ggplot_build(p[[3]])

  expect_equivalent(
    p_built$plot$labels[c("x", "y")],
    list(x = expression(Y[t - 1]), y = expression(Y[t]))
  )

  p <- gg_tsdisplay(tsbl_co2, value, plot_type = "spectrum")

  p_built <- ggplot2::ggplot_build(p[[3]])

  expect_equivalent(
    p_built$plot$labels[c("x", "y")],
    list(x = "frequency", y = "spectrum")
  )
})

test_that("gg_arma() plots", {
  skip_if_not_installed("fable")
  mdl <- tsbl_co2 %>%
    fabletools::model(fable::ARIMA(value ~ 0 + pdq(1,1,1) + PDQ(1,1,2)))

  p <- gg_arma(mdl)
  smmry <- fabletools::glance(mdl)
  ar_roots <- smmry$ar_roots[[1]]
  ma_roots <- smmry$ma_roots[[1]]

  expect_equal(
    ggplot2::layer_data(p, 4)$y,
    c(Im(1/ar_roots), Im(1/ma_roots))
  )
  expect_equal(
    ggplot2::layer_data(p, 4)$x,
    c(Re(1/ar_roots), Re(1/ma_roots))
  )
  expect_equal(
    ggplot2::layer_data(p, 4)$PANEL,
    factor(c(rep_along(ar_roots, 1), rep_along(ma_roots, 2)))
  )
})
