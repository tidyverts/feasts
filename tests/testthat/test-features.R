context("test-features")

www_usage <- as_tsibble(WWWusage)
lung_deaths_long <- as_tsibble(cbind(mdeaths, fdeaths))
lung_deaths_wide <- as_tsibble(cbind(mdeaths, fdeaths), pivot_longer = FALSE)

test_that("guerrero()", {
  ft <- features(www_usage, value, guerrero)
  expect_equal(dim(ft), c(1,1))
  expect_equal(ft$lambda_guerrero, 0.360, tolerance = 0.001)

  ft <- features(lung_deaths_long, value, guerrero)
  expect_equal(dim(ft), c(2,2))
  expect_equal(ft$lambda_guerrero, c(-1, 0.321), tolerance = 0.001)
})

test_that("unit root features", {
  ft <- features(www_usage, value, list(unitroot_kpss, unitroot_pp, unitroot_ndiffs))
  expect_equal(ft$kpss_pvalue < 0.05, as.logical(ft$ndiffs))
  expect_equal(ft$pp_pvalue, 0.1)

  ft <- features(lung_deaths_long, value, list(feat_stl, unitroot_nsdiffs))
  expect_equal(ft$seasonal_strength_year >= 0.64, as.logical(ft$nsdiffs))
})

test_that("basic features", {
  basic_features <- list(n_crossing_points, n_flat_spots, feat_spectral,
                         var_tiled_var, var_tiled_mean)
  ft <- features(www_usage, value, basic_features)
  expect_equivalent(
    as.list(ft),
    list(n_crossing_points = 7L, n_flat_spots = 13L, entropy = 0.461, var_tiled_var = 0.0139, var_tiled_mean = 0.988),
    tolerance = 0.01
  )
})


test_that("*cf features", {
  cf_features <- list(feat_acf, feat_pacf)
  ft <- features(www_usage, value, cf_features)
  expect_equivalent(
    as.list(ft),
    list(acf1 = 0.960, acf10 = 4.19,
         diff1_acf1 = 0.792, diff1_acf10 = 1.41,
         diff2_acf1 = 0.174, diff2_acf10 = 0.334,
         pacf5 = 1.04, diff1x_pacf5 = 0.802, diff2x_pacf5 = 0.222),
    tolerance = 0.01
  )
})


test_that("*shift features", {
  shift_features <- list(shift_level_max, shift_var_max, shift_kl_max)
  ft <- features(www_usage, value, shift_features)
  expect_equivalent(
    as.list(ft),
    list(shift_level_max = 71.7, shift_level_index = 84,
         shift_var_max = 749, shift_var_index = 54,
         shift_kl_max = 1.44, shift_kl_index = 57),
    tolerance = 0.01
  )
})

test_that("model based features", {
  model_features <- list(stat_arch_lm, coef_hurst, feat_stl)
  ft <- features(www_usage, value, model_features)
  expect_equivalent(
    as.list(ft),
    list(
      stat_arch_lm = 0.990, coef_hurst = 0.998,
      trend_strength = 0.985, spikiness = 0.0772,
      linearity = 178, curvature = 44,
      stl_e_acf1 = 0.774, stl_e_acf10 = 0.983),
    tolerance = 0.01
  )

  ft <- features(lung_deaths_wide, fdeaths, feat_stl)
  expect_equivalent(
    as.list(ft),
    list(trend_strength = 0.118, seasonal_strength_year = 0.881,
         seasonal_peak_year = 1, seasonal_trough_year = 8,
         spikiness = 24526, linearity = -148, cuvature = 11.6,
         stl_e_acf1 = 0.0256, stl_e_acf10 = 0.186),
    tolerance = 0.01
  )
})
