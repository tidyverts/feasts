context("test-features")

www_usage <- as_tsibble(WWWusage)
lung_deaths_long <- as_tsibble(cbind(mdeaths, fdeaths))
lung_deaths_wide <- as_tsibble(cbind(mdeaths, fdeaths), pivot_longer = FALSE)

test_that("features()", {
  expect_error(
    features(lung_deaths_wide, vars(mdeaths, fdeaths), acf_features),
    "only supports a single variable"
  )

  expect_message(
    features(lung_deaths_wide, features = list(guerrero, acf_features)),
    "Feature variable not specified, automatically selected \\`.var = mdeaths\\`"
  ) %>%
    colnames() %>%
    expect_equal(c("lambda_guerrero", "x_acf1", "x_acf10", "diff1_acf1",
                   "diff1_acf10", "diff2_acf1", "diff2_acf10", "seas_acf1"))
})

test_that("Scoped variants of features()", {
  ft_at <- features_at(lung_deaths_wide, vars(mdeaths:fdeaths), acf_features)
  expect_equal(
    substr(colnames(ft_at), 1, 7),
    c(rep("mdeaths", 7), rep("fdeaths", 7))
  )
  ft_if <- features_if(lung_deaths_wide, is.numeric, acf_features)
  expect_identical(
    ft_at, ft_if
  )
  ft_all <- features_all(lung_deaths_wide, acf_features)
  expect_identical(
    ft_if, ft_all
  )
})

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
  expect_equal(ft$kpss_pval < 0.05, as_logical(ft$ndiffs))
  expect_equal(ft$pp_pval, 0.1)

  ft <- features(lung_deaths_long, value, list(stl_features, unitroot_nsdiffs))
  expect_equal(ft$seasonal_strength.year >= 0.64, as_logical(ft$nsdiffs))
})

test_that("basic features", {
  basic_features <- list(crossing_points, flat_spots, entropy, lumpiness, stability)
  ft <- features(www_usage, value, basic_features)
  expect_equivalent(
    as_list(ft),
    list(crossing_points = 7L, flat_spots = 13L, entropy = 0.561, lumpiness = 0.0139, stability = 0.988),
    tolerance = 0.01
  )
})


test_that("*cf features", {
  cf_features <- list(acf_features, pacf_features)
  ft <- features(www_usage, value, cf_features)
  expect_equivalent(
    as_list(ft),
    list(x_acf1 = 0.960, x_acf10 = 4.19,
         diff1_acf1 = 0.792, diff1_acf10 = 1.41,
         diff2_acf1 = 0.174, diff2_acf10 = 0.334,
         x_pacf5 = 1.04, diff1x_pacf5 = 0.802, diff2x_pacf5 = 0.222),
    tolerance = 0.01
  )
})


test_that("*shift features", {
  shift_features <- list(max_level_shift, max_var_shift, max_kl_shift)
  ft <- features(www_usage, value, shift_features)
  expect_equivalent(
    as_list(ft),
    list(level_shift_max = 71.7, level_shift_index = 84,
         var_shift_max = 749, var_shift_index = 54,
         kl_shift_max = 1.44, kl_shift_index = 57),
    tolerance = 0.01
  )
})

test_that("model based features", {
  model_features <- list(arch_stat, hurst, stl_features)
  ft <- features(www_usage, value, model_features)
  expect_equivalent(
    as_list(ft),
    list(arch_lm = 0.990, hurst = 0.998,
         trend_strength = 0.985, spike = 0.0772,
         linearity = 178, cuvature = 44),
    tolerance = 0.01
  )

  ft <- features(lung_deaths_wide, fdeaths, stl_features)
  expect_equivalent(
    as_list(ft),
    list(trend_strength = 0.118, seasonal_strength.year = 0.881,
         spike = 24526, linearity = -148, cuvature = 11.6,
         seasonal_peak.year = 1, seasonal_trough.year = 8),
    tolerance = 0.01
  )
})

