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
