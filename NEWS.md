# feasts 0.2.2

Minor release for compatibility with an upcoming ggplot2 release. This release
contains a few bug fixes and improvements to existing functionality.

## Improvements

* The `gg_tsresiduals()` function now allows the type of plotted residual to be
  controlled via the `type` argument.
* Improved the default seasonal window for `STL()` decompositions. For data with
  a single seasonal pattern, the window has changed from 13 to 11. This change
  is based on results from simulation experiments.
* Documentation improvements.

## Bug fixes

* Fixed issue where `seasonal::seas()` defaults were not being used in
  `X_13ARIMA_SEATS()` when `defaults = "seasonal"` (#130).
* Fixed issue with `gg_subseries()` on data with spaces in the index column 
  name (#136).

## Breaking changes

* Replaced usage of `...` in `ACF()`, `PACF()`, and `CCF()` with `y` (and `x` 
  for `CCF()`) arguments. This change should not affect the code for most users,
  but is important for the eventual passing of `...` to `acf()`, `pacf()` and
  `ccf()` in a future version (#124).

# feasts 0.2.1

Small patch to fix check issues on Solaris, and to resolve `components()` for
automatically selected transformations in `X_13ARIMA_SEATS()`.

# feasts 0.2.0

## New features

* Added `X_13ARIMA_SEATS()` decomposition method. This is a complete wrapper of
  the X-13ARIMA-SEATS developed by the U.S. Census Bureau, implemented via
  the `seasonal::seas()` function. The defaults match what is used in the 
  seasonal pacakge, however these defaults can be removed (giving an empty 
  default model) by setting `defaults="none"`. 
  
## Breaking changes
* The new `X_13ARIMA_SEATS()` method officially deprecates (supersedes) the
  `X11()` and `SEATS()` models which were previously not exported (#66).

## Improvements

* Documentation improvements.

# feasts 0.1.7

## New features

* Added `generate()` method for `STL()` decompositions. The method uses a block 
bootstrap method to sample from the residuals.
* Added `fitted()` and `residuals()` methods for `STL()` decompositions.

## Improvements

* Changed `guerrero()` default lower bound for Box-Cox lambda selection to from
  -1 to -0.9. A transformation parameter of -1 typically results from data which
  should not be transformed with a Box-Cox transformation, and can result in
  very inaccurate forecasts if such a strong and inappropriate transformation is
  used.
* Improved time series plotting functions axis labelling.  
* Documentation improvements.
  
# feasts 0.1.6

A minor release to fix check issues introduced by changes in an upstream 
dependency.

## Improvements

* `gg_season()` labels are low aligned outward (#115).

## Bug fixes

* Fixed issue with plotting aggregated tsibbles with `gg_season()` and
  `gg_subseries()` (#117).
* Fixed occasional issue with double label/breaks displayed in `gg_season()`


# feasts 0.1.5

## Improvements

* `gg_lag()` facets are now displayed with a 1:1 aspect ratio.
* Season and subseries plots of numeric index data now starts at the earliest 
  measured observation, rather than assuming a meaningful 0 (#111).
* The `n_flat_spots()` function has been renamed to `longest_flat_spot()` to 
  more accurately describe the feature.
* `gg_season()` and `ggsubseries()` date structure improvements.
* Documentation improvements

## Breaking changes

* The `n_flat_spots()` return name is now "longest_flat_spot" to better describe
  the feature.

## Bug fixes

* Fixed spectral density plot in `gg_tsdisplay()` erroring when the `spec.ar`
  order is chosen to be 0.
* Fixed `CCF()` lag being spaced by multiples of the data's frequency.
* Fixed labelling of x-axis for `gg_season()` and `gg_subseries()` (#107).
* Fixed `View()` not working on `ACF()`, `PACF()` and `CCF()` outputs.

# feasts 0.1.4

Minor patch to resolve upstream check issues introduced by dplyr v1.0.0 and 
tsibble v0.9.0.

## New features

* Circular time plots are now supported by setting `polar = TRUE` in 
  `gg_season()`.

## Improvements

* Added partial matching of the type argument in `ACF()`.
* Updated `feat_spectral()` to use `stats::spec.ar()` instead of 
  `ForeCA::spectral_entropy()`. Note that the feature value will be slightly
  different due to use of a different spectral estimator, and the fix of a
  bug in ForeCA.

## Bug fixes

* Fixed the minimum data length for seasonal estimation in `feat_stl()`.

# feasts 0.1.3

## Improvements

* The axis for `gg_lag()` have been reversed for consistency with `stats::lag.plot()`.
* Graphical improvements for displaying weekly seasonality in season and subseries plots.
* Added intermittency features available in `feat_intermittent()`

## Bug fixes

* Fixed the sprectral density plot in `gg_tsdisplay()` not working with plotting expressions of data.
* Fixed issue with `gg_subseries()` erroring when certain column names are used (#95).
* Fixed issue with environment handling in `STL()` specials.

# feasts 0.1.2

## Improvements

* `var_tiled_var()` no longer includes partial tile windows in the computation.
* Added residual acf features to `feat_stl()`.
* Performance improvements.

## Breaking changes

* Decompositions are now treated as models. 
  To access the decomposed values, you will now have to use `components()`.
  For example, `tourism %>% STL(Trips)` is now `tourism %>% model(STL(Trips)) %>% components()`.
  This change allows for more flexible decomposition specifications, and better interfaces for decomposition modelling.

## Bug fixes

* Fixed bug with `feat_spectral()` not showing results.
* Fix warning in `ACF()`, `PACF()` and `CCF()` for tidyr change.
* `gg_tsdisplay()` will no longer fail on non-seasonal data with missing values. The last plot will instead show a PACF in this case (#76)
* Better handling of perfect fits in `stat_arch_lm()` (#85)

# feasts 0.1.1

## Improvements

* Better naming of seasonal columns in STL decomposition when seasonal period is specified.

## Bug fixes

* Fixes issues with running tests on unsupported systems.

# feasts 0.1.0

* First release.

## New features

* Added support for graphical analysis of tidy temporal data and models, with `gg_season`, `gg_subseries`, `gg_lag`, `gg_tsdisplay`, `gg_tsresiduals`, `gg_arma`.
* Added support for autocorrelation functions and plots, with `ACF`, `PACF`, `CCF`, and `autoplot.tbl_cf`
* Added a collection of features to be used with `fabletools::features()`: `feat_stl`, `feat_acf`, `feat_pacf`, `guerrero`, `unitroot_kpss`, `unitroot_pp`, `unitroot_ndiffs`, `unitroot_nsdiffs`, `box_pierce`, `ljung_box`, `var_tiled_var`, `var_tiled_mean`, `shift_level_max`, `shift_var_max`, `shift_kl_max`, `feat_spectral`, `n_crossing_points`, `n_flat_spots`, `coef_hurst`, `stat_arch_lm`
* Added support for two decomposition methods: `classical_decomposition`, `STL`
