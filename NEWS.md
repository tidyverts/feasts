# feasts 0.1.1

## Improvements

* Better naming of seasonal columns in STL decomposition when seasonal period is specified.
* Fixes issues with running tests on unsupported systems.

# feasts 0.1.0

* First release.

## New features

* Added support for graphical analysis of tidy temporal data and models, with `gg_season`, `gg_subseries`, `gg_lag`, `gg_tsdisplay`, `gg_tsresiduals`, `gg_arma`.
* Added support for autocorrelation functions and plots, with `ACF`, `PACF`, `CCF`, and `autoplot.tbl_cf`
* Added a collection of features to be used with `fabletools::features()`: `feat_stl`, `feat_acf`, `feat_pacf`, `guerrero`, `unitroot_kpss`, `unitroot_pp`, `unitroot_ndiffs`, `unitroot_nsdiffs`, `box_pierce`, `ljung_box`, `var_tiled_var`, `var_tiled_mean`, `shift_level_max`, `shift_var_max`, `shift_kl_max`, `feat_spectral`, `n_crossing_points`, `n_flat_spots`, `coef_hurst`, `stat_arch_lm`
* Added support for two decomposition methods: `classical_decomposition`, `STL`
