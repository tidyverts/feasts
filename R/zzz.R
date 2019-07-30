# nocov start
.onLoad <- function(...) {
  register_s3_method("pillar", "pillar_shaft", "cf_lag")
  register_s3_method("pillar", "obj_sum", "cf_lag")
  register_s3_method("pillar", "type_sum", "cf_lag")

  fabletools::register_feature(feat_stl, c("stl", "trend", "seasonal", "decomposition"))
  fabletools::register_feature(feat_acf, c("acf", "autocorrelation"))
  fabletools::register_feature(feat_pacf, c("pacf", "autocorrelation"))
  fabletools::register_feature(guerrero, c("optimisation", "boxcox"))
  fabletools::register_feature(unitroot_kpss, c("test", "unitroot"))
  fabletools::register_feature(unitroot_pp, c("test", "unitroot"))
  fabletools::register_feature(unitroot_ndiffs, c("test", "unitroot"))
  fabletools::register_feature(unitroot_nsdiffs, c("test", "seasonal", "unitroot"))
  fabletools::register_feature(box_pierce, c("test", "portmanteau"))
  fabletools::register_feature(ljung_box, c("test", "portmanteau"))
  fabletools::register_feature(var_tiled_var, c("lumpiness", "tile"))
  fabletools::register_feature(var_tiled_mean, c("stability", "tile"))
  fabletools::register_feature(shift_level_max, c("roll", "slide"))
  fabletools::register_feature(shift_var_max, c("roll", "slide"))
  fabletools::register_feature(shift_kl_max, c("roll", "slide"))
  fabletools::register_feature(feat_spectral, c("spectral"))
  fabletools::register_feature(n_crossing_points, "count")
  fabletools::register_feature(n_flat_spots, c("count", "rle"))
  fabletools::register_feature(coef_hurst, c("coefficients"))
  fabletools::register_feature(stat_arch_lm, c("test"))
  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
