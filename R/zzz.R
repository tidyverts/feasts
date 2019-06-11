# nocov start
.onLoad <- function(...) {
  register_s3_method("pillar", "pillar_shaft", "cf_lag")
  register_s3_method("pillar", "obj_sum", "cf_lag")
  register_s3_method("pillar", "type_sum", "cf_lag")

  fablelite::register_feature(features_stl, c("trend", "seasonal", "decomposition"))
  fablelite::register_feature(features_acf, "autocorrelation")
  fablelite::register_feature(features_pacf, "autocorrelation")
  fablelite::register_feature(guerrero, c("optimisation", "boxcox"))
  fablelite::register_feature(unitroot_kpss, c("test", "unitroot"))
  fablelite::register_feature(unitroot_pp, c("test", "unitroot"))
  fablelite::register_feature(unitroot_ndiffs, c("test", "unitroot"))
  fablelite::register_feature(unitroot_nsdiffs, c("test", "seasonal", "unitroot"))
  fablelite::register_feature(box_pierce, c("test", "portmanteau"))
  fablelite::register_feature(ljung_box, c("test", "portmanteau"))
  fablelite::register_feature(roll_lumpiness, c("roll", "tile"))
  fablelite::register_feature(roll_stability, c("roll", "tile"))
  fablelite::register_feature(shift_level_max, c("roll", "slide"))
  fablelite::register_feature(shift_var_max, c("roll", "slide"))
  fablelite::register_feature(shift_kl_max, c("roll", "slide"))
  fablelite::register_feature(features_spectral, c("spectral"))
  fablelite::register_feature(crossing_points, NULL)
  fablelite::register_feature(flat_spots, c("rle"))
  fablelite::register_feature(coef_hurst, c("coefficients"))
  fablelite::register_feature(stat_arch_lm, c("test"))
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
