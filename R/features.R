#' Extract features from a dataset
#'
#' @param .data A dataset (tsibble)
#' @param ... Additional arguments to be passed to measures that use it.
#'
#' @export
features <- function(.data, ...){
  UseMethod("features")
}

combine_features <- function(...){
  dots <- dots_list(...)
  res <- map2(dots, names(dots), function(val, nm){
    if(nchar(nm) > 0){
      nm <- paste0(nm, "_")
    }
    set_names(as.list(val), paste0(nm,names(val)))
  })
  as_tibble(flatten(unname(res)))
}

build_feature_calls <- function(measures, available_args){
  # Build measure calls
  missing_args <- chr()
  fns <- map(measures, function(fn){
    args <- formals(fn)
    args <- args[names(args) != "..."]
    req_args <- names(args)[map_lgl(args, is_missing)]
    if(!all(match_req <- req_args %in% available_args)){
      missing_args <<- c(missing_args, req_args[!match_req])
      return(NULL)
    }

    # Function call
    inputs <- available_args[available_args%in%names(args)]
    call2(fn, !!!set_names(syms(inputs), inputs))
  })

  if(!is_empty(missing_args)){
    warn(
      sprintf("Could not estimate all measures as the following arguments are missing: %s",
              paste0(missing_args, collapse = ", "))
    )
  }

  # names(fns) <- names(fns) %||% seq_along(fns)
  fns
}

#' @export
features.tbl_ts <- function(.data, y = NULL, ..., features = list(guerrero)){
  dots <- dots_list(...)

  if(is_function(features)){
    features <- list(features)
  }

  if(quo_is_null(enquo(y))){
    inform(sprintf(
      "Feature variable not specified, automatically selected `y = %s`",
      measured_vars(.data)[1]
    ))
    y <- sym(measured_vars(.data)[1])
  }
  else{
    y <- enexpr(y)
  }

  features <- squash(features)

  if(is.null(dots$.period)){
    dots$.period <- get_frequencies(NULL, .data, .auto = "smallest")
  }

  .data <- transmute(.data, x = !!y)

  fns <- build_feature_calls(features, c(names(dots), "x"))

  with(dots,
       .data %>%
         as_tibble %>%
         group_by(!!!key(.data), !!!groups(.data)) %>%
         summarise(
           .features = list(combine_features(!!!compact(fns)))
         ) %>%
         unnest(.features) %>%
         ungroup()
  )
}

#' @inherit tsfeatures::crossing_points
#' @importFrom stats median
#' @export
crossing_points <- function(x)
{
  midline <- median(x, na.rm = TRUE)
  ab <- x <= midline
  lenx <- length(x)
  p1 <- ab[1:(lenx - 1)]
  p2 <- ab[2:lenx]
  cross <- (p1 & !p2) | (p2 & !p1)
  c(crossing_points = sum(cross, na.rm = TRUE))
}

#' @inherit tsfeatures::arch_stat
#' @importFrom stats lm embed
#' @export
arch_stat <- function(x, lags = 12, demean = TRUE)
{
  if (length(x) <= 13) {
    return(c(arch_lm = NA_real_))
  }
  if (demean) {
    x <- x - mean(x, na.rm = TRUE)
  }
  mat <- embed(x^2, lags + 1)
  fit <- try(lm(mat[, 1] ~ mat[, -1]), silent = TRUE)
  if ("try-error" %in% class(fit)) {
    return(c(arch_lm = NA_real_))
  }
  arch.lm <- summary(fit)
  c(arch_lm = arch.lm$r.squared)
}

stl_features <- function(x, .period, s.window = 13, ...){
  dots <- dots_list(...)
  dots <- dots[names(dots) %in% names(formals(stats::stl))]
  season.args <- list2(!!(names(.period)%||%.period) :=
                         list(period = .period, s.window = s.window))
  dcmp <- eval_tidy(quo(estimate_stl(x, trend.args = list(),
                    season.args = season.args, lowpass.args = list(), !!!dots)))
  trend <- dcmp[["trend"]]
  remainder <- dcmp[["remainder"]]
  seas_adjust <- dcmp[["seas_adjust"]]
  seasonalities <- dcmp[seq_len(length(dcmp) - 3) + 1]
  names(seasonalities) <- sub("season_", "", names(seasonalities))

  var_e <- var(remainder, na.rm = TRUE)
  n <- length(x)

  # Spike
  d <- (remainder - mean(remainder, na.rm = TRUE))^2
  var_loo <- (var_e * (n - 1) - d)/(n - 2)
  spike <- var(var_loo, na.rm = TRUE)

  # Linearity & curvature
  tren.coef <- coef(lm(trend ~ poly(seq(n), degree = 2L)))[2L:3L]
  linearity <- tren.coef[[1L]]
  curvature <- tren.coef[[2L]]

  # Strength of terms
  trend_strength <- max(0, min(1, 1 - var_e/var(seas_adjust, na.rm = TRUE)))
  seasonal_strength <- map_dbl(seasonalities, function(seas){
    max(0, min(1, 1 - var_e/var(remainder + seas, na.rm = TRUE)))
  })

  # Position of peaks and troughs
  seasonal_peak <- map_dbl(seasonalities, function(seas){
    which.max(seas) %% .period
  })
  seasonal_trough <- map_dbl(seasonalities, function(seas){
    which.min(seas) %% .period
  })

  c(trend_strength = trend_strength, seasonal_strength = seasonal_strength,
    spike = spike, linearity = linearity, curvature = curvature,
    seasonal_peak = seasonal_peak, seasonal_trough = seasonal_trough)
}

#' Unit root tests
#'
#' Performs a test for the existence of a unit root in the vector.
#'
#' \code{unitroot_kpss} computes the statistic for the Kwiatkowski et al. unit root test with linear trend and lag 1.
#' \code{unitroot_pp} computes the statistic for the `'Z-tau'' version of Phillips & Perron unit root test with constant trend and lag 1.
#'
#' @param x A vector to be tested for the unit root.
#'
#' @rdname unitroot
#' @export
unitroot_kpss <- function(x) {
  require_package("urca")
  result <- urca::ur.kpss(x)
  pval <- tryCatch(
    approx(result@cval[1,], as.numeric(sub("pct", "", colnames(result@cval)))/100, xout=result@teststat[1], rule=2)$y,
    error = function(e){
      NA
    }
  )
  c(kpss_stat = result@teststat, kpss_pval = pval)
}

#' @rdname unitroot
#' @export
unitroot_pp <- function(x) {
  require_package("urca")
  result <- urca::ur.pp(x, type = "Z-tau")
  pval <- tryCatch(
    approx(result@cval[1,], as.numeric(sub("pct", "", colnames(result@cval)))/100, xout=result@teststat[1], rule=2)$y,
    error = function(e){
      NA
    }
  )
  c(pp_stat = result@teststat, pp_pval = pval)
}
