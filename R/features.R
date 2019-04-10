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

#' Number of flat spots
#'
#' Number of flat spots in a time series
#' @param x a univariate time series
#' @return A numeric value.
#' @author Earo Wang and Rob J Hyndman
#' @export
flat_spots <- function(x) {
  cutx <- try(cut(x, breaks = 10, include.lowest = TRUE, labels = FALSE),
              silent = TRUE
  )
  if (class(cutx) == "try-error") {
    return(c(flat_spots = NA))
  }
  rlex <- rle(cutx)
  return(c(flat_spots = max(rlex$lengths)))
}

#' Hurst coefficient
#'
#' Computes the Hurst coefficient indicating the level of fractional differencing
#' of a time series.
#' @param x a univariate time series. If missing values are present, the largest
#' contiguous portion of the time series is used.
#' @return A numeric value.
#' @author Rob J Hyndman
#' @export
hurst <- function(x) {
  require_package("fracdiff")
  # Hurst=d+0.5 where d is fractional difference.
  return(c(hurst = suppressWarnings(fracdiff::fracdiff(na.contiguous(x), 0, 0)[["d"]] + 0.5)))
}


#' Sliding window features
#'
#' Computes feature of a time series based on sliding (overlapping) windows.
#' \code{max_level_shift} finds the largest mean shift between two consecutive windows.
#' \code{max_var_shift} finds the largest var shift between two consecutive windows.
#' \code{max_kl_shift} finds the largest shift in Kulback-Leibler divergence between
#' two consecutive windows.
#'
#' Computes the largest level shift and largest variance shift in sliding mean calculations
#' @param x a univariate time series
#' @param .size size of sliding window, if NULL `.size` will be automatically chosen using `.period`
#' @param .period The seasonal period (optional)
#' @return A vector of 2 values: the size of the shift, and the time index of the shift.
#'
#' @author Earo Wang, Rob J Hyndman and Mitchell O'Hara-Wild
#'
#' @export
max_level_shift <- function(x, .size = NULL, .period = 1) {
  if(is.null(.size)){
    .size <- ifelse(.period == 1, 10, .period)
  }

  rollmean <- tsibble::slide_dbl(x, mean, .size = .size, na.rm = TRUE)

  means <- abs(diff(rollmean, .size))
  if (length(means) == 0L) {
    maxmeans <- 0
    maxidx <- NA_real_
  }
  else if (all(is.na(means))) {
    maxmeans <- NA_real_
    maxidx <- NA_real_
  }
  else {
    maxmeans <- max(means, na.rm = TRUE)
    maxidx <- which.max(means) + 1L
  }

  return(c(level_shift_max = maxmeans, level_shift_index = maxidx))
}

#' @rdname max_level_shift
#' @export
max_var_shift <- function(x, .size = NULL, .period = 1) {
  if(is.null(.size)){
    .size <- ifelse(.period == 1, 10, .period)
  }

  rollvar <- tsibble::slide_dbl(x, var, .size = .size, na.rm = TRUE)

  vars <- abs(diff(rollvar, .size))

  if (length(vars) == 0L) {
    maxvar <- 0
    maxidx <- NA_real_
  }
  else if (all(is.na(vars))) {
    maxvar <- NA_real_
    maxidx <- NA_real_
  }
  else {
    maxvar <- max(vars, na.rm = TRUE)
    maxidx <- which.max(vars) + 1L
  }

  return(c(var_shift_max = maxvar, var_shift_index = maxidx))
}

#' @rdname max_level_shift
#' @export
max_kl_shift <- function(x, .size = NULL, .period = 1) {
  if(is.null(.size)){
    .size <- ifelse(.period == 1, 10, .period)
  }

  gw <- 100 # grid width
  xgrid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = gw)
  grid <- xgrid[2L] - xgrid[1L]
  tmpx <- x[!is.na(x)] # Remove NA to calculate bw
  bw <- stats::bw.nrd0(tmpx)
  lenx <- length(x)
  if (lenx <= (2 * .size)) {
    return(c(max_kl_shift = NA_real_, time_kl_shift = NA_real_))
  }

  densities <- map(xgrid, function(xgrid) stats::dnorm(xgrid, mean = x, sd = bw))
  densities <- map(densities, pmax, dnorm(38))

  rmean <- map(densities, function(x)
    tsibble::slide_dbl(x, mean, .size = .size, na.rm = TRUE, .align = "right")
  ) %>%
    transpose() %>%
    map(unlist)

  kl <- map2_dbl(
    rmean[seq_len(lenx - .size)],
    rmean[seq_len(lenx - .size) + .size],
    function(x, y) sum(x * (log(x) - log(y)) * grid, na.rm = TRUE)
  )

  diffkl <- diff(kl, na.rm = TRUE)
  if (length(diffkl) == 0L) {
    diffkl <- 0
    maxidx <- NA_real_
  }
  else {
    maxidx <- which.max(diffkl) + 1L
  }
  return(c(kl_shift_max = max(diffkl, na.rm = TRUE), kl_shift_index = maxidx))
}
