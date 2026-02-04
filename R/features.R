#' @inherit tsfeatures::crossing_points
#' @importFrom stats median
#' @export
n_crossing_points <- function(x)
{
  midline <- median(x, na.rm = TRUE)
  ab <- x <= midline
  lenx <- length(x)
  p1 <- ab[1:(lenx - 1)]
  p2 <- ab[2:lenx]
  cross <- (p1 & !p2) | (p2 & !p1)
  c(n_crossing_points = sum(cross, na.rm = TRUE))
}

#' @inherit tsfeatures::arch_stat
#' @importFrom stats lm embed
#' @export
stat_arch_lm <- function(x, lags = 12, demean = TRUE)
{
  if (length(x) <= lags + 1) {
    return(c(arch_lm = NA_real_))
  }
  if (demean) {
    x <- x - mean(x, na.rm = TRUE)
  }
  mat <- embed(x^2, lags + 1)
  fit <- lm(mat[, 1] ~ mat[, -1])
  stat_arch_lm <- suppressWarnings(summary(fit)$r.squared)
  c(stat_arch_lm = if(is.nan(stat_arch_lm)) 1 else stat_arch_lm)
}

#' STL features
#'
#' Computes a variety of measures extracted from an STL decomposition of the
#' time series. This includes details about the strength of trend and seasonality.
#'
#' @param x A vector to extract features from.
#' @param .period The period of the seasonality.
#' @param s.window The seasonal window of the data (passed to [`stats::stl()`])
#' @param ... Further arguments passed to [`stats::stl()`]
#'
#' @seealso
#' [Forecasting Principle and Practices: Measuring strength of trend and seasonality](https://otexts.com/fpp3/stlfeatures.html)
#'
#' @return A vector of numeric features from a STL decomposition.
#'
#' @importFrom stats var coef
#' @export
feat_stl <- function(x, .period, s.window = 11, ...){
  dots <- dots_list(...)
  dots <- dots[names(dots) %in% names(formals(stats::stl))]
  season.args <- if (length(x) <= .period * 2) {
    list()
  } else {
    list2(!!(names(.period)%||%as.character(.period)) := list(period = .period, s.window = s.window))
  }

  rle_na <- rle(!is.na(x))
  if(any(!rle_na$values)){
    rle_window <- which(rle_na$values)[which.max(rle_na$lengths[rle_na$values])]
    rle_idx <- cumsum(rle_na$lengths)
    rle_window <- c(
      if(rle_window == 1) 1 else rle_idx[max(1, rle_window - 1)] + (rle_window > 1),
      rle_idx[rle_window]
    )
    x <- x[seq(rle_window[1], rle_window[2])]
  }
  else{
    rle_window <- c(1, length(x))
  }

  dcmp <- eval_tidy(quo(estimate_stl(x, trend.args = list(),
                    season.args = season.args, lowpass.args = list(), !!!dots)))
  trend <- dcmp[["trend"]]
  remainder <- dcmp[["remainder"]]
  season_adjust <- dcmp[["season_adjust"]]
  seasonalities <- dcmp[seq_len(length(dcmp) - 3) + 1]
  names(seasonalities) <- sub("season_", "", names(seasonalities))

  var_e <- var(remainder, na.rm = TRUE)
  n <- length(x)

  # Spikiness
  d <- (remainder - mean(remainder, na.rm = TRUE))^2
  var_loo <- (var_e * (n - 1) - d)/(n - 2)
  spikiness <- var(var_loo, na.rm = TRUE)

  # Linearity & curvature
  tren.coef <- coef(lm(trend ~ poly(seq(n), degree = 2L)))[2L:3L]
  linearity <- tren.coef[[1L]]
  curvature <- tren.coef[[2L]]

  # Strength of terms
  trend_strength <- max(0, min(1, 1 - var_e/var(season_adjust, na.rm = TRUE)))
  seasonal_strength <- map_dbl(seasonalities, function(seas){
    max(0, min(1, 1 - var_e/var(remainder + seas, na.rm = TRUE)))
  })
  names(seasonal_strength) <- sprintf("seasonal_strength_%s", names(seasonalities))

  # Position of peaks and troughs
  seasonal_peak <- map_dbl(seasonalities, function(seas){
    (which.max(seas) + rle_window[1] - 1) %% .period
  })
  names(seasonal_peak) <- sprintf("seasonal_peak_%s", names(seasonalities))
  seasonal_trough <- map_dbl(seasonalities, function(seas){
    (which.min(seas) + rle_window[1] - 1) %% .period
  })
  names(seasonal_trough) <- sprintf("seasonal_trough_%s", names(seasonalities))

  acf_resid <- stats::acf(remainder, lag.max = max(c(10, .period)),
                     plot = FALSE, na.action = stats::na.pass ,...)$acf

  c(
    trend_strength = trend_strength, seasonal_strength,
    seasonal_peak,  seasonal_trough,
    spikiness = spikiness, linearity = linearity, curvature = curvature,
    stl_e_acf1 = acf_resid[2L], stl_e_acf10 = sum((acf_resid[2L:11L])^2)
  )
}

#' Unit root tests
#'
#' Performs a test for the existence of a unit root in the vector.
#'
#' \code{unitroot_kpss} computes the statistic for the Kwiatkowski et al. unit root test with linear trend and lag 1.
#'
#' \code{unitroot_pp} computes the statistic for the `Z-tau` version of Phillips & Perron unit root test with constant trend and lag 1.
#'
#' @param x A vector to be tested for the unit root.
#' @inheritParams urca::ur.kpss
#' @param ... Arguments passed to unit root test function.
#'
#' @return A vector of numeric features for the test's statistic and p-value.
#'
#' @seealso [urca::ur.kpss()]
#'
#' @rdname unitroot
#' @export
unitroot_kpss <- function(x, type = c("mu", "tau"), lags = c("short", "long", "nil"), ...) {
  check_installed("urca")
  result <- urca::ur.kpss(x, type = type, lags = lags, ...)
  pval <- stats::approx(result@cval[1,], as.numeric(sub("pct", "", colnames(result@cval)))/100, xout=result@teststat[1], rule=2)$y
  c(kpss_stat = result@teststat, kpss_pvalue = pval)
}

#' @inheritParams urca::ur.pp
#' @rdname unitroot
#'
#' @seealso [urca::ur.pp()]
#'
#' @export
unitroot_pp <- function(x, type = c("Z-tau", "Z-alpha"), model = c("constant", "trend"),
                        lags = c("short", "long"), ...) {
  check_installed("urca")
  result <- urca::ur.pp(x, type = match.arg(type), model = match.arg(model),
                        lags = match.arg(lags), ...)
  pval <- stats::approx(result@cval[1,], as.numeric(sub("pct", "", colnames(result@cval)))/100, xout=result@teststat[1], rule=2)$y
  c(pp_stat = result@teststat, pp_pvalue = pval)
}

#' Number of differences required for a stationary series
#'
#' Use a unit root function to determine the minimum number of differences
#' necessary to obtain a stationary time series.
#'
#' Note that the default 'unit root function' for `unitroot_nsdiffs()` is based
#' on the seasonal strength of an STL decomposition. This is not a test for the
#' presence of a seasonal unit root, but generally works reasonably well in
#' identifying the presence of seasonality and the need for a seasonal
#' difference.
#'
#' @inheritParams unitroot_kpss
#' @param alpha The level of the test.
#' @param unitroot_fn A function (or lambda) that provides a p-value for a unit root test.
#' @param differences The possible differences to consider.
#' @param ... Additional arguments passed to the `unitroot_fn` function
#'
#' @return A numeric corresponding to the minimum required differences for stationarity.
#'
#' @export
unitroot_ndiffs <- function(x, alpha = 0.05, unitroot_fn = ~ unitroot_kpss(.)["kpss_pvalue"],
                            differences = 0:2, ...) {
  unitroot_fn <- as_function(unitroot_fn)

  diff <- function(x, differences, ...){
    if(differences == 0) return(x)
    base::diff(x, differences = differences, ...)
  }

  # Non-missing x
  keep <- map_lgl(differences, function(.x){
    dx <- diff(x, differences = .x)
    !all(is.na(dx))
  })
  differences <- differences[keep]

  # Estimate the test
  keep <- map_lgl(differences[-1]-1, function(.x) {
    unitroot_fn(diff(x, differences = .x), ...) < alpha
  })

  c(ndiffs = max(differences[c(TRUE, keep)], na.rm = TRUE))
}

#' @rdname unitroot_ndiffs
#' @param .period The period of the seasonality.
#'
#' @export
unitroot_nsdiffs <- function(x, alpha = 0.05, unitroot_fn = ~ feat_stl(.,.period)[2]<0.64,
                             differences = 0:2, .period = 1, ...) {
  if(.period == 1) return(c(nsdiffs = min(differences)))

  unitroot_fn <- as_function(unitroot_fn)
  environment(unitroot_fn) <- new_environment(parent = get_env(unitroot_fn))
  environment(unitroot_fn)$.period <- .period

  diff <- function(x, differences, ...){
    if(differences == 0) return(x)
    base::diff(x, differences = differences, ...)
  }

  # Non-missing x
  keep <- map_lgl(differences, function(.x){
    dx <- diff(x, lag = .period, differences = .x)
    !all(is.na(dx))
  })
  differences <- differences[keep]

  # Estimate the test
  keep <- map_lgl(differences[-1]-1, function(.x) {
    unitroot_fn(diff(x, lag = .period, differences = .x)) < alpha
  })

  c(nsdiffs = max(differences[c(TRUE, keep)], na.rm = TRUE))
}

#' @inherit urca::ca.jo
#'
#' @param ... Additional arguments passed to [urca::ca.jo()].
#'
#' @seealso [urca::ca.jo()]
#'
#' @examples
#'
#' cointegration_johansen(cbind(mdeaths, fdeaths))
#'
#'
#' @export
cointegration_johansen <- function(x, ...) {
  check_installed("urca")
  result <- urca::ca.jo(x, ...)

  pct <- as.numeric(sub("pct", "", colnames(result@cval)))/100
  pval <- mapply(
    function(cval, teststat) {
      stats::approx(cval, pct, xout=teststat, rule=2)$y
    }, split(result@cval, row(result@cval)), result@teststat
  )
  names(pval) <- names(result@teststat) <- c(paste0("R<=", rev(seq_len(length(pval) - 1))), "R=0")
  c(johansen_stat = list(result@teststat), johansen_pvalue = list(pval))
}

#' Phillips and Ouliaris Cointegration Features
#'
#' Compute Phillips and Ouliaris (1990) residual-based cointegration test
#' statistics and an approximate p-value as numeric features.
#'
#' This is a small wrapper around \code{urca::ca.po()} designed so that the
#' Phillips–Ouliaris test can be used directly inside
#' \code{\link[fabletools]{features}}.
#'
#' @description
#' \code{cointegration_phillips_ouliaris()} calls \code{urca::ca.po()} and
#' returns a named numeric vector containing:
#' \itemize{
#'   \item \code{phillips_ouliaris_stat}: the \eqn{P_u} or \eqn{P_z} test
#'   statistic; and
#'   \item \code{phillips_ouliaris_pvalue}: an approximate p-value obtained by
#'   linearly interpolating the tabulated critical values in
#'   \code{result@cval}.
#' }
#'
#' Since it returns a simple numeric vector, this function is suitable for use
#' as a feature extractor within the \pkg{fabletools}
#' \code{\link[fabletools]{features}} framework.
#'
#' @param x
#'   A numeric matrix (or object coercible to a matrix) of time series to be
#'   tested for cointegration. Columns represent series and rows represent
#'   ordered observations.
#'
#' @param ...
#'   Additional arguments passed to \code{urca::ca.po()}, such as
#'   \code{demean}, \code{lag}, \code{type}, and \code{tol}. See
#'   \code{\link[urca]{ca.po}} for details.
#'
#' @details
#' The function requires the \pkg{urca} package; an informative error is raised
#' if it is not installed.
#'
#' The p-value is computed by interpolating over the first row of
#' \code{result@cval}, which contains critical values at various significance
#' levels (e.g., \code{"10pct"}, \code{"5pct"}, \code{"1pct"}). These labels
#' are converted to probabilities (0.10, 0.05, 0.01), and
#' \code{\link[stats]{approx}} is used to obtain the approximate p-value at the
#' observed test statistic. The interpolation is done with \code{rule = 2},
#' implying linear extrapolation outside the tabulated range.
#'
#' @return
#' A named numeric vector of length two:
#' \itemize{
#'   \item \code{phillips_ouliaris_stat}
#'   \item \code{phillips_ouliaris_pvalue}
#' }
#'
#' @references
#' Phillips, P.C.B. and Ouliaris, S. (1990),
#' \dQuote{Asymptotic Properties of Residual Based Tests for Cointegration},
#' \emph{Econometrica}, \bold{58}(1), 165–193.
#'
#' @seealso
#' \code{\link[urca]{ca.po}},
#' \code{\link[fabletools]{features}}
#'
#' @examples
#'
#' cointegration_phillips_ouliaris(cbind(mdeaths, fdeaths))
#'
#' @export
cointegration_phillips_ouliaris <- function(x, ...) {
  check_installed("urca")
  result <- urca::ca.po(x, ...)
  pval <- stats::approx(result@cval[1,], as.numeric(sub("pct", "", colnames(result@cval)))/100, xout=result@teststat[1], rule=2)$y
  c(phillips_ouliaris_stat = result@teststat, phillips_ouliaris_pvalue = pval)
}

#' Longest flat spot length
#'
#' "Flat spots” are computed by dividing the sample space of a time series into
#' ten equal-sized intervals, and computing the maximum run length within any
#' single interval.
#'
#' @param x a vector
#' @return A numeric value.
#' @author Earo Wang and Rob J Hyndman
#'
#' @aliases n_flat_spots
#' @export
longest_flat_spot <- function(x) {
  cutx <- cut(x, breaks = 10, include.lowest = TRUE, labels = FALSE)
  rlex <- rle(cutx)
  return(c(longest_flat_spot = max(rlex$lengths)))
}

#' @export
n_flat_spots <- function(x) {
  lifecycle::deprecate_warn("0.1.5", "feasts::n_flat_spots()", "feasts::longest_flat_spot()")
  longest_flat_spot(x)
}

#' Hurst coefficient
#'
#' Computes the Hurst coefficient indicating the level of fractional differencing
#' of a time series.
#'
#' @param x a vector. If missing values are present, the largest
#' contiguous portion of the vector is used.
#' @return A numeric value.
#' @author Rob J Hyndman
#'
#' @export
coef_hurst <- function(x) {
  check_installed("fracdiff")
  # Hurst=d+0.5 where d is fractional difference.
  return(c(coef_hurst = suppressWarnings(fracdiff::fracdiff(na.contiguous(x), 0, 0)[["d"]] + 0.5)))
}

#' Sliding window features
#'
#' Computes feature of a time series based on sliding (overlapping) windows.
#' \code{shift_level_max} finds the largest mean shift between two consecutive windows.
#' \code{shift_var_max} finds the largest var shift between two consecutive windows.
#' \code{shift_kl_max} finds the largest shift in Kulback-Leibler divergence between
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
shift_level_max <- function(x, .size = NULL, .period = 1) {
  if(is.null(.size)){
    .size <- ifelse(.period == 1, 10, .period)
  }

  rollmean <- slider::slide_dbl(x, mean, .before = .size - 1, na.rm = TRUE)

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

  return(c(shift_level_max = maxmeans, shift_level_index = maxidx))
}

#' @rdname shift_level_max
#' @export
shift_var_max <- function(x, .size = NULL, .period = 1) {
  if(is.null(.size)){
    .size <- ifelse(.period == 1, 10, .period)
  }

  rollvar <- slider::slide_dbl(x, var, .before = .size - 1, na.rm = TRUE)

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

  return(c(shift_var_max = maxvar, shift_var_index = maxidx))
}

#' @rdname shift_level_max
#' @export
shift_kl_max <- function(x, .size = NULL, .period = 1) {
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
    abort("length of `x` is too short for `.size`.")
  }

  densities <- map(xgrid, function(xgrid) stats::dnorm(xgrid, mean = x, sd = bw))
  densities <- map(densities, pmax, stats::dnorm(38))

  rmean <- map(densities, function(x)
    slider::slide_dbl(x, mean, .before = .size - 1, na.rm = TRUE)
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
  return(c(shift_kl_max = max(diffkl, na.rm = TRUE), shift_kl_index = maxidx))
}

#' Spectral features of a time series
#'
#' @description
#' Computes spectral entropy from a univariate normalized
#' spectral density, estimated using an AR model.
#'
#' @details
#' The \emph{spectral entropy} equals the Shannon entropy of the spectral density
#' \eqn{f_x(\lambda)} of a stationary process \eqn{x_t}:
#' \deqn{
#' H_s(x_t) = - \int_{-\pi}^{\pi} f_x(\lambda) \log f_x(\lambda) d \lambda,
#' }
#' where the density is normalized such that
#' \eqn{\int_{-\pi}^{\pi} f_x(\lambda) d \lambda = 1}.
#' An estimate of \eqn{f(\lambda)} can be obtained using \code{\link[stats]{spec.ar}} with
#' the `burg` method.
#'
#' @inheritParams shift_level_max
#' @param .period The seasonal period.
#' @param ... Further arguments for [`stats::spec.ar()`]
#'
#' @author Rob J Hyndman
#' @return
#' A non-negative real value for the spectral entropy \eqn{H_s(x_t)}.
#' @seealso \code{\link[stats]{spec.ar}}
#' @references
#' Jerry D. Gibson and Jaewoo Jung (2006). \dQuote{The
#' Interpretation of Spectral Entropy Based Upon Rate Distortion Functions}.
#' IEEE International Symposium on Information Theory, pp. 277-281.
#'
#' Goerg, G. M. (2013). \dQuote{Forecastable Component Analysis}.
#' Journal of Machine Learning Research (JMLR) W&CP 28 (2): 64-72, 2013.
#' Available at \url{https://proceedings.mlr.press/v28/goerg13.html}.
#'
#' @examples
#' feat_spectral(rnorm(1000))
#' feat_spectral(lynx)
#' feat_spectral(sin(1:20))
#' @export
feat_spectral <- function(x, .period = 1, ...) {
  if(all(x == x[1])) return(c(spectral_entropy = NA_real_))
  #spec <- spectrum(x, plot = FALSE, n.freq = ceiling(length(x)/2 + 1), ...)
  spec <- try(stats::spec.ar(na.contiguous(ts(x, frequency = .period)),
                             plot=FALSE, method='burg',
                             n.freq = ceiling(length(x)/2 + 1)), ...)
  if (inherits(spec, "try-error")) {
    entropy <- NA
  } else {
    fx <- c(rev(spec$spec[-1]),spec$spec)/ length(x)
    fx <- fx/sum(fx)
    prior.fx = rep(1 / length(fx), length = length(fx))
    prior.weight = 0.001
    fx <- (1 - prior.weight) * fx + prior.weight * prior.fx
    entropy <- pmin(1, -sum(fx * log(fx, base = length(x))))
  }
  return(c(spectral_entropy = entropy))
}

#' Time series features based on tiled windows
#'
#' Computes feature of a time series based on tiled (non-overlapping) windows.
#' Means or variances are produced for all tiled windows. Then stability is
#' the variance of the means, while lumpiness is the variance of the variances.
#'
#' @inheritParams shift_level_max
#' @return A numeric vector of length 2 containing a measure of lumpiness and
#' a measure of stability.
#' @author Earo Wang and Rob J Hyndman
#'
#' @rdname tile_features
#'
#' @importFrom stats var
#' @export
var_tiled_var <- function(x, .size = NULL, .period = 1) {
  if(is.null(.size)){
    .size <- ifelse(.period == 1, 10, .period)
  }

  x <- scale(x, center = TRUE, scale = TRUE)
  varx <- slider::slide_dbl(x[,1], var, na.rm = TRUE,
                            .after = .size - 1, .step = .size, .complete = TRUE)
  if (length(x) < 2 * .size) {
    lumpiness <- 0
  } else {
    lumpiness <- var(varx, na.rm = TRUE)
  }
  return(c(var_tiled_var = lumpiness))
}

#' @rdname tile_features
#' @export
var_tiled_mean <- function(x, .size = NULL, .period = 1) {
  if(is.null(.size)){
    .size <- ifelse(.period == 1, 10, .period)
  }

  x <- scale(x, center = TRUE, scale = TRUE)
  meanx <- slider::slide_dbl(x, mean, na.rm = TRUE,
                             .after = .size - 1, .step = .size)

  if (length(x) < 2 * .size) {
    stability <- 0
  } else {
    stability <- var(meanx, na.rm = TRUE)
  }
  return(c(var_tiled_mean = stability))
}

#' Autocorrelation-based features
#'
#' Computes various measures based on autocorrelation coefficients of the
#' original series, first-differenced series and second-differenced series
#'
#' @inheritParams var_tiled_var
#' @param lag_max maximum lag at which to calculate the acf. The default is
#' `max(.period, 10L)` for `feat_acf`, and `max(.period, 5L)` for `feat_pacf`
#' @param ... Further arguments passed to [`stats::acf()`] or [`stats::pacf()`]
#'
#' @return A vector of 6 values: first autocorrelation coefficient and sum of squared of
#' first ten autocorrelation coefficients of original series, first-differenced series,
#' and twice-differenced series.
#' For seasonal data, the autocorrelation coefficient at the first seasonal lag is
#' also returned.
#'
#' @author Thiyanga Talagala
#'
#' @export
feat_acf <- function(x, .period = 1, lag_max = NULL, ...) {
  acfx <- stats::acf(x, lag.max = lag_max%||%max(.period, 10L), plot = FALSE, na.action = stats::na.pass ,...)
  acfdiff1x <- stats::acf(diff(x, differences = 1), lag.max = lag_max%||%10L, plot = FALSE, na.action = stats::na.pass)
  acfdiff2x <- stats::acf(diff(x, differences = 2), lag.max = lag_max%||%10L, plot = FALSE, na.action = stats::na.pass)

  # first autocorrelation coefficient
  acf_1 <- acfx$acf[2L]

  # sum of squares of first 10 autocorrelation coefficients
  sum_of_sq_acf10 <- sum((acfx$acf[2L:11L])^2)

  # first autocorrelation coefficient of differenced series
  diff1_acf1 <- acfdiff1x$acf[2L]

  # Sum of squared of first 10 autocorrelation coefficients of differenced series
  diff1_acf10 <- sum((acfdiff1x$acf[-1L])^2)

  # first autocorrelation coefficient of twice-differenced series
  diff2_acf1 <- acfdiff2x$acf[2L]

  # Sum of squared of first 10 autocorrelation coefficients of twice-differenced series
  diff2_acf10 <- sum((acfdiff2x$acf[-1L])^2)

  output <- c(
    acf1 = unname(acf_1),
    acf10 = unname(sum_of_sq_acf10),
    diff1_acf1 = unname(diff1_acf1),
    diff1_acf10 = unname(diff1_acf10),
    diff2_acf1 = unname(diff2_acf1),
    diff2_acf10 = unname(diff2_acf10)
  )

  if (.period > 1) {
    output <- c(output, season_acf1 = unname(acfx$acf[.period + 1L]))
  }

  return(output)
}

#' Partial autocorrelation-based features
#'
#' Computes various measures based on partial autocorrelation coefficients of the
#' original series, first-differenced series and second-differenced series.
#'
#' @inheritParams feat_acf
#'
#' @return A vector of 3 values: Sum of squared of first 5
#' partial autocorrelation coefficients of the original series, first differenced
#' series and twice-differenced series.
#' For seasonal data, the partial autocorrelation coefficient at the first seasonal
#' lag is also returned.
#' @author Thiyanga Talagala
#' @export
feat_pacf <- function(x, .period = 1, lag_max = NULL, ...) {
  pacfx <- stats::pacf(x, lag.max = lag_max%||%max(.period, 5L),
                       plot = FALSE, ...)$acf
  # Sum of squared of first 5 partial autocorrelation coefficients
  pacf_5 <- sum((pacfx[seq(5L)])^2)

  # Sum of squared of first 5 partial autocorrelation coefficients of difference series
  diff1_pacf_5 <- sum((stats::pacf(diff(x, differences = 1),
                                   lag.max = lag_max%||%max(.period, 5L),
                                   plot = FALSE, ...)$acf)^2)

  # Sum of squared of first 5 partial autocorrelation coefficients of twice differenced series
  diff2_pacf_5 <- sum((stats::pacf(diff(x, differences = 2),
                                   lag.max = lag_max%||%max(.period, 5L),
                                   plot = FALSE, ...)$acf)^2)

  output <- c(
    pacf5 = unname(pacf_5),
    diff1_pacf5 = unname(diff1_pacf_5),
    diff2_pacf5 = unname(diff2_pacf_5)
  )
  if (.period > 1) {
    output <- c(output, season_pacf = pacfx[.period])
  }

  return(output)
}

#' Intermittency features
#'
#' Computes various measures that can indicate the presence and structures of
#' intermittent data.
#'
#' @param x A vector to extract features from.
#'
#' @return A vector of named features:
#' - zero_run_mean: The average interval between non-zero observations
#' - nonzero_squared_cv: The squared coefficient of variation of non-zero observations
#' - zero_start_prop: The proportion of data which starts with zero
#' - zero_end_prop: The proportion of data which ends with zero
#'
#' @references
#' Kostenko, A. V., & Hyndman, R. J. (2006). A note on the categorization of
#' demand patterns. \emph{Journal of the Operational Research Society}, 57(10),
#' 1256-1257.
#'
#' @export
feat_intermittent <- function(x){
  rle <- rle(x)
  nonzero <- x[x!=0]

  c(
    zero_run_mean = if(length(nonzero) == length(x)) 0 else mean(rle$lengths[rle$values == 0]),
    nonzero_squared_cv = (sd(nonzero, na.rm = TRUE) / mean(nonzero, na.rm = TRUE))^2,
    zero_start_prop = if(rle$values[1] != 0) 0 else rle$lengths[1]/length(x),
    zero_end_prop = if(rle$values[length(rle$values)] != 0) 0 else rle$lengths[length(rle$lengths)]/length(x)
  )
}
