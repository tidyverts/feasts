#' (Partial) Autocorrelation and Cross-Correlation Function Estimation
#'
#' The function `ACF` computes an estimate of the autocorrelation function
#' of a (possibly multivariate) tsibble. Function `PACF` computes an estimate
#' of the partial autocorrelation function of a (possibly multivariate) tsibble.
#' Function `CCF` computes the cross-correlation or cross-covariance of two columns
#' from a tsibble.
#'
#' The functions improve the [`stats::acf()`], [`stats::pacf()`] and
#' [`stats::ccf()`] functions. The main differences are that `ACF` does not plot
#' the exact correlation at lag 0 when `type=="correlation"` and
#' the horizontal axes show lags in time units rather than seasonal units.
#'
#' The resulting tables from these functions can also be plotted using
#' [`autoplot()`] with the methods provided by the ggtime package.
#'
# The tapered versions implement the ACF and PACF estimates and plots
# described in Hyndman (2015), based on the banded and tapered estimates of
# autocovariance proposed by McMurry and Politis (2010).
#'
#' @param .data A tsibble
#' @param ... The column(s) from the tsibble used to compute the ACF, PACF or CCF.
#' @param lag_max maximum lag at which to calculate the acf. Default is 10*log10(N/m)
#' where N is the number of observations and m the number of series. Will be
#' automatically limited to one less than the number of observations in the series.
#' @param type character string giving the type of ACF to be computed. Allowed values are `"correlation"` (the default), `"covariance"` or `"partial"`.
#' @param tapered Produces banded and tapered estimates of the (partial) autocorrelation.
#' @inheritParams stats::acf
#'
#' @return The `ACF`, `PACF` and `CCF` functions return objects
#' of class "tbl_cf", which is a tsibble containing the correlations computed.
#'
#' @author Mitchell O'Hara-Wild and Rob J Hyndman
#'
#' @references Hyndman, R.J. (2015). Discussion of "High-dimensional
#' autocovariance matrices and optimal linear prediction". \emph{Electronic
#' Journal of Statistics}, 9, 792-796.
#'
#' McMurry, T. L., & Politis, D. N. (2010). Banded and tapered estimates for
#' autocovariance matrices and the linear process bootstrap. \emph{Journal of
#' Time Series Analysis}, 31(6), 471-482.
#'
#' @seealso [`stats::acf()`], [`stats::pacf()`], [`stats::ccf()`]
#'
#' @examples
#' library(tsibble)
#' library(tsibbledata)
#' library(dplyr)
#'
#' vic_elec %>% ACF(Temperature)
#'
#' vic_elec %>% ACF(Temperature) %>% autoplot()
#'
#' @importFrom tibble tibble
#' @importFrom stats as.ts frequency
#' @importFrom utils tail
#' @importFrom fabletools get_frequencies
#'
#' @rdname ACF
#' @export
ACF <- function(.data, y, ..., lag_max = NULL,
                type = c("correlation", "covariance", "partial"),
                na.action = na.contiguous, demean = TRUE, tapered = FALSE){
  type <- match.arg(type)
  compute_acf <- function(x){
    acf <- if(tapered) {
      tacf(x)[seq_len(lag_max+1)]
    } else {
      as.numeric(acf(x, plot=FALSE, lag.max=lag_max, demean=demean,
                     type=type, na.action=na.action)$acf)
    }
    if(type != "partial"){ # First indx already dropped if partial
      acf <- tail(acf, -1)
    }
    list(lag = seq_along(acf), value = acf)
  }
  if(dots_n(...) > 0) {
    lifecycle::deprecate_warn(
      "0.2.2", "PACF(...)", details = "ACF variables should be passed to the `y` argument. If multiple variables are to be used, specify them using `vars(...)`."
    )
  }
  value <- Filter(negate(quo_is_missing), enquos(y, ...))
  if(length(value) == 0){
    if(is_empty(measured_vars(.data))){
      abort("There are no variables to compute the ACF.")
    }
    inform(sprintf(
      "Response variable not specified, automatically selected `var = %s`",
      measured_vars(.data)[1]
    ))
    value <- syms(measured_vars(.data)[1])
  }
  if(length(value) > 1){
    warn(sprintf("ACF currently only supports one column, `%s` will be used.",
                 as_name(value[[1]])))
  }
  build_cf(.data, compute_acf, value[1], "acf")
}

#' @rdname ACF
#' @examples
#' vic_elec %>% PACF(Temperature)
#'
#' vic_elec %>% PACF(Temperature) %>% autoplot()
#'
#' @export
PACF <- function(.data, y, ..., lag_max = NULL,
                 na.action = na.contiguous,
                 tapered = FALSE){
  compute_pacf <- function(x){
    lag.max <- lag_max

    if (tapered) {
      # Compute acf using tapered estimate
      acvf <- tacf(x)
      # Durbin-Levinson recursions
      # Modified from http://faculty.washington.edu/dbp/s519/R-code/LD-recursions.R
      p <- length(acvf) - 1
      phis <- acvf[2] / acvf[1]
      pev <- rep(acvf[1], p + 1)
      pacf <- rep(phis, lag.max)
      pev[2] <- pev[1] * (1 - phis ^ 2)
      if (p > 1) {
        for (k in 2:lag.max)
        {
          old.phis <- phis
          phis <- rep(0, k)
          ## compute kth order pacf (reflection coefficient)
          phis[k] <- (acvf[k + 1] - sum(old.phis * acvf[k:2])) / pev[k]
          phis[1:(k - 1)] <- old.phis - phis[k] * rev(old.phis)
          pacf[k] <- phis[k]
          pev[k + 1] <- pev[k] * (1 - phis[k] ^ 2)
          # if(abs(pacf[k]) > 1)
          #  warning("PACF larger than 1 in absolute value")
        }
      }
    } else {
      pacf <- as.numeric(pacf(x, plot=FALSE, lag.max = lag.max, na.action = na.action)$acf)
    }

    list(lag = seq_along(pacf), value = pacf)
  }
  if(dots_n(...) > 0) {
    lifecycle::deprecate_warn(
      "0.2.2", "PACF(...)", details = "PACF variables should be passed to the `y` argument. If multiple variables are to be used, specify them using `vars(...)`."
    )
  }
  value <- Filter(negate(quo_is_missing), enquos(y, ...))
  if(length(value) == 0){
    if(is_empty(measured_vars(.data))){
      abort("There are no variables to compute the PACF.")
    }
    inform(sprintf(
      "Response variable not specified, automatically selected `var = %s`",
      measured_vars(.data)[1]
    ))
    value <- syms(measured_vars(.data)[1])
  }
  if(length(value) > 1){
    warn(sprintf("PACF currently only supports one column, `%s` will be used.",
                 as_name(value[[1]])))
  }
  build_cf(.data, compute_pacf, value[1], "pacf")
}

#' @rdname ACF
#' @examples
#' global_economy %>%
#'   filter(Country == "Australia") %>%
#'   CCF(GDP, Population)
#'
#' global_economy %>%
#'   filter(Country == "Australia") %>%
#'   CCF(GDP, Population) %>%
#'   autoplot()
#'
#' @export
CCF <- function(.data, y, x, ..., lag_max = NULL,
                type = c("correlation", "covariance"),
                na.action = na.contiguous){
  compute_ccf <- function(y, x){
    ccf <- ccf(y = y, x = x, plot=FALSE, lag.max=lag_max, type=type,
               na.action=na.action)
    list(lag = as.integer(ccf$lag), value = as.numeric(ccf$acf))
  }
  if(dots_n(...) > 0) {
    lifecycle::deprecate_warn(
      "0.2.2", "CCF(...)", details = "CCF variables should be passed to the `y` and `x` arguments. If multiple variables are to be used, specify them using `vars(...)`."
    )
  }
  value <- Filter(negate(quo_is_missing), enquos(y, x, ...))
  if(length(value) == 0){
    if(length(measured_vars(.data)) < 2){
      abort("CCF requires two columns to be specified.")
    }
    inform(sprintf(
      "Response variable not specified, automatically selected `%s` and `%s`",
      measured_vars(.data)[1], measured_vars(.data)[2]
    ))
    value <- syms(measured_vars(.data)[1:2])
  }
  if(length(value) > 2){
    warn(sprintf("CCF currently only supports two columns, `%s` and `%s` will be used.",
                 as_name(value[[1]]), as_name(value[[2]])))
  }
  if(length(value) == 1){
    abort("CCF requires two columns to be specified.")
  }
  build_cf(.data, compute_ccf, value[1:2], "ccf")
}

# Compute a correlation function for each series of `.data`.
#
# `cf_fn` takes the values of the quosures in `values` (evaluated within each
# series) and returns a list of integer `lag` counts and numeric `value`s.
# The result is a tsibble indexed by lag, with the values in column `name`.
#' @importFrom stats na.contiguous
build_cf <- function(.data, cf_fn, values, name){
  check_gaps(.data)
  regular <- is_regular(.data)
  if(!regular){
    warn("Provided data has an irregular interval, results should be treated with caution. Computing ACF by observation.")
  }

  keys <- as_tibble(key_data(.data))
  rows <- keys[[".rows"]]
  keys <- keys[setdiff(names(keys), ".rows")]
  data <- as_tibble(.data)

  cf <- map(rows, function(i){
    series <- vctrs::vec_slice(data, i)
    exec(cf_fn, !!!map(values, eval_tidy, data = series))
  })
  n_lags <- map_int(cf, function(x) length(x[["lag"]]))
  # Series of different lengths have different lags, so lags are counted per
  # series and converted to durations (e.g. months) in one step.
  lag <- lag_duration(
    as.integer(unlist(map(cf, function(x) x[["lag"]]))), rep.int(seq_along(rows), n_lags),
    series_chronons(.data, rows, regular)
  )

  out <- vctrs::vec_slice(keys, rep.int(seq_along(rows), n_lags))
  out[["lag"]] <- lag
  out[[name]] <- as.numeric(unlist(map(cf, function(x) x[["value"]])))

  # The lags of each series are ordered and unique, so the tsibble is built
  # without the (slow) validation of its index.
  ends <- cumsum(n_lags)
  key_rows <- keys
  key_rows[[".rows"]] <- vctrs::new_list_of(
    map2(ends - n_lags + 1L, ends, seq2), ptype = integer()
  )
  tsbl <- tsibble::build_tsibble(
    out, key = !!key_vars(.data), key_data = key_rows, index = "lag",
    ordered = TRUE, interval = tsibble::interval_pull(lag), validate = FALSE
  )

  keys[[".len"]] <- as.numeric(lengths(rows))
  new_tsibble(tsbl, num_obs = keys, class = "tbl_cf")
}

# The time between observations of each series (in key order), e.g. 1 month for
# a monthly series, as a list of distinct chronons with the one used by each
# series. Lags are in observations when the data is irregular.
series_chronons <- function(.data, rows, regular){
  n <- length(rows)
  if(!regular) {
    return(list(units = list(mixtime::mt_unit(1L)), id = rep.int(1L, n)))
  }
  index <- .data[[index_var(.data)]]
  # Only mixtime indices have units that differ between series
  if(!mixtime::is_mixtime(index)) {
    chronon <- mixtime::chronon_glb(mixtime::as_mixtime(index))
    return(list(units = list(chronon), id = rep.int(1L, n)))
  }
  chronons <- map(rows, function(i) mixtime::chronon_glb(index[i]))
  # Chronons are S7 objects, which are compared by their printed form
  label <- map_chr(chronons, function(x) paste(format(x), collapse = " "))
  first <- !duplicated(label)
  list(units = chronons[first], id = match(label, label[first]))
}

# Durations of `lag` counts (one per row), in the unit of each row's series.
# `series` gives the series of each row, and `chronons` its time unit.
lag_duration <- function(lag, series, chronons){
  unit <- chronons$id[series]
  if(length(chronons$units) == 1L){
    return(mixtime::duration(lag, chronon = !!chronons$units[[1]]))
  }
  rows <- map(seq_along(chronons$units), function(u) which(unit == u))
  pieces <- map2(rows, chronons$units, function(r, u) {
    mixtime::duration(lag[r], chronon = !!u)
  })
  vctrs::vec_c(!!!pieces)[order(unlist(rows))]
}

tacf <- function(x) {
  acf <- as.numeric(acf(x, plot=FALSE, lag.max=length(x)-1)$acf)
  # Taper estimates
  s <- seq_along(acf)
  upper <- 2 * sqrt(log(length(x), 10) / length(x))
  ac <- abs(acf)
  # Find l: ac < upper for 5 consecutive lags
  j <- (ac < upper)
  l <- 0
  k <- 1
  N <- length(j) - 4
  while (l < 1 && k <= N) {
    if (all(j[k:(k + 4)])) {
      l <- k
    } else {
      k <- k + 1
    }
  }
  sl <- s / l
  k <- numeric(length(sl))
  k[sl <= 1] <- 1
  k[sl > 1 & sl <= 2] <- 2 - sl[sl > 1 & sl <= 2]

  acf <- acf * k
  # End of Tapering

  # Now do some shrinkage towards white noise using eigenvalues
  # Construct covariance matrix
  gamma <- acf
  s <- length(gamma)
  Gamma <- matrix(1, s, s)
  d <- row(Gamma) - col(Gamma)
  for (i in 1:(s - 1))
    Gamma[d == i | d == (-i)] <- gamma[i + 1]
  # Compute eigenvalue decomposition
  ei <- eigen(Gamma)
  # Shrink eigenvalues
  d <- pmax(ei$values, 20 / length(x))
  # Construct new covariance matrix
  Gamma2 <- ei$vectors %*% diag(d) %*% t(ei$vectors)
  Gamma2 <- Gamma2 / mean(d)
  # Estimate new ACF
  d <- row(Gamma2) - col(Gamma2)
  for (i in 2:s)
    gamma[i] <- mean(Gamma2[d == (i - 1)])

  ############### end of shrinkage
  gamma
}
