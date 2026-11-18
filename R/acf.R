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
  compute_acf <- function(.data, value, lag.max, ...){
    value <- enexpr(value)
    x <- eval_tidy(value, data = .data)

    acf <- if(tapered) {
      tacf(x)[seq_len(lag_max+1)]
    } else {
      as.numeric(acf(x, plot=FALSE, lag.max=lag.max, ...)$acf)
    }

    if(type != "partial"){ # First indx already dropped if partial
      acf <- tail(acf, -1)
    }
    tibble(lag = seq_along(acf), acf = acf)
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
  build_cf(.data, compute_acf, value=!!value[[1]], lag.max = lag_max,
           demean = demean, type = type, na.action = na.action)
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
  compute_pacf <- function(.data, value, lag.max, ...){
    value <- enexpr(value)
    x <- eval_tidy(value, data = .data)

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
      pacf <- as.numeric(pacf(x, plot=FALSE, lag.max = lag.max, ...)$acf)
    }

    tibble(lag = seq_along(pacf), pacf = pacf)
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
  build_cf(.data, compute_pacf, value=!!value[[1]], lag.max = lag_max,
           na.action = na.action)
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
  compute_ccf <- function(.data, value1, value2, ...){
    value1 <- enexpr(value1)
    value2 <- enexpr(value2)
    ccf <- ccf(y = eval_tidy(value1, data = .data),
               x = eval_tidy(value2, data = .data),
               plot=FALSE, ...)
    lag <- as.numeric(ccf$lag)
    tibble(lag = lag, ccf = as.numeric(ccf$acf))
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
  build_cf(.data, compute_ccf, value1=!!value[[1]], value2=!!value[[2]],
           lag.max = lag_max, type = type, na.action = na.action)
}

#' @importFrom stats na.contiguous
build_cf <- function(.data, cf_fn, ...){
  check_gaps(.data)
  if(!is_regular(.data)){
    warn("Provided data has an irregular interval, results should be treated with caution. Computing ACF by observation.")
  }
  chronons <- series_chronons(.data)

  kv <- key_vars(.data)

  lens <- key_data(.data) %>%
    transmute(
      !!!key(.data),
      .len = map_dbl(!!sym(".rows"), length)
    )

  .data <- nest_keys(.data)
  .data[["data"]] <- map(.data[["data"]], cf_fn, ...)
  # Lags are counted per series, and then converted to durations in one step
  # as combining many small duration vectors is slow.
  series_len <- map_int(.data[["data"]], nrow)
  .data[["data"]] <- map(.data[["data"]], function(x) {
    x[["lag"]] <- as.integer(x[["lag"]])
    x
  })
  .data <- unnest_tbl(.data, "data")
  .data[["lag"]] <- lag_duration(.data[["lag"]], chronons, series_len)
  new_tsibble(
    as_tsibble(.data, index = "lag", key = !!kv),
    num_obs = lens, class = "tbl_cf"
  )
}

# The time between observations of each series (in key order), e.g. 1 month for
# a monthly series. Lags are in observations when the data is irregular.
# The index is converted once, as converting each series separately is slow.
series_chronons <- function(.data){
  rows <- key_data(.data)[[".rows"]]
  if(!is_regular(.data)){
    return(rep(list(mixtime::mt_unit(1L)), length(rows)))
  }
  index <- mixtime::as_mixtime(.data[[index_var(.data)]])
  map(rows, function(i) mixtime::chronon_glb(index[i]))
}

# Durations of `lag` counts, where series `i` (of `series_len[i]` rows) counts
# in units of `chronons[[i]]`. Each distinct chronon is converted once.
lag_duration <- function(lag, chronons, series_len){
  # Chronons are S7 objects, which are compared by their printed form
  id <- map_chr(chronons, function(x) paste(format(x), collapse = " "))
  first <- !duplicated(id)
  units <- chronons[first]
  unit <- rep(match(id, id[first]), series_len)
  if(length(units) == 1L){
    return(mixtime::duration(lag, chronon = units[[1]]))
  }
  rows <- map(seq_along(units), function(u) which(unit == u))
  pieces <- map2(rows, units, function(r, u) mixtime::duration(lag[r], chronon = u))
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
