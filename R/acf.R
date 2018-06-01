#' (Partial) Autocorrelation and Cross-Correlation Function Estimation
#'
#' The function `ACF` computes an estimate of the autocorrelation function
#' of a (possibly multivariate) tsibble. Function `PACF` computes an estimate
#' of the partial autocorrelation function of a (possibly multivariate) tsibble.
#' Function `CCF` computes the cross-correlation or cross-covariance of two columns
#' from a tsibble.
#'
#' The functions improve the \code{\link[stats]{acf}},
#' \code{\link[stats]{pacf}} and \code{\link[stats]{ccf}} functions. The main
#' differences are that `ACF` does not plot a spike at lag 0 when
#' \code{type=="correlation"} (which is redundant) and the horizontal axes show
#' lags in time units rather than seasonal units.
#'
#' The tapered versions implement the ACF and PACF estimates and plots
#' described in Hyndman (2015), based on the banded and tapered estimates of
#' autocovariance proposed by McMurry and Politis (2010).
#'
#' @param .data A tsibble
#' @param value,value1,value2 The column(s) from the tsibble used to compute the ACF, PACF or CCF.
#' @param ... Further arguments to be passed on to acf, pacf, and ccf
#'
#' @return The `ACF`, `PACF` and `CCF` functions return objects
#' of class "tbl_cf", which is a tibble containing the correlations computed.
#'
#' @author Mitchell O'Hara-Wild and Rob J Hyndman
#'
#' @references Hyndman, R.J. (2015). Discussion of ``High-dimensional
#' autocovariance matrices and optimal linear prediction''. \emph{Electronic
#' Journal of Statistics}, 9, 792-796.
#'
#' McMurry, T. L., & Politis, D. N. (2010). Banded and tapered estimates for
#' autocovariance matrices and the linear process bootstrap. \emph{Journal of
#' Time Series Analysis}, 31(6), 471-482.
#'
#' @seealso \code{\link[stats]{acf}}, \code{\link[stats]{pacf}},
#' \code{\link[stats]{ccf}}
#'
#' @examples
#' tsibbledata::elecdemand %>% ACF(Temperature)
#'
#' @importFrom tibble tibble
#' @importFrom stats as.ts frequency
#' @importFrom utils tail
#'
#' @rdname ACF
#' @export
ACF <- function(.data, value = NULL, ...){
  compute_acf <- function(.data, value, ...){
    value <- enexpr(value)
    if(is.null(value)){
      x <- as.ts(.data)
    }
    else{
      x <- as.ts(.data, !!value)
    }
    acf <- tail(as.numeric(acf(x, plot=FALSE, ...)$acf), -1)
    tibble(lag = seq_along(acf), acf = acf)
  }
  build_cf(.data, compute_acf, value=!!enexpr(value), ...)
}

#' @rdname ACF
#' @examples
#' tsibbledata::elecdemand %>% PACF(Demand)
#' @export
PACF <- function(.data, value = NULL, ...){
  compute_pacf <- function(.data, value, ...){
    value <- enexpr(value)
    if(is.null(value)){
      x <- as.ts(.data)
    }
    else{
      x <- as.ts(.data, !!value)
    }
    pacf <- tail(as.numeric(pacf(x, plot=FALSE, ...)$acf), -1)
    tibble(lag = seq_along(pacf), pacf = pacf)
  }
  build_cf(.data, compute_pacf, value=!!enexpr(value), ...)
}

#' @rdname ACF
#' @examples
#' tsibbledata::UKLungDeaths %>% CCF(mdeaths, fdeaths)
#' @export
CCF <- function(.data, value1, value2, ...){
  compute_ccf <- function(.data, value1, value2, ...){
    value1 <- enexpr(value1)
    value2 <- enexpr(value2)
    ccf <- ccf(x = as.ts(.data %>% select(!!index(.), !!value1)),
               y = as.ts(.data %>% select(!!index(.), !!value2)),
               plot=FALSE, ...)
    lag <- as.numeric(ccf$lag)*frequency(.data)
    tibble(lag = lag, ccf = as.numeric(ccf$acf))
  }
  build_cf(.data, compute_ccf, value1=!!enexpr(value1), value2=!!enexpr(value2), ...)
}

build_cf <- function(.data, cf_fn, ...){
  .data <- as_tsibble(.data)
  interval <- interval(.data)
  .data %>%
    group_by(!!!syms(key_vars(.data))) %>%
    nest %>%
    as_tibble %>%
    mutate(data = map(!!sym("data"), cf_fn, ...)) %>%
    unnest(!!sym("data")) %>%
    mutate(lag = as_lag(!!sym("lag"), interval = interval)) %>%
    enclass("tbl_cf")
}

#' @export
type_sum.lag <- function(x){
  "lag"
}

#' @export
obj_sum.lag <- function(x){
  rep("lag", length(x))
}

#' @export
pillar_shaft.lag <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format(x), align = "right", min_width = 10)
}

as_lag <- function(x, ...) {
  structure(x, ..., class = "lag")
}

#' @export
`[.lag` <- function(x, i) {
  as_lag(NextMethod(), interval = attr(x, "interval"))
}

#' @export
c.lag <- function(x, ...) {
  as_lag(NextMethod(), interval = attr(x, "interval"))
}

#' @export
format.lag <- function(x, ...){
  x %>% map_chr(~ format(attr(x, "interval") %>%
                           map(`*`, .x) %>%
                           enclass("interval")))
}

#' @export
print.lag <- function(x, ...){
  print(format(x, ...), quote = FALSE)
  invisible(x)
}

#' @export
is_vector_s3.lag <- function(x) {
  TRUE
}
