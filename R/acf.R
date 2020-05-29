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
#' The resulting tables from these functions can also be plotted using
#' [`autoplot.tbl_cf()`].
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
#' @inheritParams stats::acf
#'
#' @return The `ACF`, `PACF` and `CCF` functions return objects
#' of class "tbl_cf", which is a tsibble containing the correlations computed.
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
ACF <- function(.data, ..., lag_max = NULL, demean = TRUE,
                type = c("correlation", "covariance", "partial")){
  type <- match.arg(type)
  compute_acf <- function(.data, value, ...){
    value <- enexpr(value)
    x <- eval_tidy(value, data = .data)
    acf <- as.numeric(acf(x, plot=FALSE, ...)$acf)
    if(type != "partial"){ # First indx already dropped if partial
      acf <- tail(acf, -1)
    }
    tibble(lag = seq_along(acf), acf = acf)
  }
  value <- enexprs(...)
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
                 expr_text(value[[1]])))
  }
  build_cf(.data, compute_acf, value=!!value[[1]], lag.max = lag_max,
           demean = demean, type = type)
}

#' @rdname ACF
#' @examples
#' vic_elec %>% PACF(Temperature)
#'
#' vic_elec %>% PACF(Temperature) %>% autoplot()
#'
#' @export
PACF <- function(.data, ..., lag_max = NULL){
  compute_pacf <- function(.data, value, ...){
    value <- enexpr(value)
    x <- eval_tidy(value, data = .data)
    pacf <- as.numeric(pacf(x, plot=FALSE, ...)$acf)
    tibble(lag = seq_along(pacf), pacf = pacf)
  }
  value <- enexprs(...)
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
                 expr_text(value[[1]])))
  }
  build_cf(.data, compute_pacf, value=!!value[[1]], lag.max = lag_max)
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
CCF <- function(.data, ..., lag_max = NULL, type = c("correlation", "covariance")){
  compute_ccf <- function(.data, value1, value2, ...){
    value1 <- enexpr(value1)
    value2 <- enexpr(value2)
    ccf <- ccf(x = eval_tidy(value1, data = .data),
               y = eval_tidy(value2, data = .data),
               plot=FALSE, ...)
    lag <- as.numeric(ccf$lag)*frequency(.data)
    tibble(lag = lag, ccf = as.numeric(ccf$acf))
  }
  value <- enexprs(...)
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
                 expr_text(value[[1]]), expr_text(value[[2]])))
  }
  if(length(value) == 1){
    abort("CCF requires two columns to be specified.")
  }
  build_cf(.data, compute_ccf, value1=!!value[[1]], value2=!!value[[2]],
           lag.max = lag_max, type = type)
}

#' @importFrom stats na.contiguous
build_cf <- function(.data, cf_fn, na.action = na.contiguous, ...){
  check_gaps(.data)
  if(is_regular(.data)){
    interval <- interval(.data)
  }
  else{
    warn("Provided data has an irregular interval, results should be treated with caution. Computing ACF by observation.")
    interval <- new_interval(unit = 1)
  }

  kv <- key_vars(.data)

  lens <- key_data(.data) %>%
    transmute(
      !!!key(.data),
      .len = map_dbl(!!sym(".rows"), length)
    )

  .data <- nest_keys(.data)
  .data[["data"]] <- map(.data[["data"]], cf_fn, na.action = na.action, ...)
  .data <- unnest_tbl(.data, "data")
  .data[["lag"]] <- as_lag(interval) * .data[["lag"]]
  new_tsibble(
    as_tsibble(.data, index = "lag", key = !!kv),
    num_obs = lens, class = "tbl_cf"
  )
}

# Temporary until generic time class is available for temporal hierarchies
as_lag <- function(x, ...) {
  UseMethod("as_lag")
}

as_lag.interval <- function(x, ...){
  new_lag(1, x)
}

as_lag.default <- function(x, ...){
  abort(
    sprintf("`as_lag()` doesn't know how to handle the '%s' class yet.",
            class(x)[1])
  )
}

new_lag <- function(x, interval){
  vctrs::new_vctr(x, interval = interval, class = "cf_lag")
}

#' @export
vec_arith.cf_lag <- function(op, x, y){
  out <- vctrs::vec_data(x)
  out <- get(op)(out, y)
  vctrs::vec_restore(out, x)
}

#' @export
format.cf_lag <- function(x, ...){
  interval <- attr(x, "interval")
  itvl_data <- if(inherits(interval, "vctrs_vctr")) vctrs::vec_data else unclass
  scale <- do.call(sum, itvl_data(interval))
  suffix <- substring(format(interval), first = nchar(format(scale)) + 1)
  paste0(scale*vec_data(x), suffix)
}

#' @export
vec_ptype_full.cf_lag <- function(x, ...){
  "lag"
}

#' @importFrom vctrs vec_ptype2
#' @method vec_ptype2 cf_lag
#' @export
vec_ptype2.cf_lag <- function(x, y, ...) UseMethod("vec_ptype2.cf_lag", y)
#' @method vec_ptype2.cf_lag default
#' @export
vec_ptype2.cf_lag.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.cf_lag double
#' @export
vec_ptype2.cf_lag.double <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  double()
}
#' @method vec_ptype2.double cf_lag
#' @export
vec_ptype2.double.cf_lag <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  double()
}

#' @importFrom vctrs vec_cast
#' @method vec_cast cf_lag
#' @export
vec_cast.cf_lag <- function(x, to, ...) UseMethod("vec_cast.cf_lag")
#' @method vec_cast.cf_lag default
#' @export
vec_cast.cf_lag.default <- function(x, to, ...) vec_default_cast(x, to)
#' @method vec_cast.cf_lag double
#' @export
vec_cast.cf_lag.double <- function(x, to, ...) vec_data(x)
#' @method vec_cast.double cf_lag
#' @export
vec_cast.double.cf_lag <- function(x, to, ...) vec_data(x)

#' @export
index_valid.cf_lag <- function(x) TRUE

#' @export
interval_pull.cf_lag <- function(x) {
  attr(x, "interval")
}

#' Auto- and Cross- Covariance and -Correlation plots
#'
#' Produces an appropriate plot for the result of  [`ACF()`], [`PACF()`], or [`CCF()`].
#'
#' @param object A tbl_cf object (the result [`ACF()`], [`PACF()`], or [`CCF()`]).
#' @param level The level of confidence for the blue dashed lines.
#' @param ... Unused.
#'
#' @return A ggplot object showing the correlations.
#'
#' @importFrom ggplot2 ggplot geom_hline xlab ylab ggtitle vars
#' @importFrom stats qnorm
#' @export
autoplot.tbl_cf <- function(object, level = 95, ...){
  cf_type <- colnames(object)[colnames(object) %in% c("acf", "pacf", "ccf")]
  plot_aes <- eval_tidy(expr(ggplot2::aes(x = !!sym("lag"), y = !!sym(cf_type))))
  interval <- interval(object)

  if(length(level) > 1){
    abort("Only one confidence interval is currently supported for this autoplot.")
  }

  p <- ggplot(object, plot_aes) +
    geom_linecol() +
    geom_hline(yintercept = 0) +
    xlab(paste0("lag [", format(interval),"]"))

  if(!is.null(level)){
    conf_int <- object%@%"num_obs" %>%
      mutate(
        upper = qnorm((1 + (level/100)) / 2) / sqrt(!!sym(".len")),
        lower = !!parse_expr("-upper")
      ) %>%
      gather("type", ".ci", !!!syms(c("upper", "lower")))
    p <- p + geom_hline(aes(yintercept = !!sym(".ci"), group = !!sym("type")),
                        data = conf_int, colour = "blue", linetype = "dashed")
  }

  if(n_keys(object) > 1){
    p <- p + facet_grid(rows = vars(!!!key(object)))
  }

  p
}

#' @importFrom ggplot2 scale_type
#' @export
scale_type.cf_lag <- function(x) c("cf_lag", "continuous")

#' lagged datetime scales
#' This set of scales defines new scales for lagged time structures.
#'
#' @param ... Further arguments to be passed on to scale_x_continuous()
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @name scale_cf_lag
#' @rdname scale_cf_lag
#' @export
scale_x_cf_lag <- function(...) {
  scale <- ggplot2::ggproto("ScaleContinuousLag", ggplot2::scale_x_continuous(...),
                   train = function (self, x){
                     if (length(x) == 0)
                       return()
                     self$range$train(x)
                     if(!is.null(gap <- attr(x, "interval"))){
                       self$interval <- gap
                       freq <- get_frequencies(NULL, gap, .auto = "all")
                       self$frequency <- min(freq[freq>3]%empty%NA)
                     }
                   },
                   get_breaks = function(self, limits = self$get_limits()) {
                     if(inherits(self$breaks, "waiver") && !is.na(self$frequency)){
                       freq <- self$frequency
                       lags <- ceiling(limits[1]):floor(limits[2])
                       # nlags <- length(lags)
                       # np <- nlags / freq
                       self$breaks <- lags[lags%%(freq/2)==0]
                     }
                     ggplot2::ggproto_parent(ggplot2::ScaleContinuous, self)$get_breaks()
                   },
                   get_labels = function(self, breaks = self$get_breaks()){
                     # if(inherits(self$labels, "waiver")){
                     #   interval_type <- map_lgl(self$interval, ~.x!=0)
                     #   self$labels <- breaks
                     #   #suffix <- toupper(names(self$interval)[interval_type])
                     #   #self$labels <- paste(breaks*as.numeric(self$interval[interval_type]), paste0(suffix, ifelse(breaks!=0, "S", "")))
                     # }
                     ggplot2::ggproto_parent(ggplot2::ScaleContinuous, self)$get_labels()
                   })
  scale
}
