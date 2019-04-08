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
#' @param ... The column(s) from the tsibble used to compute the ACF, PACF or CCF.
#' @param lag_max maximum lag at which to calculate the acf. Default is 10*log10(N/m)
#' where N is the number of observations and m the number of series. Will be
#' automatically limited to one less than the number of observations in the series.
#' @inheritParams stats::acf
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
#' library(tsibble)
#' library(tsibbledata)
#'
#' aus_elec %>% ACF(Temperature)
#'
#' @importFrom tibble tibble
#' @importFrom stats as.ts frequency
#' @importFrom utils tail
#' @importFrom fablelite get_frequencies
#'
#' @rdname ACF
#' @export
ACF <- function(.data, ..., lag_max = NULL, demean = TRUE,
                type = c("correlation", "covariance", "partial")){
  compute_acf <- function(.data, value, ...){
    value <- enexpr(value)
    x <- as.ts(transmute(.data, !!value))
    acf <- tail(as.numeric(acf(x, plot=FALSE, ...)$acf), -1)
    tibble(lag = seq_along(acf), acf = acf)
  }
  value <- enexprs(...)
  if(length(value) == 0){
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
#' aus_elec %>% PACF(Temperature)
#'
#' @export
PACF <- function(.data, ..., lag_max = NULL){
  compute_pacf <- function(.data, value, ...){
    value <- enexpr(value)
    x <- as.ts(transmute(.data, !!value))
    pacf <- as.numeric(pacf(x, plot=FALSE, ...)$acf)
    tibble(lag = seq_along(pacf), pacf = pacf)
  }
  value <- enexprs(...)
  if(length(value) == 0){
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
#' @export
CCF <- function(.data, ..., lag_max = NULL, type = c("correlation", "covariance")){
  compute_ccf <- function(.data, value1, value2, ...){
    value1 <- enexpr(value1)
    value2 <- enexpr(value2)
    ccf <- ccf(x = as.ts(transmute(.data, !!value1)),
               y = as.ts(transmute(.data, !!value2)),
               plot=FALSE, ...)
    lag <- as.numeric(ccf$lag)*frequency(.data)
    tibble(lag = lag, ccf = as.numeric(ccf$acf))
  }
  value <- enexprs(...)
  if(length(value) == 0){
    if(length(measured_vars(.data) < 2)){
      abort("CCF requires two columns specified.")
    }
    inform(sprintf(
      "Response variable not specified, automatically selected `%s` and `%s",
      measured_vars(.data)[1], measured_vars(.data)[2]
    ))
    value <- syms(measured_vars(.data)[1:2])
  }
  if(length(value) > 2){
    warn(sprintf("CCF currently only supports two column, `%s` and `%s` will be used.",
                 expr_text(value[[1]]), expr_text(value[[2]])))
  }
  if(length(value) == 1){
    abort("CCF requires two columns specified.")
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

  lens <- key_data(.data) %>%
    transmute(
      !!!key(.data),
      .len = map_dbl(!!sym(".rows"), length)
    )

  .data %>%
    group_by(!!!syms(key_vars(.data))) %>%
    nest %>%
    mutate(data = map(!!sym("data"), cf_fn, na.action = na.action, ...)) %>%
    unnest(!!sym("data")) %>%
    mutate(lag = as_lag(!!sym("lag"), interval = interval)) %>%
    as_tsibble(index = !!sym("lag"), key = key_vars(.data)) %>%
    new_tsibble(num_obs = lens, class = "tbl_cf")
}

#' @export
type_sum.lag <- function(x){
  "lag"
}

#' @export
obj_sum.lag <- function(x){
  rep("lag", length(x))
}

pillar_shaft.lag <- function(x, ...) {
  require_package("pillar")
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
  x %>% map_chr(function(.x){
    format(add_class(map(attr(x, "interval"), `*`, .x), "interval"))
  })
}

#' @export
print.lag <- function(x, ...){
  print(format(x, ...), quote = FALSE)
  invisible(x)
}

#' @importFrom tibble is_vector_s3
#' @export
is_vector_s3.lag <- function(x) {
  TRUE
}

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

#' @export
index_valid.lag <- function(x) TRUE

#' @export
interval_pull.lag <- function(x) attr(x, "interval")

#' @importFrom ggplot2 scale_type
#' @export
scale_type.lag <- function(x) c("lag", "continuous")

#' lagged datetime scales
#' This set of scales defines new scales for lagged time structures.
#'
#' @param ... Further arguments to be passed on to scale_x_continuous()
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @name scale_lag
#' @rdname scale_lag
#' @export
scale_x_lag <- function(...) {
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
