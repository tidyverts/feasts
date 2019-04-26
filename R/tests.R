#' Portmanteau tests
#'
#' Compute the Box–Pierce or Ljung–Box test statistic for examining the null hypothesis of independence in a given time series. These are sometimes known as ‘portmanteau’ tests.
#'
#' @param x A numeric vector
#' @param lag The number of lag autocorrelation coefficients to use in calculating the statistic
#' @param dof Degrees of freedom of the fitted model (useful if x is a series of residuals).
#' @param  ... Unused.
#'
#' @seealso [stats::Box.test()]
#'
#' @examples
#' ljung_box(rnorm(100))
#'
#' @rdname portmanteau_tests
#' @export
ljung_box <- function(x, lag = 1, dof = 0, ...){
  out <- stats::Box.test(x, lag = lag, type = "Ljung-Box", fitdf = dof)
  c(lb_stat = unname(out$statistic), lb_pval = out$p.value)
}

#' @rdname portmanteau_tests
#' @examples
#' box_pierce(rnorm(100))
#' @export
box_pierce <- function(x, lag = 1, dof = 0, ...){
  out <- stats::Box.test(x, lag = lag, type = "Box-Pierce", fitdf = dof)
  c(bp_stat = unname(out$statistic), bp_pval = out$p.value)
}

#' @rdname portmanteau_tests
#' @export
portmanteau_tests <- list(ljung_box, box_pierce)
