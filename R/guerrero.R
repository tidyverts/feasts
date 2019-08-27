# #' @param lambda Box-cox transformation parameter
# #' @param x A vector
# #' @param .period The length of each subseries (usually the length of seasonal period)
# #' @importFrom stats sd
lambda_coef_var <- function(lambda, x, .period = 2) {
  if(all(x == x[1])) return(1)
  x <- split(x, (seq_along(x)-1)%/%.period)
  mu_h <- map_dbl(x, mean, na.rm = TRUE)
  sig_h <- map_dbl(x, sd, na.rm = TRUE)
  rat <- sig_h / mu_h ^ (1 - lambda)
  sd(rat, na.rm = TRUE) / mean(rat, na.rm = TRUE)
}

#' Guerrero's method for Box Cox lambda selection
#'
#' Applies Guerrero's (1993) method to select the lambda which minimises the
#' coefficient of variation for subseries of x.
#'
#' @param x A numeric vector. The data used to identify the transformation
#' parameter lambda.
#' @param lower The lower bound for lambda.
#' @param upper The upper bound for lambda.
#' @param .period The length of each subseries (usually the length of seasonal
#' period). Subseries length must be at least 2.
#'
#' @return A Box Cox transformation parameter (lambda) chosen by Guerrero's method.
#'
#' @importFrom stats optimise
#'
#' @references
#'
#' Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations. JRSS B 26 211–246.
#'
#' Guerrero, V.M. (1993) Time-series analysis supported by power transformations. Journal of Forecasting, 12, 37–48.
#'
#' @export
guerrero <- function(x, lower = -1, upper = 2, .period = 2L) {
  c(lambda_guerrero = optimise(
    lambda_coef_var, c(lower, upper), x = x,
    .period = max(.period, 2)
  )$minimum)
}
