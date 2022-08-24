
#' Guerrero's method for Box Cox lambda selection
#'
#' Applies Guerrero's (1993) method to select the lambda which minimises the
#' coefficient of variation for subseries of x.
#'
#' Note that this function will give slightly different results to
#' `forecast::BoxCox.lambda(y)` if your data does not start at the start of the
#' seasonal period. This function will make use of all of your data, whereas the
#' forecast package will not use data that doesn't complete a seasonal period.
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
guerrero <- function(x, lower = -0.9, upper = 2, .period = 2L) {
  if(all(x == x[1])) return(1)

  # Prepare seasonal subseries
  n_obs <- length(x)
  n_subseries <- floor(n_obs/.period)
  x <- matrix(
    x[(n_obs - floor(n_subseries * .period) + 1):n_obs],
    nrow = .period, ncol = n_subseries
  )

  c(lambda_guerrero = optimise(
    lambda_coef_var, c(lower, upper), x = x,
    .period = max(.period, 2)
  )$minimum)
}

# #' @param lambda Box-cox transformation parameter
# #' @param x A vector
# #' @param .period The length of each subseries (usually the length of seasonal period)
#' @importFrom stats sd
lambda_coef_var <- function(lambda, x, .period = 2) {
  # Mean of each subseries
  mu_h <- apply(x, 2, mean, na.rm = TRUE)
  # Standard deviation of each subseries
  sig_h <- apply(x, 2, sd, na.rm = TRUE)
  # Compute fitness metric
  rat <- sig_h / mu_h ^ (1 - lambda)
  sd(rat, na.rm = TRUE) / mean(rat, na.rm = TRUE)
}
