# This R script contains code for extracting the Box-Cox
# parameter, lambda, using Guerrero's method (1993).
# Written by Leanne Chhay
# Adapted for usage with features() by Mitchell O'Hara-Wild

# guer.cv computes the coefficient of variation
# Input:
# 	lam = lambda
# 	x = original time series as a time series object
# Output: coefficient of variation

guer.cv <- function(lam, x, .period = 2) {
  if(all(x == x[1])) return(1)
  nobsf <- length(x)
  nyr <- floor(nobsf / .period)
  nobst <- floor(nyr * .period)
  x.mat <- matrix(x[(nobsf - nobst + 1):nobsf], .period, nyr)
  x.mean <- apply(x.mat, 2, mean, na.rm = TRUE)
  x.sd <- apply(x.mat, 2, sd, na.rm = TRUE)
  x.rat <- x.sd / x.mean ^ (1 - lam)
  res <- sd(x.rat, na.rm = TRUE) / mean(x.rat, na.rm = TRUE)
  res
}

#' Guerrero's method for Box Cox lambda selection
#'
#' Applies Guerrero's (1993) method to select the lambda which minimises the
#' coefficient of variation for subseries x.
#'
#' @param x A numeric vector. The data used to identify the transformation
#' parameter lambda.
#' @param lower The lower bound for lambda.
#' @param upper The upper bound for lambda.
#' @param .period The seasonal period of the time series.
#'
#' @export
guerrero <- function(x, lower=-1, upper=2, .period) {
  lambda <- optimize(
    guer.cv, c(lower, upper), x = x,
    .period = max(2, .period)
  )$minimum
  c(lambda_guerrero = lambda)
}
