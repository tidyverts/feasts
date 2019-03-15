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

# guerrero extracts the required lambda
# Input: x = original time series as a time series object
# Output: lambda that minimises the coefficient of variation

guerrero <- function(x, lower=-1, upper=2, .period) {
  lambda <- optimize(
    guer.cv, c(lower, upper), x = x,
    .period = max(2, .period)
  )$minimum
  c(lambda_guerrero = lambda)
}
