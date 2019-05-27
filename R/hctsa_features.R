#' CompEngine feature set
#'
#' Calculate the features that have been used in CompEngine database, using method introduced in package
#' \code{hctsa}.
#'
#' The features involved can be grouped as \code{autocorrelation},
#' \code{prediction}, \code{stationarity}, \code{distribution}, and \code{scaling}.
#'
#' @param x a vector
#' @param ... Unused.
#'
#' @return a vector with CompEngine features
#' @seealso \code{\link{autocorr_features}}
#' @seealso \code{\link{pred_features}}
#' @seealso \code{\link{stationarity_features}}
#' @seealso \code{\link{distribution_features}}
#' @seealso \code{\link{scal_features}}
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
compengine_features <- function(x, ...) {
  c(autocorr_features(x), pred_features(x), stationarity_features(x), distribution_features(x), scal_features(x))
}

#' The autocorrelation feature set from software package \code{hctsa}
#'
#' Calculate the features that grouped as autocorrelation set,
#' which have been used in CompEngine database, using method introduced in package \code{hctsa}.
#'
#' Features in this set are \code{embed2_incircle_1},
#' \code{embed2_incircle_2},
#' \code{ac_9},
#' \code{firstmin_ac},
#' \code{trev_num},
#' \code{motiftwo_entro3},
#' and \code{walker_propcross}.
#'
#' @inheritParams compengine_features
#'
#' @return a vector with autocorrelation features
#' @seealso \code{\link{embed2_incircle}}
#' @seealso \code{\link{firstmin_ac}}
#' @seealso \code{\link{trev_num}}
#' @seealso \code{\link{motiftwo_entro3}}
#' @seealso \code{\link{walker_propcross}}
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
autocorr_features <- function(x, ...) {
  acfv <- stats::acf(x, length(x) - 1, plot = FALSE, na.action = stats::na.pass)
  output <- c(
    embed2_incircle(x, 1, acfv = acfv),
    embed2_incircle(x, 2, acfv = acfv),
    firstmin_ac(x, acfv),
    trev_num(x),
    motiftwo_entro3(x),
    walker_propcross(x)
  )
  return(output)
}


#' The prediction feature set from software package \code{hctsa}
#'
#' Calculate the features that grouped as prediction set,
#' which have been used in CompEngine database, using method introduced in package \code{hctsa}.
#'
#' Features in this set are \code{localsimple_mean_ac},
#' \code{localsimple_lfit_ac},
#' and \code{sampen_first}.
#'
#' @inheritParams compengine_features
#'
#' @return a vector with autocorrelation features
#' @seealso \code{\link{localsimple_taures}}
#' @seealso \code{\link{sampen_first}}
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
pred_features <- function(x, ...) {
  output <- c(
    localsimple_taures(x, "mean"),
    localsimple_taures(x, "lfit"),
    sampen_first(x)
  )
  return(output)
}


#' The stationarity feature set from software package \code{hctsa}
#'
#' Calculate the features that grouped as stationarity set,
#' which have been used in CompEngine database, using method introduced in package \code{hctsa}.
#'
#' Features in this set are \code{sd_deriv_1},
#' \code{spreadrandomlocal_meantaul_50},
#' and \code{spreadrandomlocal_meantaul_ac2}.
#'
#' @inheritParams compengine_features
#'
#' @return a vector with autocorrelation features
#' @seealso \code{\link{sd_deriv_1}}
#' @seealso \code{\link{bootstrap_stationarity}}
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
stationarity_features <- function(x, ...) {
  output <- c(
    sd_deriv_1(x),
    bootstrap_stationarity(x, 50),
    bootstrap_stationarity(x, "ac2")
  )
  return(output)
}

#' The distribution feature set from software package \code{hctsa}
#'
#' Calculate the features that grouped as distribution set,
#' which have been used in CompEngine database, using method introduced in package \code{hctsa}.
#'
#' Features in this set are \code{histogram_mode_10}
#' and \code{outlierinclude_mdrmd}.
#'
#' @inheritParams compengine_features
#'
#' @return a vector with autocorrelation features
#' @seealso \code{\link{histogram_mode}}
#' @seealso \code{\link{outlier_include}}
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
distribution_features <- function(x, ...) {
  output <- c(
    histogram_mode(x),
    outlier_include(x)
  )
  return(output)
}

#' The scaling feature set from software package \code{hctsa}
#'
#' Calculate the features that grouped as scaling set,
#' which have been used in CompEngine database, using method introduced in package \code{hctsa}.
#'
#' Feature in this set is \code{fluctuation_analysis}.
#'
#' @inheritParams compengine_features
#'
#' @return a vector with autocorrelation features
#' @seealso \code{\link{fluctuation_analysis}}
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
scal_features <- function(x, ...) {
  output <- c(fluctuation_analysis(x))
  return(output)
}

# CO_Embed2_Basic_tau_incircle_1
#' Points inside a given circular boundary in a 2-d embedding space from software package \code{hctsa}
#'
#' The time lag is set to the first zero crossing of the autocorrelation function.
#'
#' @inheritParams compengine_features
#' @param boundary the given circular boundary, setting to 1 or 2 in CompEngine. Default to 1.
#' @param acfv vector of autocorrelation, if exist, used to avoid repeated computation.
#'
#' @return the proportion of points inside a given circular boundary
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
embed2_incircle <- function(x, boundary = NULL, acfv = stats::acf(x, length(x) - 1, plot = FALSE, na.action = stats::na.pass), ...) {
  if (is.null(boundary)) {
    warn("`embed2_incircle()` using `boundary = 1`. Set value with `boundary`.")
    boundary <- 1
  }
  tau <- firstzero_ac(x, acfv)
  xt <- x[1:(length(x) - tau)] # part of the time series
  xtp <- x[(1 + tau):length(x)] # time-lagged time series
  N <- length(x) - tau # Length of each time series subsegment

  # CIRCLES (points inside a given circular boundary)
  set_names(sum(xtp^2 + xt^2 < boundary, na.rm = TRUE) / N, paste0("embed2_incircle_", boundary))
}

# CO_firstzero_ac
#' The first zero crossing of the autocorrelation function from software package \code{hctsa}
#'
#' Search up to a maximum of the length of the time series
#'
#' @inheritParams embed2_incircle
#'
#' @return The first zero crossing of the autocorrelation function
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
firstzero_ac <- function(x, acfv = stats::acf(x, N - 1, plot = FALSE, na.action = stats::na.pass), ...) {
  N <- length(x)
  tau <- which(acfv$acf[-1] < 0)
  if(length(tau)==0L) # Nothing to see here
    c(first_zero_ac = 0)
  else if(all(is.na(tau))) # All missing
    c(first_zero_ac = 0)
  else if(!any(tau))  # No negatives, so set output to sample size
    c(first_zero_ac = N)
  else # Return lag of first negative
    c(first_zero_ac = tau[1])
}

# CO_firstmin_ac
#' Time of first minimum in the autocorrelation function from software package \code{hctsa}
#'
#' @inheritParams firstzero_ac
#'
#' @return The lag of the first minimum
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @examples
#' firstmin_ac(WWWusage)
#'
#' @export
firstmin_ac <- function(x, acfv = stats::acf(x, lag.max = N - 1, plot = FALSE, na.action = stats::na.pass), ...) {
  # hctsa uses autocorr in MatLab to calculate autocorrelation
  N <- length(x)
  # getting acf for all lags
  # possible delay when sample size is too big
  autoCorr <- numeric(N - 1)
  autoCorr[1:(N - 1)] <- acfv$acf[-1]
  for (i in 1:length(autoCorr)) {
    if (is.na(autoCorr[i])) {
      warning("No minimum was found.")
      return(c(firstmin_ac = NA))
    }
    if (i == 2 && autoCorr[2] > autoCorr[1]) {
      return(c(firstmin_ac = 1))
    } else if (i > 2 && autoCorr[i - 2] > autoCorr[i - 1] && autoCorr[i - 1] < autoCorr[i]) {
      return(c(firstmin_ac = i - 1))
    }
  }
  c(firstmin_ac = N - 1)
}

# CO_trev_1_num
#' Normalized nonlinear autocorrelation, the numerator of the trev function of a time series from software package \code{hctsa}
#'
#' Calculates the numerator of the trev function, a normalized nonlinear autocorrelation,
#' The time lag is set to 1.
#'
#' @inheritParams embed2_incircle
#'
#' @return the numerator of the trev function of a time series
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @examples
#' trev_num(WWWusage)
#'
#' @export
trev_num <- function(x, ...) {
  yn <- x[1:(length(x) - 1)]
  yn1 <- x[2:length(x)]
  c(trev_num = mean((yn1 - yn)^3, na.rm = TRUE))
}

# SB_MotifTwo_mean_hhh
#' Local motifs in a binary symbolization of the time series from software package \code{hctsa}
#'
#'
#' Coarse-graining is performed. Time-series values above its mean are given 1,
#' and those below the mean are 0.
#'
#' @inheritParams trev_num
#'
#' @return Entropy of words in the binary alphabet of length 3.
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @examples
#' motiftwo_entro3(WWWusage)
#'
#' @export
motiftwo_entro3 <- function(x, ...) {
  yBin <- binarize_mean(x)
  N <- length(yBin)
  if (N < 5) warning("Time series too short")

  r1 <- yBin == 1
  r0 <- yBin == 0

  r1 <- r1[1:(length(r1) - 1)]
  r0 <- r0[1:(length(r0) - 1)]

  r00 <- r0 & yBin[2:N] == 0
  r01 <- r0 & yBin[2:N] == 1
  r10 <- r1 & yBin[2:N] == 0
  r11 <- r1 & yBin[2:N] == 1

  r00 <- r00[1:(length(r00) - 1)]
  r01 <- r01[1:(length(r01) - 1)]
  r10 <- r10[1:(length(r10) - 1)]
  r11 <- r11[1:(length(r11) - 1)]

  r000 <- r00 & yBin[3:N] == 0
  r001 <- r00 & yBin[3:N] == 1
  r010 <- r01 & yBin[3:N] == 0
  r011 <- r01 & yBin[3:N] == 1
  r100 <- r10 & yBin[3:N] == 0
  r101 <- r10 & yBin[3:N] == 1
  r110 <- r11 & yBin[3:N] == 0
  r111 <- r11 & yBin[3:N] == 1

  out.ddd <- mean(r000)
  out.ddu <- mean(r001)
  out.dud <- mean(r010)
  out.duu <- mean(r011)
  out.udd <- mean(r100)
  out.udu <- mean(r101)
  out.uud <- mean(r110)
  out.uuu <- mean(r111)
  ppp <- c(out.ddd, out.ddu, out.dud, out.duu, out.udd, out.udu, out.uud, out.uuu)
  c(motiftwo_entro3 = f_entropy(ppp))
}

f_entropy <- function(x) {
  # entropy of a set of counts, log(0)=0
  -sum(x[x > 0] * log(x[x > 0]))
}

# BF_BF_binarize_mean
#' Converts an input vector into a binarized version from software package \code{hctsa}
#'
#' @inheritParams trev_num
#'
#' @return Time-series values above its mean are given 1, and those below the mean are 0.
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#' @export
binarize_mean <- function(x, ...) {
  x <- x - mean(x)
  Y <- numeric(length(x))
  Y[x > 0] <- 1
  c(binary_mean = Y)
}

# PH_Walker_prop_01_sw_propcross
#' Simulates a hypothetical walker moving through the time domain from software package \code{hctsa}
#'
#' The hypothetical particle (or 'walker') moves in response to values of the
#' time series at each point.
#' The walker narrows the gap between its value and that
#' of the time series by 10%.
#'
#' @inheritParams trev_num
#'
#' @return fraction of time series length that walker crosses time series
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#' @export
walker_propcross <- function(x, ...) {
  N <- length(x)
  p <- 0.1
  #   walker starts at zero and narrows the gap between its position
  #   and the time series value at that point by 0.1, to give the value at the subsequent time step
  w <- numeric(N)
  w[1] <- 0 # start at zero
  for (i in 2:N) {
    w[i] <- w[i - 1] + p * (x[i - 1] - w[i - 1])
  }
  out.sw_propcross <- sum((w[1:(N - 1)] - x[1:(N - 1)]) * (w[2:N] - x[2:N]) < 0, na.rm = TRUE) / (N - 1)
  c(walker_propcross = out.sw_propcross)
}

# FC_localsimple_mean1_taures
# FC_localsimple_lfit_taures
#' The first zero crossing of the autocorrelation function of the residuals from simple local time-series forecasting from software package \code{hctsa}
#'
#' Simple predictors using the past `train_length` values of the time series to
#' predict its next value.
#'
#' @inheritParams pred_features
#' @param fc_method the forecasting method, default to \code{mean}.
#' \code{mean}: local mean prediction using the past `train_length` values.
#' \code{lfit}: local linear prediction using the past `train_length` values.
#' @param train_length the number of values to use to forecast the next value.
#' Default to 1 when using method \code{mean} and the first zero auto-correlation
#' (via [`firstzero_ac()`] when using method \code{lfit}.
#'
#' @return The first zero crossing of the autocorrelation function of the residuals
#'
#' @export
localsimple_taures <- function(x, fc_method = c("mean", "lfit"), train_length = NULL, ...) {
  fc_method <- match.arg(fc_method)
  if(is.null(train_length)){
    train_length <- switch(fc_method, mean = 1, lfit = firstzero_ac(x))
  }

  if (train_length >= length(x))
    abort("Time series too short for forecasting in `localsimple_taures`")

  evalr <- (train_length + 1):length(x)

  res <- numeric(length(evalr))
  if (fc_method == "mean") {
    res <- lag(tsibble::slide_dbl(x, mean, .size = train_length), train_length) - x
  }
  if (fc_method == "lfit") {
    xreg <- matrix(c(rep(1, train_length), seq_len(train_length)), ncol = 2)
    res <- lag(tsibble::slide_dbl(x, .size = train_length, function(x){
      sum(stats::.lm.fit(xreg, x)$coefficients * c(1, train_length + 1))
    }), 1) - x
  }

  set_names(firstzero_ac(res), paste0("localsimple_", fc_method, "_ac"))
}


# EN_SampEn_5_03_sampen1
#' Second Sample Entropy of a time series from software package \code{hctsa}
#'
#' Modified from the Ben Fulcher's \code{EN_SampEn} which uses code from PhysioNet.
#' The publicly-available PhysioNet Matlab code, sampenc (renamed here to
#' RN_sampenc) is available from:
#' http://www.physionet.org/physiotools/sampen/matlab/1.1/sampenc.m
#'
#' Embedding dimension is set to 5.
#' The threshold is set to 0.3.
#'
#' @inheritParams sampenc
#'
#' @references cf. "Physiological time-series analysis using approximate entropy and sample
#' entropy", J. S. Richman and J. R. Moorman, Am. J. Physiol. Heart Circ.
#' Physiol., 278(6) H2039 (2000)
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
sampen_first <- function(x, ...) {
  M <- 5
  r <- 0.3
  c(sampen_first = sampenc(x, M + 1, r))
}



# PN_sampenc
#' Second Sample Entropy from software package \code{hctsa}
#'
#' Modified from the Ben Fulcher version of original code sampenc.m from
#' http://physionet.org/physiotools/sampen/
#' http://www.physionet.org/physiotools/sampen/matlab/1.1/sampenc.m
#' Code by DK Lake (dlake@virginia.edu), JR Moorman and Cao Hanqing.
#'
#' @inheritParams pred_features
#' @param M embedding dimension
#' @param r threshold
#'
#' @references cf. "Physiological time-series analysis using approximate entropy and sample
#' entropy", J. S. Richman and J. R. Moorman, Am. J. Physiol. Heart Circ.
#' Physiol., 278(6) H2039 (2000)
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
sampenc <- function(x, M = 6, r = 0.3, ...) {
  N <- length(x)
  lastrun <- numeric(N) # zeros(1,N)
  run <- numeric(N) # zeros(1,N)
  A <- numeric(M) # zeros(M,1)
  B <- numeric(M) # zeros(M,1)
  # Get counting:
  for (i in 1:(N - 1)) { # go through each point in the time series, counting matches
    y1 <- x[i]
    for (jj in 1:(N - i)) { # compare to points through the rest of the time series
      # Compare to future index, j:
      j <- i + jj
      # This future point, j, matches the time-series value at i:
      if (isTRUE(abs(x[j] - y1) < r)) {
        run[jj] <- lastrun[jj] + 1 # increase run count for this lag
        M1 <- min(M, run[jj])

        A[1:M1] <- A[1:M1] + 1
        if (j < N) B[1:M1] <- B[1:M1] + 1
      } else {
        run[jj] <- 0
      }
    }
    for (j in 1:(N - i)) {
      lastrun[j] <- run[j]
    }
  }
  # Calculate for m <- 2
  # NN <- N*(N-1)/2
  p <- A[2] / B[1]
  e <- -log(p)
  return(e)
}

# SY_StdNthDer_1
#' Standard deviation of the first derivative of the time series from software package \code{hctsa}
#'
#' Modified from \code{SY_StdNthDer} in \code{hctsa}. Based on an idea by Vladimir Vassilevsky.
#'
#' @inheritParams stationarity_features
#'
#' @return Standard deviation of the first derivative of the time series.
#' @references cf. http://www.mathworks.de/matlabcentral/newsreader/view_thread/136539
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
sd_deriv_1 <- function(x, ...) {
  if (length(x) < 2) stop("Time series is too short to compute differences")
  c(sd_deriv_1 = sd(diff(x), na.rm = TRUE))
}

# SY_SpreadRandomLocal_50_100_meantaul
# SY_SpreadRandomLocal_ac2_100_meantaul
#'  Bootstrap-based stationarity measure from software package \code{hctsa}
#'
#' 100 time-series segments of length \code{l} are selected at random from the time series and
#' the mean of the first zero-crossings of the autocorrelation function in each segment is calculated.
#'
#' @inheritParams stationarity_features
#' @param segment_length the length of local time-series segments to analyse as a positive integer. Can also be a specified character string: "ac2": twice the first zero-crossing of the autocorrelation function
#'
#' @return mean of the first zero-crossings of the autocorrelation function
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
bootstrap_stationarity <- function(x, segment_length = 50, ...) {
  seg_input <- segment_length
  if (segment_length == "ac2") segment_length <- 2 * firstzero_ac(x)
  if (!is.numeric(segment_length)) abort("`segment_length` must be either 'ac2' or an integer.")

  numSegs <- 100
  N <- length(x)
  if (segment_length > 0.9 * N) stop("This input vector is too short. Specify proper segment length in `segment_length`")

  qs <- numeric(numSegs)

  for (j in 1:numSegs) {
    # pick a range
    # in this implementation, ranges CAN overlap
    ist <- sample(N - 1 - segment_length, 1) # random start point (not exceeding the endpoint)
    ifh <- ist + segment_length - 1 # finish index
    rs <- ist:ifh # sample range (from starting to finishing index)
    ysub <- x[rs] # subsection of the time series
    taul <- firstzero_ac(ysub)
    qs[j] <- taul
  }
  set_names(mean(qs, na.rm = TRUE), paste0("bootstrap_stationarity_", seg_input))
}


# distribution ------------------------------------------------------------

# DN_histogram_mode_10
#' Mode of a data vector from software package \code{hctsa}
#'
#' Measures the mode of the data vector binned by the specified number of bins.
#' The value calculated is different from \code{hctsa} and \code{CompEngine} as the histogram edges are calculated differently.
#'
#' @inheritParams distribution_features
#' @param bins the number of bins to use.
#'
#' @return the mode
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#'
#' @export
histogram_mode <- function(x, bins = 10, ...) {
  # Compute the histogram from the data:
  if (!is.numeric(bins)) {
    abort("The number of bins for `histogram_mode()` must be a number.")
  }
  breaks <- pretty(range(x), n = bins, min.n = 1)
  mids <- 0.5 * (breaks[-1L] + breaks[-length(breaks)])

  # Mean position of maximums (if multiple):
  c(histogram_mode = mean(mids[which.max(table(cut(x, breaks)))]))
}

# DN_OutlierInclude_abs_001_mdrmd
#' How median depend on distributional outliers from software package \code{hctsa}
#'
#' Measures median as more and more outliers are included in the calculation
#' according to a specified rule, of outliers being furthest from the mean.
#'
#' The threshold for including time-series data points in the analysis increases
#' from zero to the maximum deviation, in increments of 0.01*sigma (by default),
#' where sigma is the standard deviation of the time series.
#'
#' At each threshold, proportion of time series points included and median are
#' calculated, and outputs from the algorithm measure how these statistical
#' quantities change as more extreme points are included in the calculation.
#'
#' Outliers are defined as furthest from the mean.
#'
#' @inheritParams distribution_features
#' @param scale Should the data be scaled before computing the statistic.
#'
#' @return median of the median of range indices
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
#' @importFrom stats ts tsp sd
outlier_include <- function(x, scale = TRUE, ...) {
  if (identical(x, rep(x[1], length(x)))) {
    stop("The vector is constant!")
  }
  if (scale) {
    x <- scale(x)
    isd <- 1
  } else {
    isd <- stats::sd(x, na.rm = TRUE) # Modified to fit the 0.01*sigma increment in description
  }
  inc <- 0.01 * isd
  # inc <- 0.01
  thr <- seq(from = 0, to = max(abs(x), na.rm = TRUE), by = inc)
  if (length(thr) == 0) stop("peculiar time series")

  N <- length(x)

  tot <- N

  msDt <- numeric(length(thr))
  msDtp <- numeric(length(thr))
  for (i in 1:length(thr)) {
    th <- thr[i] # the threshold
    # Construct a time series consisting of inter-event intervals for parts
    # of the time serie exceeding the threshold, th
    r <- which(abs(x) >= th)

    Dt_exc <- diff(r) # Delta t (interval) time series exceeding threshold
    msDt[i] <- median(r) / (tot / 2) - 1
    msDtp[i] <- length(Dt_exc) / tot * 100
    # this is just really measuring the distribution:
    # the proportion of possible values
    # that are actually used in
    # calculation
  }

  # Trim off where the statistic power is lacking: less than 2% of data
  # included
  trimthr <- 2 # percent
  mj <- which(msDtp > trimthr)[length(which(msDtp > trimthr))]
  if (length(mj) != 0) {
    msDt <- msDt[1:mj]
    msDtp <- msDtp[1:mj]
    thr <- thr[1:mj]
  } else {
    stop("the statistic power is lacking: less than 2% of data included")
  }

  c(outlier_include = median(msDt))
}

# SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#' Implements fluctuation analysis from software package \code{hctsa}
#'
#' Fits a polynomial of order 1 and then returns the
#' range. The order of fluctuations is 2, corresponding to root mean
#' square fluctuations.
#'
#' @inheritParams distribution_features
#'
#' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
#' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
#' @author Yangzhuoran Yang
#'
#' @export
fluctuation_analysis <- function(x, ...) {
  q <- 2
  tauStep <- 50
  k <- 1

  N <- length(x)
  x_NA0 <- ifelse(!is.na(x), x, 0)

  y <- cumsum(x_NA0)
  taur <- unique(round(exp(seq(from = log(5), to = log(floor(N / 2)), length.out = tauStep))))
  ntau <- length(taur)
  if (ntau < 8) { # fewer than 8 points
    stop("This time series is too short to analyse using this fluctuation analysis")
  }

  Fl <- numeric(ntau)

  for (i in 1:ntau) {
    # buffer the time series at the scale tau
    tau <- taur[i] # the scale on which to compute fluctuations
    y_buff <- split(y, ceiling(seq_along(y) / tau))

    if (length(y_buff) > floor(N / tau)) { # zero-padded, remove trailing set of points...
      y_buff <- y_buff[-length(y_buff)]
    }

    # analysed length of time series (with trailing end-points removed)
    nn <- length(y_buff) * tau
    tt <- (1:tau) # faux time range

    for (j in 1:length(y_buff)) {
      # fit a polynomial of order k in each subsegment
      lm.tt <- lm(lmy ~ tt, data = data.frame(tt, lmy = y_buff[[j]]))
      # remove the trend, store back in y_buff
      y_buff[[j]] <- stats::residuals(lm.tt)
    }

    tem <- sapply(y_buff, range)
    y_dt <- tem[2, ] - tem[1, ]

    # Compute fluctuation function:

    Fl[i] <- (mean(y_dt^q))^(1 / q)
  }
  logtt <- log(taur)
  logFF <- log(Fl)
  ntt <- ntau


  ## Try assuming two components (2 distinct scaling regimes)
  # Move through, and fit a straight line to loglog before and after each point.
  # Find point with the minimum sum of squared errors
  # First spline interpolate to get an even sampling of the interval
  # (currently, in the log scale, there are relatively more at large scales
  # Determine the errors
  sserr <- rep(NA, ntt) # don't choose the end points
  minPoints <- 6
  for (i in minPoints:(ntt - minPoints)) {
    r1 <- 1:i
    # p1 <- polyfit(logtt(r1),logFF(r1),1)
    p1 <- lm(y ~ x, data = data.frame(x = logtt[r1], y = logFF[r1]))
    r2 <- i:ntt
    # p2 <- polyfit(logtt(r2),logFF(r2),1)
    p2 <- lm(y ~ x, data = data.frame(x = logtt[r2], y = logFF[r2]))
    # Sum of errors from fitting lines to both segments:
    sserr[i] <- norm(-stats::residuals(p1), type = "2") + norm(-stats::residuals(p2), type = "2")
  }

  # breakPt is the point where it's best to fit a line before and another line after
  breakPt <- which.min(sserr)
  r1 <- 1:breakPt
  r2 <- breakPt:ntt

  prop_r1 <- length(r1) / ntt
  c(fluctuation = prop_r1)
}
