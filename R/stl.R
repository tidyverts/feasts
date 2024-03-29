globalVariables("self")

specials_stl <- fabletools::new_specials(
  trend = function(window, degree, jump){
    args <- as.list(environment())
    args <- args[!map_lgl(args, rlang::is_missing)]

    if(isFALSE(is.finite(args$window)) && sign(args$window) == 1){
      args$window <- NROW(self$data) + 1e7
    }
    if(length(args) > 0){
      set_names(args, paste0("t.", names(args)))
    }
  },
  season = function(period = NULL, window = NULL, degree, jump){
    args <- as.list(environment())
    args <- args[!map_lgl(args, rlang::is_missing)]
    args <- args[names(args)!="period"]

    if(isFALSE(is.finite(args$window)) && is.numeric(args$window)){
      if(sign(args$window) == 1){
        args$window <- "periodic"
      }
    }
    args <- set_names(args, paste0("s.", names(args)))

    nm <- period
    period <- get_frequencies(period, self$data, .auto = "all")
    if(is.null(names(period))) names(period) <- nm
    period <- period[NROW(self$data)/period >= 2]
    if(!is.null(period)){
      map(period, function(.x) c(period = .x, args))
    }
  },
  lowpass = function(window, degree, jump){
    args <- as.list(environment())
    args <- args[!map_lgl(args, rlang::is_missing)]

    if(isFALSE(is.finite(args$window)) && sign(args$window) == 1){
      args$window <- NROW(self$data) + 1e7
    }
    if(length(args) > 0){
      set_names(args, paste0("l.", names(args)))
    }
  },

  .required_specials = c("trend", "season", "lowpass")
)

estimate_stl <- function(y, trend.args, season.args, lowpass.args,
                         iterations = 2, ...){
  if(any(is.na(y))){
    abort("STL decomposition does not support series with missing values.")
  }
  deseas <- y
  season.args <- Filter(function(x) x[["period"]] > 1, season.args)
  period <- map_dbl(season.args, function(x) x[["period"]])
  season.args <- map2(season.args, 7 + 4*order(period),
                      function(x, default_window){
                        x$s.window <- x$s.window %||% default_window
                        x
                      })
  season.args <- season.args[order(period)]
  seas <- set_names(as.list(rep(0, length(season.args))),
                    sprintf("season_%s", names(season.args)%||%map(season.args, function(x) x[["period"]])))
  if(length(season.args) > 0){
    for (j in seq_len(iterations))
    {
      for (i in seq_along(season.args))
      {
        deseas <- ts(deseas + seas[[i]], frequency = season.args[[i]][[1]])
        fit <- eval_tidy(expr(stl(deseas, !!!c(trend.args, season.args[[i]][-1], lowpass.args), ...)))
        seas[[i]] <- as.numeric(fit$time.series[, "seasonal"])
        deseas <- deseas - seas[[i]]
      }
    }
    trend <- fit$time.series[, "trend"]
  }
  else{
    trend <- stats::supsmu(seq_along(y), y)$y
  }

  trend <- as.numeric(trend)
  deseas <- as.numeric(deseas)
  list2(trend = trend, !!!seas, remainder = deseas - trend, season_adjust = deseas)
}

train_stl <- function(.data, formula, specials, iterations = 2, ...){
  stopifnot(is_tsibble(.data))

  y <- .data[[measured_vars(.data)]]

  trend.args <- specials$trend[[1]]
  season.args <- unlist(specials$season, recursive = FALSE)
  season.args <- Filter(function(x) x[["period"]] > 1, season.args)
  lowpass.args <- specials$lowpass[[1]]

  decomposition <- .data %>%
    mutate(!!!estimate_stl(y, trend.args, season.args, lowpass.args, ...))

  seas_cols <- sprintf("season_%s", names(season.args)%||%map(season.args, function(x) x[["period"]]))
  seasonalities <- lapply(season.args, function(x){
    x["base"] <- 0
    x[c("period", "base")]
  })
  names(seasonalities) <- seas_cols

  aliases <- list2(
    !!measured_vars(.data) := reduce(syms(c("trend", seas_cols, "remainder")),
                                   function(x,y) call2("+", x, y)),
    season_adjust = call2("+", sym("trend"), sym("remainder"))
  )

  structure(
    list(decomposition = decomposition,
         response = measured_vars(.data), method = "STL",
         seasons = seasonalities, aliases = aliases
    ),
    class = "stl_decomposition"
  )
}

#' @importFrom fabletools components as_dable
#' @export
components.stl_decomposition <- function(object, ...){
  as_dable(object[["decomposition"]], response = !!sym(object[["response"]]),
           method = object[["method"]], seasons = object[["seasons"]],
           aliases = object[["aliases"]])
}

#' @export
fitted.stl_decomposition <- function(object, ...) {
  object[["decomposition"]][[object[["response"]]]] - residuals(object)
}

#' @importFrom stats residuals
#' @export
residuals.stl_decomposition <- function(object, ...) {
  object[["decomposition"]][["remainder"]]
}

MBB <- function (x, window_size) {
  bx <- array(0, (floor(length(x)/window_size) + 2) * window_size)
  for (i in 1:(floor(length(x)/window_size) + 2)) {
    c <- sample(1:(length(x) - window_size + 1), 1)
    bx[((i - 1) * window_size + 1):(i * window_size)] <- x[c:(c + window_size - 1)]
  }
  start_from <- sample(0:(window_size - 1), 1) + 1
  bx[start_from:(start_from + length(x) - 1)]
}

#' Generate block bootstrapped series from an STL decomposition
#'
#' Produces new data with the same structure by resampling the residuals using
#' a block bootstrap procedure. This method can only generate within sample, and
#' any generated data out of the trained sample will produce NA simulations.
#'
#' @inheritParams fable::generate.ARIMA
#'
#' @examples
#' as_tsibble(USAccDeaths) %>%
#'   model(STL(log(value))) %>%
#'   generate(as_tsibble(USAccDeaths), times = 3)
#'
#' @references
#' Bergmeir, C., R. J. Hyndman, and J. M. Benitez (2016). Bagging Exponential Smoothing Methods using STL Decomposition and Box-Cox Transformation. International Journal of Forecasting 32, 303-312.
#'
#' @importFrom fabletools generate
#'
#' @export
generate.stl_decomposition <- function(x, new_data, specials = NULL, ...){
  dcmp <- x$decomposition

  # Match new_data index with dcmp index
  pos <- vec_match(new_data[[index_var(new_data)]], dcmp[[index_var(dcmp)]])

  if(!(".innov" %in% names(new_data))){
    # Block bootstrap for each replicate
    kr <- tsibble::key_rows(new_data)

    # Get default bootstrap params
    period <- max(vapply(x$seasons, `[[`, double(1L), "period"))
    block_size <- ifelse(period > 1, 2 * period, min(8, floor(length(x)/2)))

    # Block bootstrap
    innov <- lapply(seq_along(kr), function(...) MBB(dcmp[["remainder"]], block_size))
    innov <- mapply(function(i, e) e[pos[i]], kr, innov, SIMPLIFY = FALSE)
    new_data$.innov <- vec_c(!!!innov)
  }

  dcmp <- as_tibble(dcmp)[pos,]
  dcmp$remainder <- new_data$.innov

  new_data[[".sim"]] <- as.numeric(eval_tidy(x$aliases[[x$response]], dcmp))
  new_data[[".innov"]] <- NULL
  new_data
}

#' @importFrom fabletools model_sum
#' @export
model_sum.stl_decomposition <- function(x){
  "STL"
}

#' Multiple seasonal decomposition by Loess
#'
#' @inherit forecast::mstl
#'
#' @inheritParams classical_decomposition
#' @param iterations Number of iterations to use to refine the seasonal component.
#' @param ... Other arguments passed to [stats::stl()].
#'
#' @return A [`fabletools::dable()`] containing the decomposed trend, seasonality
#' and remainder from the STL decomposition.
#'
#' @section Specials:
#'
#' \subsection{trend}{
#' The `trend` special is used to specify the trend extraction parameters.
#' \preformatted{
#' trend(window, degree, jump)
#' }
#'
#' \tabular{ll}{
#'   `window` \tab The span (in lags) of the loess window, which should be odd. If NULL, the default, nextodd(ceiling((1.5*period) / (1-(1.5/s.window)))), is taken.\cr
#'   `degree` \tab The degree of locally-fitted polynomial. Should be zero or one. \cr
#'   `jump`   \tab Integers at least one to increase speed of the respective smoother. Linear interpolation happens between every `jump`th value.
#' }
#' }
#'
#' \subsection{season}{
#' The `season` special is used to specify the season extraction parameters.
#' \preformatted{
#' season(period = NULL, window = NULL, degree, jump)
#' }
#'
#' \tabular{ll}{
#'   `period` \tab The periodic nature of the seasonality. This can be either a number indicating the number of observations in each seasonal period, or text to indicate the duration of the seasonal window (for example, annual seasonality would be "1 year").\cr
#'   `window` \tab The span (in lags) of the loess window, which should be odd. If the `window` is set to `"periodic"` or `Inf`, the seasonal pattern will be fixed. The window size should be odd and at least 7, according to Cleveland et al. The default (NULL) will choose an appropriate default, for a dataset with one seasonal pattern this would be 11, the second larger seasonal window would be 15, then 19, 23, ... onwards.\cr
#'   `degree` \tab The degree of locally-fitted polynomial. Should be zero or one. \cr
#'   `jump`   \tab Integers at least one to increase speed of the respective smoother. Linear interpolation happens between every `jump`th value.
#' }
#' }
#'
#' \subsection{lowpass}{
#' The `lowpass` special is used to specify the low-pass filter parameters.
#' \preformatted{
#' lowpass(window, degree, jump)
#' }
#'
#' \tabular{ll}{
#'   `window` \tab The span (in lags) of the loess window of the low-pass filter used for each subseries. Defaults to the smallest odd integer greater than or equal to the seasonal `period` which is recommended since it prevents competition between the trend and seasonal components. If not an odd integer its given value is increased to the next odd one. \cr
#'   `degree` \tab The degree of locally-fitted polynomial. Must be zero or one. \cr
#'   `jump`   \tab Integers at least one to increase speed of the respective smoother. Linear interpolation happens between every `jump`th value.
#' }
#' }
#'
#' @references
#' R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990) STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal of Official Statistics, 6, 3–73.
#'
#' @examples
#' as_tsibble(USAccDeaths) %>%
#'   model(STL(value ~ trend(window = 10))) %>%
#'   components()
#'
#' @importFrom stats ts stl
#' @importFrom fabletools new_model_class new_model_definition
#' @export
STL <- function(formula, iterations = 2, ...){
  dcmp <- new_model_class("STL",
                          train = train_stl, specials = specials_stl,
                          check = all_tsbl_checks)
  new_model_definition(dcmp, !!enquo(formula), iterations = iterations, ...)
}

