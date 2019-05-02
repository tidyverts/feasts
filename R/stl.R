globalVariables("self")

specials_stl <- fablelite::new_specials(
  trend = function(window, degree, jump){
    args <- map(call_args(match.call()), eval_tidy)
    if(isFALSE(is.finite(args$window)) && sign(args$window) == 1){
      args$window <- NROW(self$data) + 1e7
    }
    if(length(args) > 0){
      set_names(args, paste0("t.", names(args)))
    }
  },
  season = function(period = NULL, window = 13, degree, jump){
    args <- map(call_args(match.call()), eval_tidy)
    args <- args[names(args)!="period"]
    if(is.null(args$window)){
      args$window <- window
    }
    if(isFALSE(is.finite(args$window)) && is.numeric(args$window)){
      if(sign(args$window) == 1){
        args$window <- "periodic"
      }
    }
    args <- set_names(args, paste0("s.", names(args)))

    period <- get_frequencies(period, self$data, .auto = "all")
    period <- period[NROW(self$data)/period >= 2]
    if(!is.null(period)){
      map(period, function(.x) c(period = .x, args))
    }
  },
  lowpass = function(window, degree, jump){
    args <- map(call_args(match.call()), eval_tidy)
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
  seas <- set_names(as.list(rep(0, length(season.args))), paste0("season_", names(season.args)%||%map(season.args, function(x) x[["period"]])))
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
    trend <- stats::supsmu(seq_len(length(y)), y)$y
  }

  trend <- as.numeric(trend)
  deseas <- as.numeric(deseas)
  list2(trend = trend, !!!seas, remainder = deseas - trend, seas_adjust = deseas)
}

train_stl <- function(.data, formula, specials, iterations = 2, ...){
  stopifnot(is_tsibble(.data))

  y <- .data[[measured_vars(.data)]]

  trend.args <- specials$trend[[1]]
  season.args <- unlist(specials$season, recursive = FALSE)
  lowpass.args <- specials$lowpass[[1]]

  decomposition <- .data %>%
    mutate(!!!estimate_stl(y, trend.args, season.args, lowpass.args, ...))

  seas_cols <- paste0("season_", names(season.args)%||%map(season.args, "period"))
  seasonalities <- lapply(season.args, function(x){
    x["base"] <- 0
    x[c("period", "base")]
  })
  names(seasonalities) <- seas_cols

  aliases <- list2(
    !!measured_vars(.data) := reduce(syms(c("trend", seas_cols, "remainder")),
                                   function(x,y) call2("+", x, y)),
    seas_adjust = call2("+", sym("trend"), sym("remainder"))
  )

  fablelite::as_dable(decomposition, resp = !!sym(measured_vars(.data)),
                      method = "STL", seasons = seasonalities, aliases = aliases)
}

#' Multiple seasonal decomposition by Loess
#'
#' @inherit forecast::mstl
#'
#' @inheritParams classical_decomposition
#' @param iterations Number of iterations to use to refine the seasonal component.
#' @param ... Other arguments passed to [stats::stl()].
#'
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
#' season(period = NULL, window = 13, degree, jump)
#' }
#'
#' \tabular{ll}{
#'   `period` \tab The periodic nature of the seasonality. This can be either a number indicating the number of observations in each seasonal period, or text to indicate the duration of the seasonal window (for example, annual seasonality would be "1 year").\cr
#'   `window` \tab The span (in lags) of the loess window, which should be odd. If the `window` is set to `"periodic"` or `Inf`, the seasonal pattern will be fixed. The window size should be odd and at least 7, according to Cleveland et al. \cr
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
#' USAccDeaths %>% as_tsibble %>% STL(value ~ trend(window = 10))
#'
#' @importFrom stats ts stl
#' @importFrom fablelite new_decomposition_class new_decomposition
#' @export
STL <- function(.data, formula, iterations = 2, ...){
  dcmp <- new_decomposition_class("STL", train = train_stl, specials = specials_stl)
  new_decomposition(dcmp, .data, !!enquo(formula), iterations = iterations, ...)
}

