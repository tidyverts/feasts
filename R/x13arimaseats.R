globalVariables("self")

x13_valid_args <- function(...) {
  list(...)
}

specials_x13arimaseats <- fabletools::new_specials(
  transform = x13_valid_args,
  x11 = x13_valid_args,
  x11regression = x13_valid_args,
  seats = x13_valid_args,
  force = x13_valid_args,
  automdl = x13_valid_args,
  pickmdl = x13_valid_args,
  arima = x13_valid_args,
  regression = x13_valid_args,
  estimate = x13_valid_args,
  check = x13_valid_args,
  forecast = x13_valid_args,
  outlier = x13_valid_args,
  xreg = function(...){
    abort("Exogenous regressors are not yet supported.")
  },
  .required_specials = NULL
)

train_x13arimaseats <- function(.data, formula, specials, ...){
  require_package("seasonal")
  stopifnot(is_tsibble(.data))
  series_name <- measured_vars(.data)

  specials <- lapply(specials, do.call, what = "c")
  specification <- unlist(specials, recursive = FALSE)

  fit <- seasonal::seas(
    x = as.ts(.data),
    list = c(specification, list(...))
  )
  fit$call <- NULL
  fit$spc$series$title <- series_name

  structure(
    list(fit = fit, response = series_name, index = index_var(.data)),
    class = "feasts_x13arimaseats"
  )
}

#' @importFrom fabletools components as_dable
#' @export
components.feasts_x13arimaseats <- function(object, ...){
  fit <- object$fit
  period <- as.integer(fit$udg["freq"])
  .data <- as_tsibble(fit$x)
  colnames(.data) <- c(object$index, object$response)
  dcmp <- unclass(fit$data)
  if(is.null(dcmp)){
    abort("The X-13ARIMA-SEATS model does not contain a decomposition, are you missing a seasonal component?")
  }
  op <- switch(fit$udg["finmode"], multiplicative = "*", additive = "+")

  dcmp <- .data %>%
    mutate(
      trend = dcmp[,"trend"],
      seasonal = dcmp[,"adjustfac"],
      irregular = dcmp[,"irregular"],
      season_adjust = !!call2(op, sym("trend"), sym("irregular"))
    )

  seasonalities <- list(
    seasonal = list(period = period, base = switch(op, `+` = 0, 1))
  )

  aliases <- list2(
    !!object$response := reduce(syms(c("trend", "seasonal", "irregular")),
                                     function(x,y) call2(op, x, y)),
    season_adjust = call2(op, sym("trend"), sym("irregular"))
  )

  as_dable(dcmp, response = object$response,
           method = "X-13ARIMA-SEATS", seasons = seasonalities,
           aliases = aliases)
}

#' @importFrom fabletools model_sum
#' @export
model_sum.feasts_x13arimaseats <- function(x){
  "X-13ARIMA-SEATS"
}

#' Seasonal decomposition with X-13ARIMA-SEATS
#'
#' Decomposes a time series via model based signal extraction using X-13ARIMA-SEATS.
#'
#' @param .data A tsibble.
#' @param formula Decomposition specification.
#' @param ... Other arguments passed to [seasonal::seas()].
#'
#' @section Specials:
#'
#' \subsection{season}{
#' The `season` special is used to specify seasonal adjustment method for the
#' decomposition.
#' \preformatted{
#' season(period = NULL)
#' }
#'
#' \tabular{ll}{
#'   `period`   \tab The periodic nature of the seasonality. X-13ARIMA-SEATS decompositions only seasonal patterns with periods of 1, 2, 4, 6 or 12.
#' }
#' }
#'
#' @examples
#' tsibbledata::aus_production %>%
#'   model(X_13ARIMA_SEATS(Beer)
#'
#' @seealso [seasonal::seas()]
#'
#' @references
#'
#' Dagum, E. B., & Bianconcini, S. (2016) "Seasonal adjustment methods and real
#' time trend-cycle estimation". \emph{Springer}.
#'
#' X-13ARIMA-SEATS Documentation from the seasonal package's website:
#' http://www.seasonal.website/seasonal.html
#'
#' Official X-13ARIMA-SEATS manual: https://www.census.gov/ts/x13as/docX13ASHTML.pdf
#'
#' @importFrom fabletools new_model_class new_model_definition
X_13ARIMA_SEATS <- function(formula, ..., na.action = seasonal::na.x13){
  dcmp <- new_model_class("x13arimaseats",
                          train = train_x13arimaseats,
                          specials = specials_x13arimaseats,
                          check = all_tsbl_checks)
  new_model_definition(dcmp, !!enquo(formula), na.action = na.action, ...)
}

#' @export
report.feasts_x13arimaseats <- function(x){
  cat(capture.output(summary(x$fit))[-(1:3)], sep = "\n")
}
