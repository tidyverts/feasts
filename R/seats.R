globalVariables("self")

specials_seats <- fablelite::new_specials(
  season = function(period = NULL){
    period <- get_frequencies(period, self$data, .auto = "smallest")
    if(!(period %in% c(1, 2, 4, 6, 12))){
      warning("The SEATS method only supports seasonal patterns with seasonal periods of 1, 2, 4, 6 or 12. Defaulting to `period = 1`.")
      period <- 1
    }
    period
  },

  xreg = function(...){
    abort("Exogenous regressors are not supported for SEATS decompositions.")
  },

  .required_specials = "season"
)

train_seats <- function(.data, formula, specials, x11, x11.mode, ...){
  require_package("seasonal")
  stopifnot(is_tsibble(.data))

  if(length(specials$season) != 1){
    abort("SEATS only supports one seasonal period.")
  }
  period <- specials$season[[1]]
  y <- as.ts(.data, frequency = period)

  fit <- seasonal::seas(y, ...)

  dcmp <- unclass(fit$data)

  dcmp <- .data %>%
    mutate(
      trend = dcmp[,"trend"],
      seasonal = dcmp[,"adjustfac"],
      irregular = dcmp[,"irregular"],
      seas_adjust = !!sym("trend") * !!sym("irregular")
    )

  seasonalities <- list(
    seasonal = list(period = period, base = 1)
  )

  aliases <- list2(
    !!measured_vars(.data) := reduce(syms(c("trend", "seasonal", "irregular")),
                                     function(x,y) call2("*", x, y)),
    seas_adjust = call2("*", sym("trend"), sym("irregular"))
  )

  fablelite::as_dable(dcmp, resp = !!sym(measured_vars(.data)),
                      method = "SEATS", seasons = seasonalities, aliases = aliases)
}

#' Seasonal Extraction in ARIMA Time Series
#'
#' Decomposes a time series via model based signal extraction using SEATS:
#' a seasonal adjustment program developed by Victor Gomez and Agustin Maravall
#' at the Bank of Spain
#'
#' @param .data A tsibble.
#' @param formula Decomposition specification.
#' @param ... Other arguments passed to [seasonal::seas()].
#'
#' @examples
#' tsibbledata::aus_production %>% SEATS(Beer)
#'
#' @seealso [seasonal::seas()]
#'
#' @references
#'
#' Dagum, E. B., & Bianconcini, S. (2016) "Seasonal adjustment methods and real
#' time trend-cycle estimation". \emph{Springer}.
#'
#' SEATS Documentation from the seasonal package's website:
#' http://www.seasonal.website/seasonal.html
#'
#' Official X-13ARIMA-SEATS manual: https://www.census.gov/ts/x13as/docX13ASHTML.pdf
#'
#' @importFrom fablelite new_decomposition_class new_decomposition
#' @export
SEATS <- function(.data, formula, ...){
  dcmp <- new_decomposition_class("SEATS", train = train_seats, specials = specials_seats)
  new_decomposition(dcmp, .data, !!enquo(formula), ...)
}

