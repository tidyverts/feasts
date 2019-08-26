globalVariables("self")

specials_X11 <- fabletools::new_specials(
  season = function(period = NULL){
    period <- get_frequencies(period, self$data, .auto = "smallest")
    if(!(period %in% c(4, 12))){
      abort("The X11 method only supports monthly (`period = 12`) and quarterly (`period = 4`) seasonal patterns")
    }
    period
  },

  xreg = function(...){
    abort("Exogenous regressors are not supported for X11 decompositions.")
  },

  .required_specials = "season"
)

train_X11 <- function(.data, formula, specials, type, ...){
  require_package("seasonal")
  stopifnot(is_tsibble(.data))

  if(length(specials$season) != 1){
    abort("X11 only supports one seasonal period.")
  }
  period <- specials$season[[1]]
  y <- as.ts(.data, frequency = period)

  type <- switch(type, additive = "add", multiplicative = "mult")
  op <- switch(type, add = "+", mult = "*")

  fit <- seasonal::seas(y, x11 = "", x11.mode = type,
                        transform.function = switch(type, add = "none", "log"))

  dcmp <- unclass(fit$data)

  dcmp <- .data %>%
    mutate(
      trend = dcmp[,"trend"],
      seasonal = dcmp[,"adjustfac"],
      irregular = dcmp[,"irregular"],
      season_adjust = !!call2(op, !!!syms(c("trend", "irregular")))
    )

  seasonalities <- list(
    seasonal = list(period = period, base = switch(op, `+` = 0, 1))
  )

  aliases <- list2(
    !!measured_vars(.data) := reduce(syms(c("trend", "seasonal", "irregular")),
                                     function(x,y) call2(op, x, y)),
    season_adjust = call2(op, sym("trend"), sym("irregular"))
  )

  fabletools::as_dable(dcmp, resp = !!sym(measured_vars(.data)),
                      method = "X11", seasons = seasonalities, aliases = aliases)
}

# #' X11 seasonal decomposition
# #'
# #' Applies a seasonal adjustment by an enhanced version of the methodology of the
# #' Census Bureau X-11 and X-11Q programs. The type of seasonal decomposition
# #' (additive or multiplicative) can be controlled using the `type` argument.
# #'
# #' @param .data A tsibble.
# #' @param formula Decomposition specification.
# #' @param type The type of decomposition: additive or multiplicative
# #' @param ... Other arguments passed to [seasonal::seas()].
# #'
# #' @section Specials:
# #'
# #' \subsection{season}{
# #' The `season` special is used to specify seasonal attributes of the decomposition.
# #' \preformatted{
# #' season(period = NULL)
# #' }
# #'
# #' \tabular{ll}{
# #'   `period`   \tab The periodic nature of the seasonality. X11 decompositions only support monthly (`period = 12`) and quarterly (`period = 4`) seasonal patterns.
# #' }
# #' }
# #'
# #'
# #' @examples
# #' tsibbledata::aus_production %>% feasts:::X11(Beer)
# #'
# #' @seealso [seasonal::seas()]
# #'
# #' @references
# #'
# #' Dagum, E. B., & Bianconcini, S. (2016) "Seasonal adjustment methods and real
# #' time trend-cycle estimation". \emph{Springer}.
# #'
# #' X11 Documentation from the seasonal package's website:
# #' http://www.seasonal.website/seasonal.html#input
# #'
# #' Official X-13ARIMA-SEATS manual: https://www.census.gov/ts/x13as/docX13ASHTML.pdf
# #'
# #' @importFrom fabletools new_decomposition_class new_decomposition_definition
X11 <- function(.data, formula, type = c("additive", "multiplicative"), ...){
  type <- match.arg(type)
  dcmp <- new_decomposition_class("X11",
                                  train = train_X11, specials = specials_X11,
                                  check = all_tsbl_checks)
  new_decomposition_definition(dcmp, .data, !!enquo(formula), type = type, ...)
}

