globalVariables("self")

specials_seats <- fabletools::new_specials(
  season = function(period = NULL){
    period <- get_frequencies(period, self$data, .auto = "smallest")
    if(!(period %in% c(1, 2, 4, 6, 12))){
      abort("The X-13ARIMA-SEATS method only supports seasonal patterns with seasonal periods of 1, 2, 4, 6 or 12. Defaulting to `period = 1`.")
    }
    period
  },

  xreg = function(...){
    abort("Exogenous regressors are not supported for X-13ARIMA-SEATS decompositions.")
  },

  .required_specials = "season"
)

train_seats <- function(.data, formula, specials, x11, x11.mode, ...){
  check_installed("seasonal")
  stopifnot(is_tsibble(.data))

  if(!missing(x11) || !missing(x11.mode)){
    abort("Use `X11()` to perform an X11 decomposition.")
  }

  if(length(specials$season) != 1){
    abort("X-13ARIMA-SEATS only supports one seasonal period.")
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
      season_adjust = !!sym("trend") * !!sym("irregular")
    )

  seasonalities <- list(
    seasonal = list(period = period, base = 1)
  )

  aliases <- list2(
    !!measured_vars(.data) := reduce(syms(c("trend", "seasonal", "irregular")),
                                     function(x,y) call2("*", x, y)),
    season_adjust = call2("*", sym("trend"), sym("irregular"))
  )

  structure(
    list(decomposition = dcmp,
         response = measured_vars(.data), method = "X-13ARIMA-SEATS",
         seasons = seasonalities, aliases = aliases
    ),
    class = "x13_decomposition"
  )
}

#' @importFrom fabletools components as_dable
#' @export
components.x13_decomposition <- function(object, ...){
  as_dable(object[["decomposition"]], response = !!sym(object[["response"]]),
           method = object[["method"]], seasons = object[["seasons"]],
           aliases = object[["aliases"]])
}

#' @importFrom fabletools model_sum
model_sum.x13_decomposition <- function(x){
  "SEATS"
}

SEATS <- function(formula, ...){
  lifecycle::deprecate_warn("0.2.0", "feasts::SEATS()", "feasts::X_13ARIMA_SEATS()", "You can specify the SEATS decomposition method by including seats() in the model formula of `X_13ARIMA_SEATS()`.")
  dcmp <- new_model_class("SEATS", train = train_seats,
                          specials = specials_seats, check = all_tsbl_checks)
  new_model_definition(dcmp, !!enquo(formula), ...)
}

