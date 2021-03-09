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

  structure(
    list(decomposition = dcmp,
         response = measured_vars(.data), method = "X11",
         seasons = seasonalities, aliases = aliases
    ),
    class = "x11_decomposition"
  )
}

#' @importFrom fabletools components as_dable
#' @export
components.x11_decomposition <- function(object, ...){
  as_dable(object[["decomposition"]], response = !!sym(object[["response"]]),
           method = object[["method"]], seasons = object[["seasons"]],
           aliases = object[["aliases"]])
}

#' @importFrom fabletools model_sum
model_sum.x11_decomposition <- function(x){
  "X11"
}

X11 <- function(formula, type = c("additive", "multiplicative"), ...){
  lifecycle::deprecate_warn("0.2.0", "feasts::X11()", "feasts::X_13ARIMA_SEATS()", "You can specify the X-11 decomposition method by including x11() in the model formula of `X_13ARIMA_SEATS()`.")
  type <- match.arg(type)
  dcmp <- new_model_class("X11",
                          train = train_X11, specials = specials_X11,
                          check = all_tsbl_checks)
  new_model_definition(dcmp, !!enquo(formula), type = type, ...)
}

