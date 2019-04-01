specials_classical <- fablelite::new_specials(
  season = function(period = NULL){
    period <- get_frequencies(period, self$data, .auto = "smallest")

    if(length(period) > 1){
      warn("Multiple seasonal decomposition is not supported by classical decomposition")
    }
    period[[1]]
  },
  .required_specials = c("season")
)

train_classical <- function(.data, formula, specials,
                            type = c("additive", "multiplicative"), ...){
  stopifnot(is_tsibble(.data))

  type <- match.arg(type)

  y <- .data[[measured_vars(.data)]]
  m <- specials$season[[1]]

  dcmp_op <- switch(type,
                    additive = "+",
                    multiplicative = "*")

  dcmp <- decompose(ts(y, frequency = m), type = type, ...)[c("trend", "seasonal", "random")]

  dcmp <- .data %>%
    mutate(
      !!!map(dcmp, as.numeric),
      seas_adjust = !!call2(dcmp_op, sym("trend"), sym("random"))
    )

  seasonalities <- list(
    seasonal = list(period = m, base = switch(dcmp_op, `+` = 0, 1))
  )

  aliases <- list2(
    !!measured_vars(.data) := reduce(syms(c("trend", "seasonal", "random")),
                                     function(x,y) call2(dcmp_op, x, y)),
    seas_adjust = call2(dcmp_op, sym("trend"), sym("random"))
  )

  fablelite::as_dable(dcmp, resp = !!sym(measured_vars(.data)), method = "Classical",
                      seasons = seasonalities, aliases = aliases)
}

#' Classical Seasonal Decomposition by Moving Averages
#'
#' @inherit stats::decompose description details
#'
#' @param .data A tsibble.
#' @param formula Decomposition specification.
#' @param ... Other arguments passed to `\link[stats]{decompose}`.
#' @inheritParams stats::decompose
#'
#' @examples
#' USAccDeaths %>%
#'   as_tsibble %>%
#'   classical_decomposition(value)
#'
#' USAccDeaths %>%
#'   as_tsibble %>%
#'   classical_decomposition(value ~ season(12), type = "mult")
#'
#' @importFrom stats ts decompose
#' @importFrom fablelite new_decomposition_class new_decomposition
#' @export
classical_decomposition <- function(.data, formula, type = c("additive", "multiplicative"), ...){
  dcmp <- new_decomposition_class("Classical decomposition",
                                  train = train_classical, specials = specials_classical)
  new_decomposition(dcmp, .data, !!enquo(formula), type = type, ...)
}
