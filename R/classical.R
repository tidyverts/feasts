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

  resp <- measured_vars(.data)
  y <- .data[[resp]]
  m <- specials$season[[1]]

  dcmp_op <- switch(type,
                    additive = "+",
                    multiplicative = "*")
  dcmp_op_inv <- switch(type,
                        additive = "-",
                        multiplicative = "/")

  dcmp <- decompose(ts(y, frequency = m), type = type, ...)[c("trend", "seasonal", "random")]

  dcmp <- .data %>%
    mutate(
      !!!map(dcmp, as.numeric),
      season_adjust = !!call2(dcmp_op_inv, sym(resp), sym("seasonal"))
    )

  seasonalities <- list(
    seasonal = list(period = m, base = switch(dcmp_op, `+` = 0, 1))
  )

  aliases <- list2(
    !!resp := reduce(syms(c("trend", "seasonal", "random")),
                     function(x,y) call2(dcmp_op, x, y)),
    season_adjust = call2(dcmp_op_inv, sym(resp), sym("seasonal"))
  )

  fablelite::as_dable(dcmp, resp = !!sym(resp), method = "Classical",
                      seasons = seasonalities, aliases = aliases)
}

#' Classical Seasonal Decomposition by Moving Averages
#'
#' @inherit stats::decompose description details
#'
#' @param .data A tsibble.
#' @param formula Decomposition specification (see "Specials" section).
#' @param ... Other arguments passed to `\link[stats]{decompose}`.
#' @inheritParams stats::decompose
#'
#'
#' @section Specials:
#'
#' \subsection{season}{
#' The `season` special is used to specify seasonal attributes of the decomposition.
#' \preformatted{
#' season(period = NULL)
#' }
#'
#' \tabular{ll}{
#'   `period`   \tab The periodic nature of the seasonality. This can be either a number indicating the number of observations in each seasonal period, or text to indicate the duration of the seasonal window (for example, annual seasonality would be "1 year").
#' }
#' }
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
                                  train = train_classical, specials = specials_classical,
                                  check = all_tsbl_checks)
  new_decomposition(dcmp, .data, !!enquo(formula), type = type, ...)
}
