specials_classical <- fablelite::new_specials(
  season = function(period = "smallest"){
    period <- get_frequencies(period, self$data)

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

  dcmp <- decompose(ts(y, frequency = m), type = type, ...)[c("trend", "seasonal", "random")]

  dcmp <- .data %>%
    select(!!!key(.data), !!index(.)) %>%
    mutate(
      !!!map(dcmp, as.numeric)
    )

  dcmp_op <- switch(type,
                    additive = "+",
                    multiplicative = "*")

  fablelite::as_dable(dcmp,
           !!sym(measured_vars(.data)),
           !!(Reduce(function(x,y) call2(dcmp_op, x, y), syms(measured_vars(dcmp))))
  )
}

classical_decomposition_def <- R6::R6Class(NULL,
                                 inherit = fablelite::decomposition_definition,
                                 public = list(
                                   method = "Classical decomposition",
                                   train = train_classical,
                                   specials = specials_classical
                                 )
)

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
#'   classical_decomposition(value ~ season("smallest"))
#'
#' USAccDeaths %>%
#'   as_tsibble %>%
#'   classical_decomposition(value ~ season(12), type = "mult")
#'
#' @importFrom stats ts decompose
#' @export
classical_decomposition <- fablelite::new_decomposition(classical_decomposition_def)
