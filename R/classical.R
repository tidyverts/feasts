#' Classical Seasonal Decomposition by Moving Averages
#'
#' @inherit stats::decompose description details
#'
#' @param data A tsibble.
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
#' @importFrom fablelite validate_model multi_univariate new_specials_env parse_model model_lhs as_dable
#' @importFrom stats ts decompose
#' @export
classical_decomposition <- function(data, formula, type = c("additive", "multiplicative"), ...){
  # Capture user call
  cl <- call_standardise(match.call())

  # Coerce data
  stopifnot(is_tsibble(data))

  formula <- validate_model(formula, data)

  # Handle multivariate inputs
  if(n_keys(data) > 1){
    return(multi_univariate(data, cl))
  }

  type <- match.arg(type)

  # Define specials
  specials <- new_specials_env(
    season = function(period = "smallest"){
      period <- get_frequencies(period, .data)

      if(length(period) > 1){
        warn("Multiple seasonal decomposition is not supported by classical decomposition")
      }
      period[[1]]
    },

    .env = caller_env(),
    .required_specials = c("season"),
    .vals = list(.data = data)
  )

  # Parse model
  model_inputs <- parse_model(data, formula, specials = specials)

  y <- eval_tidy(model_lhs(model_inputs$model), data = data)
  m <- model_inputs$specials$season[[1]]

  dcmp <- decompose(ts(y, frequency = m), type = type, ...)

  dcmp <- data %>%
    select(!!!key(data), !!index(.)) %>%
    mutate(
      !!!dcmp[c("trend", "seasonal", "random")],
    )

  dcmp_op <- switch(type,
                    additive = "+",
                    multiplicative = "*")

  as_dable(dcmp,
           !!(model_inputs$response),
           !!(Reduce(function(x,y) call2(dcmp_op, x, y), syms(measured_vars(dcmp))))
  )
}
