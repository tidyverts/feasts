#' Multiple seasonal decomposition by Loess
#'
#' @inherit forecast::mstl
#'
#' @param data A tsibble.
#' @param formula Decomposition specification.
#' @param iterations Number of iterations to use to refine the seasonal component.
#' @param ... Other arguments passed to \code{\link[forecast]{mstl}}.
#'
#' @examples
#' USAccDeaths %>% as_tsibble %>% STL(value ~ season("all") + trend(window = 10))
#'
#' @importFrom fablelite validate_model multi_univariate new_specials_env parse_model model_lhs as_dable
#' @export
STL <- function(data, formula, iterations = 2, ...){
  # Capture user call
  cl <- call_standardise(match.call())

  # Coerce data
  stopifnot(is_tsibble(data))

  formula <- validate_model(formula, data)

  # Handle multivariate inputs
  if(n_keys(data) > 1){
    return(multi_univariate(data, cl))
  }

  # Define specials
  specials <- new_specials_env(
    trend = function(window, degree, jump){
      args <- call_args(match.call())
      if(length(args > 1)){
        set_names(args, paste0("t.", names(args)))
      }
    },
    season = function(period = "all", window = 13, degree, jump){
      args <- call_args(match.call())
      args <- args[names(args)!="period"]
      if(is.null(args$window)){
        args$window <- window
      }
      args <- set_names(args, paste0("s.", names(args)))

      period <- get_frequencies(period, .data)
      period <- period[NROW(.data)/period >= 2]
      if(!is.null(period)){
        map(period, ~ c(period = .x, args))
      }
    },

    .env = caller_env(),
    .required_specials = c("trend", "season"),
    .vals = list(.data = data)
  )

  # Parse model
  model_inputs <- parse_model(data, formula, specials = specials)

  y <- eval_tidy(model_lhs(model_inputs$model), data = data)

  trend.args <- model_inputs$specials$trend[[1]]
  season.args <- unlist(model_inputs$specials$season, recursive = FALSE)

  deseas <- y
  seas <- set_names(as.list(rep(0, length(season.args))), paste0("season_", names(season.args)%||%map(season.args, "period")))
  if(length(season.args) > 0){
    for (j in seq_len(iterations))
    {
      for (i in seq_along(season.args))
      {
        deseas <- ts(deseas + seas[[i]], frequency = season.args[[i]][[1]])
        fit <- eval_tidy(expr(stl(deseas, !!!c(trend.args, season.args[[i]][-1]), ...)))
        seas[[i]] <- as.numeric(fit$time.series[, "seasonal"])
        deseas <- deseas - seas[[i]]
      }
    }
    trend <- fit$time.series[, "trend"]
  }
  else{
    trend <- stats::supsmu(seq_len(n), y)$y
  }

  decomposition <- data %>%
    select(!!!key(data), !!index(.)) %>%
    mutate(
      trend = as.numeric(trend),
      !!!seas,
      remainder = as.numeric(deseas - trend)
    )

  as_dable(decomposition,
           !!(model_inputs$response),
           !!(Reduce(function(x,y) call2("+", x, y), syms(measured_vars(decomposition))))
  )
}
