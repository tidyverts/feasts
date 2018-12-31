globalVariables("self")

specials_stl <- fablelite::new_specials(
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

    period <- get_frequencies(period, self$data)
    period <- period[NROW(self$data)/period >= 2]
    if(!is.null(period)){
      map(period, ~ c(period = .x, args))
    }
  },

  .required_specials = c("trend", "season")
)

train_stl <- function(.data, formula, specials, iterations = 2, ...){
  stopifnot(is_tsibble(.data))

  y <- .data[[measured_vars(.data)]]

  trend.args <- specials$trend[[1]]
  season.args <- unlist(specials$season, recursive = FALSE)

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
    trend <- stats::supsmu(seq_len(length(y)), y)$y
  }

  decomposition <- .data %>%
    select(!!!key(.data), !!index(.)) %>%
    mutate(
      trend = as.numeric(trend),
      !!!seas,
      remainder = as.numeric(deseas - trend)
    )

  as_dable(decomposition,
           !!sym(measured_vars(.data)),
           !!(Reduce(function(x,y) call2("+", x, y), syms(measured_vars(decomposition))))
  )
}

stl_decomposition <- R6::R6Class(NULL,
                                 inherit = fablelite::decomposition_definition,
                                 public = list(
                                   method = "STL",
                                   train = train_stl,
                                   specials = specials_stl
                                 )
)

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
#' @importFrom fablelite model_lhs as_dable
#' @importFrom stats ts stl
#' @export
STL <- function(data, formula, iterations = 2, ...){
  keys <- key(data)
  dcmp <- stl_decomposition$new(!!enquo(formula), iterations = iterations, ...)
  fablelite::validate_formula(dcmp, data)
  data <- nest(group_by(data, !!!keys), .key = "lst_data")

  eval_dcmp <- function(lst_data){
    map(lst_data, function(data){
      dcmp$data <- data
      parsed <- fablelite::parse_model(dcmp)
      data <- transmute(data, !!model_lhs(parsed$model))
      eval_tidy(
        expr(dcmp$train(.data = data, formula = dcmp$formula,
                        specials = parsed$specials, !!!dcmp$extra))
      )
    })
  }

  out <- mutate(data,
         dcmp = eval_dcmp(lst_data)
  )

  dcmp <- map(out[["dcmp"]], function(x) x%@%"dcmp")
  resp <- map(out[["dcmp"]], function(x) x%@%"resp")
  if(length(resp <- unique(resp)) > 1){
    abort("Decomposition response variables must be the same for all models.")
  }
  if(length(dcmp <- unique(dcmp)) > 1){
    warn("Batch decompositions contain different components. Using decomposition with most variables.")
    vars <- map(dcmp, all.vars)
    dcmp <- dcmp[[which.max(map_dbl(vars, length))]]
  }
  else{
    dcmp <- dcmp[[1]]
  }

  out <- unnest(out, !!sym("dcmp"), key = keys)

  as_dable(out, resp = !!resp[[1]], dcmp = !!dcmp)
}

