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
      map(period, function(.x) c(period = .x, args))
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

  trend <- as.numeric(trend)
  deseas <- as.numeric(deseas)
  decomposition <- .data %>%
    mutate(
      trend = trend,
      !!!seas,
      remainder = deseas - trend,
      seas_adjust = deseas
    )

  seasonalities <- lapply(season.args, function(x){
    x["base"] <- 0
    x[c("period", "base")]
  })

  aliases <- list2(
    !!measured_vars(.data) := reduce(syms(c("trend", names(seas), "remainder")),
                                   function(x,y) call2("+", x, y)),
    seas_adjust = call2("+", sym("trend"), sym("remainder"))
  )

  names(seasonalities) <- names(seas)

  fablelite::as_dable(decomposition, resp = !!sym(measured_vars(.data)),
                      method = "STL", seasons = seasonalities, aliases = aliases)
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
#' @param .data A tsibble.
#' @param formula Decomposition specification.
#' @param iterations Number of iterations to use to refine the seasonal component.
#' @param ... Other arguments passed to \code{\link[forecast]{mstl}}.
#'
#' @examples
#' USAccDeaths %>% as_tsibble %>% STL(value ~ season("all") + trend(window = 10))
#'
#' @importFrom stats ts stl
#' @export
STL <- fablelite::new_decomposition(stl_decomposition)
