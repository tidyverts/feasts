#' @inherit forecast::ggseasonplot
#'
#' @param x A time series object
#' @param ... Additional arguments passed to methods
#'
#' @examples
#' tsibbledata::ausretail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   ggseasonplot(Turnover)
#'
#' @rdname ggseasonplot
#' @export
ggseasonplot <- function(x, ...){
  UseMethod("ggseasonplot")
}

#' @param var The variable to plot (a bare expression). If NULL, it will
#' automatically selected from the data.
#' @param period The seasonal period to display
#' @param facet_period A secondary seasonal period to facet by
#' (typically smaller than period)
#' @rdname ggseasonplot
#' @importFrom ggplot2 ggplot aes geom_line
#' @export
ggseasonplot.tbl_ts <- function(x, var = NULL, period = "largest",
                                facet_period = NULL, ...){
  if(quo_is_null(enquo(var))){
    inform(sprintf(
      "Plot variable not specified, automatically selected `var = %s`",
      measured_vars(x)[1]
    ))
    var <- sym(measured_vars(x)[1])
  }
  else{
    var <- enexpr(var)
  }

  check_gaps(x)
  idx <- index(x)

  period <- get_frequencies(period, x)
  if(period <= 1){
    abort("The data must contain at least one observation per seasonal period.")
  }
  period <- period*time_unit(interval(x))

  if(!is.null(facet_period)){
    facet_period <- get_frequencies(facet_period, x)
    if(facet_period <= 1){
      abort("The data must contain at least one observation per seasonal period.")
    }
    facet_period <- facet_period*time_unit(interval(x))
  }

  x <- as_tibble(x) %>%
    mutate(
      id = units_since(!!idx) %/% period,
      facet_id = units_since(!!idx) %/% facet_period %empty% NA,
      !!as_string(idx) := !!idx - period * (units_since(!!idx) %/% period)
    )

  p <- ggplot(x, aes(x = !!idx, y = !!var, group = factor(!!sym("id")))) +
    geom_line()

  if(!is.null(facet_period)){
    p <- p + facet_wrap(~ facet_id)
  }

  p
}

#' @inherit forecast::ggsubseriesplot
#'
#' @inheritParams ggseasonplot
#'
#' @examples
#' tsibbledata::ausretail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   ggsubseriesplot(Turnover)
#'
#' @rdname ggsubseriesplot
#' @export
ggsubseriesplot <- function(x, ...){
  UseMethod("ggsubseriesplot")
}

#' @inheritParams ggseasonplot.tbl_ts
#' @rdname ggsubseriesplot
#' @importFrom ggplot2 ggplot aes geom_line geom_hline facet_grid
#' @export
ggsubseriesplot.tbl_ts <- function(x, var = NULL, period = "smallest", ...){
  if(quo_is_null(enquo(var))){
    inform(sprintf(
      "Plot variable not specified, automatically selected `var = %s`",
      measured_vars(x)[1]
    ))
    var <- sym(measured_vars(x)[1])
  }
  else{
    var <- enexpr(var)
  }

  check_gaps(x)
  idx <- index(x)

  period <- get_frequencies(period, x)
  if(period <= 1){
    abort("The data must contain at least one observation per seasonal period.")
  }
  period_units <- period*time_unit(interval(x))

  x <- as_tibble(x) %>%
    mutate(
      id = !!idx - period_units*(units_since(!!idx)%/%period_units)
    ) %>%
    group_by(id) %>%
    mutate(.yint = mean(!!var))

  ggplot(x, aes(x = !!idx, y = !!var)) +
    geom_line() +
    facet_grid(~ id) +
    geom_hline(aes(yintercept = !!sym(".yint")), colour = "blue")
}


#' @inheritParams ggseasonplot
#' @inherit forecast::gglagplot
#'
#' @examples
#' tsibbledata::ausretail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   gglagplot(Turnover)
#'
#' @rdname gglagplot
#' @export
gglagplot <- function(x, ...){
  UseMethod("gglagplot")
}

#' @inheritParams ggseasonplot.tbl_ts
#' @param lags A vector of lags to display as facets.
#' @rdname gglagplot
#' @importFrom ggplot2 ggplot aes geom_path geom_abline facet_wrap
#' @export
gglagplot.tbl_ts <- function(x, var = NULL, period = "smallest", lags = 1:16, ...){
  if(quo_is_null(enquo(var))){
    inform(sprintf(
      "Plot variable not specified, automatically selected `var = %s`",
      measured_vars(x)[1]
    ))
    var <- sym(measured_vars(x)[1])
  }
  else{
    var <- enexpr(var)
  }

  period <- get_frequencies(period, x)
  if(period <= 1){
    abort("The data must contain at least one observation per seasonal period.")
  }
  period_units <- period*time_unit(interval(x))

  lag_exprs <- map(lags, function(lag) expr(lag(!!var, !!lag))) %>%
    set_names(paste0(".lag_", lags))

  idx <- index(x)

  x <- x %>%
    as_tibble %>%
    mutate(
      season = factor(!!idx - period_units*(units_since(!!idx)%/%period_units)),
      !!!lag_exprs) %>%
    gather(".lag", ".value", !!names(lag_exprs)) %>%
    mutate(.lag = factor(.lag, levels = names(lag_exprs), labels = paste("lag", lags)))

  x %>%
    ggplot(aes(x = !!var, y = !!sym(".value"), colour = !!sym("season"))) +
    geom_abline(colour = "gray", linetype = "dashed") +
    geom_path() +
    facet_wrap(~ .lag)
}
