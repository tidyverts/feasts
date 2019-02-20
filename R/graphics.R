guess_plot_var <- function(x, var){
  if(quo_is_null(enquo(var))){
    inform(sprintf(
      "Plot variable not specified, automatically selected `var = %s`",
      measured_vars(x)[1]
    ))
    sym(measured_vars(x)[1])
  }
  else{
    get_expr(enexpr(var))
  }
}

#' @inherit forecast::ggseasonplot
#'
#' @param x A time series object
#' @param ... Additional arguments passed to methods
#'
#' @examples
#' library(tsibble)
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
  var <- guess_plot_var(x, !!enquo(var))

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
    group_by(
      facet_id = time_identifier(!!idx, facet_period) %empty% NA
    ) %>%
    mutate(
      id = time_identifier(!!idx, period),
      !!as_string(idx) := !!idx - period * (units_since(!!idx) %/% period)
    )

  p <- ggplot(x, aes(x = !!idx, y = !!var, group = !!sym("id"))) +
    geom_line()

  if(!is.null(facet_period)){
    p <- p + facet_grid(~ facet_id, scales = "free_x")
  }

  p
}

#' @inherit forecast::ggsubseriesplot
#'
#' @inheritParams ggseasonplot
#'
#' @examples
#' library(tsibble)
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
  var <- guess_plot_var(x, !!enquo(var))

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
#' library(tsibble)
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
  var <- guess_plot_var(x, !!enquo(var))

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
    mutate(.lag = factor(!!sym(".lag"), levels = names(lag_exprs), labels = paste("lag", lags)))

  x %>%
    ggplot(aes(x = !!var, y = !!sym(".value"), colour = !!sym("season"))) +
    geom_abline(colour = "gray", linetype = "dashed") +
    geom_path() +
    facet_wrap(~ .lag)
}

#' @inheritParams ggtsdisplay
#' @inherit forecast::ggtsdisplay
#'
#' @examples
#' library(tsibble)
#' tsibbledata::ausretail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   ggtsdisplay(Turnover)
#'
#' @rdname ggtsdisplay
#' @export
ggtsdisplay <- function(x, ...){
  UseMethod("ggtsdisplay")
}

#' @inheritParams ggseasonplot.tbl_ts
#' @param lags A vector of lags to display as facets.
#' @rdname ggtsdisplay
#' @importFrom ggplot2 ggplot aes geom_point geom_histogram
#' @export
ggtsdisplay.tbl_ts <- function(x, var = NULL, plot_type = c("partial", "histogram", "scatter", "spectrum"),
                               lag_max = NULL, ...){
  require_package("grid")

  # Set up grid for plots
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))

  plot_type <- match.arg(plot_type)
  var <- guess_plot_var(x, !!enquo(var))

  p1 <- ggplot(x, aes(x = !!index(x), y = !!var)) +
    geom_line() +
    geom_point()

  p2 <- autoplot(ACF(x, !!var, lag_max = lag_max))

  if(plot_type == "partial"){
    p3 <- autoplot(PACF(x, !!var, lag_max = lag_max))

    # Match y-axis range across ACF and PACF
    p2_yrange <- ggplot2::layer_scales(p2)$y$range$range
    p3_yrange <- ggplot2::layer_scales(p3)$y$range$range
    yrange <- range(c(p2_yrange, p3_yrange))
    p2 <- p2 + ylim(yrange)
    p3 <- p3 + ylim(yrange)
  } else if(plot_type == "histogram"){
    p3 <- ggplot(x, aes(x = !!var)) +
      geom_histogram(bins = min(500, grDevices::nclass.FD(na.exclude(x[[expr_text(var)]])))) +
      ggplot2::geom_rug()
  } else if(plot_type == "scatter"){
    p3 <- x %>%
      mutate(!!paste0(expr_text(var),"_lag") := lag(!!var, 1)) %>%
      .[complete.cases(.),] %>%
      ggplot(aes(y = !!var, x = !!sym(paste0(expr_text(var),"_lag")))) +
      geom_point() +
      xlab(expression(Y[t - 1])) + ylab(expression(Y[t]))
  } else if(plot_type == "spectrum"){
    p3 <- spec.ar(x[[expr_text(var)]], plot = FALSE) %>%
      {tibble(spectrum = .$spec[,1], frequency = .$freq)} %>%
      ggplot(aes(x = !!sym("frequency"), y = !!sym("spectrum"))) +
      geom_line() +
      ggplot2::scale_y_log10()
  }

  print(p1, vp = grid::viewport(layout.pos.row = c(1, 1), layout.pos.col = c(1, 2)))
  print(p2, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p3, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
}
