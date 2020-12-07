format_time <- function(x, format, ...){
  if(format == "%Y W%V"){
    return(format(yearweek(x)))
  }
  out <- format(x, format = format)
  if (grepl("%q", format)) {
    qtr <- 1 + as.numeric(format(as.Date(x), "%m"))%/%3
    out <- split(out, qtr) %>% imap(function(x, rpl) gsub("%q", rpl, x)) %>% unsplit(qtr)
  }

  lvls <- switch(format,
                 `W%V` = sprintf("W%02d", 1:53),
                 unique(out[order(x)]))

  ordered(out, levels = lvls)
}

tz_units_since <- function(x){
  if(!is.null(attr(x, "tz"))){
    x <- as.POSIXct(`attr<-`(as.POSIXlt(x), "tzone", "UTC"))
  }
  as.double(x)
}

# Find minimum largest identifier for each group
# 1. Find largest homogeneous descriptor within groups
# 2. Return if descriptor is distinct across groups
# 3. If descriptor varies across groups, add it to list
# 4. Go to next largest descriptor and repeat from 2.
time_identifier <- function(idx, period, base = NULL, within = NULL, interval){
  if(is.null(period)){
    return(rep(NA, length(idx)))
  }

  # Early return for years in weeks, as years are structured differently in context of weeks
  if (identical(period, lubridate::years(1)) && identical(base, lubridate::weeks(1))){
    return(list(facet_id = NA, id = format_time(idx, format = "%G")))
  }

  grps <- floor_tsibble_date(idx, period)

  facet_grps <- if(!is.null(within)){
    time_identifier(idx, period = within, base = period, interval = interval)$id
  } else {
    FALSE
  }

  # Create format groups for each series
  fmt_idx_grp <- map2(
    split(idx, facet_grps),
    split(grps, facet_grps),
    split
  )

  formats <- list(
    Weekday = "%A",
    Monthday = "%d",
    Yearday = "%j",
    Week = "W%V",
    Month = "%b",
    Year = "%Y",
    Yearweek = "%G W%V",
    Yearmonth = "%Y %b",
    Minute = "%M",
    Hour = "%H",
    HourMinute = "%H:%M",
    Time = "%X",
    Date = "%x",
    Datetime = "%x %X"
  )
  # Remove unreasonable formats for a given interval
  if(!is_empty(interval)){
    if(interval >= months(1)){
      formats <- formats[c("Month", "Year", "Yearmonth", "Date")]
    } else if (interval >= lubridate::weeks(1)){
      formats <- formats[c("Monthday", "Yearday", "Week", "Month", "Year",
                           "Yearweek", "Yearmonth", "Date")]
    } else if (interval >= lubridate::days(1)){
      formats <- formats[c("Weekday", "Monthday", "Yearday", "Week", "Month", "Year",
                           "Yearweek", "Yearmonth", "Date")]
    }
  }


  # Check if the format uniquely identifies the group
  for(fmt in formats){
    for(fct in fmt_idx_grp){
      found_format <- FALSE
      id <- rep(NA_character_, length(fct))
      for(i in seq_along(fct)){
        val <- unique(format_time(fct[[i]], format = fmt))
        if(length(val) > 1) break
        id[i] <- as.character(val)
      }
      if(is.na(id[length(id)])) break
      if(anyDuplicated(id)) break
      found_format <- TRUE
    }
    if(found_format) break
  }

  out <- if(found_format){
    format_time(idx, format = fmt)
  }
  else{
    # Default to time ranges
    map2(fmt_idx_grp, split(grps, facet_grps), function(x, grps){
      map(x, function(y){
        rep(paste0(c(min(y), max(y)), collapse = " - "), length(y))
      }) %>%
        unsplit(grps)
    }) %>%
      unsplit(facet_grps) %>%
      ordered()
  }
  list(
    facet_id = if(!is.null(within)) facet_grps else NA,
    id = out
  )
}

within_time_identifier <- function(x){
  formats <- list(
    Year = "%Y",
    Quarter = "Q%q",
    Month = "%b",
    Week = "W%V",
    Weekday = "%A",
    Monthday = "%d",
    Yearquarter = "%Y Q%q",
    Yearmonth = "%Y %b",
    Yearweek = "%G W%V",
    Yearday = "%j",
    Date = "%x",
    Hour = "%H",
    Minute = "%M",
    HourMinute = "%H:%M",
    Time = "%X",
    Datetime = "%x %X"
  )

  y <- x
  x <- unique(x[!is.na(x)])

  for(fmt in formats){
    if(sum(duplicated(format_time(x[-length(x)], format = fmt))) == 0){
      break
    }
  }

  format_time(y, format = fmt)
}

guess_plot_var <- function(x, y){
  if(quo_is_null(enquo(y))){
    inform(sprintf(
      "Plot variable not specified, automatically selected `y = %s`",
      measured_vars(x)[1]
    ))
    sym(measured_vars(x)[1])
  }
  else{
    get_expr(enexpr(y))
  }
}

#' Seasonal plot
#'
#' Produces a time series seasonal plot. A seasonal plot is similar to a regular
#' time series plot, except the x-axis shows data from within each season. This
#' plot type allows the underlying seasonal pattern to be seen more clearly,
#' and is especially useful in identifying years in which the pattern changes.
#'
#' @param data A tidy time series object (tsibble)
#' @param y The variable to plot (a bare expression). If NULL, it will
#' automatically selected from the data.
#' @param period The seasonal period to display.
#' @param facet_period A secondary seasonal period to facet by
#' (typically smaller than period).
#' @param max_col The maximum number of colours to display on the plot. If the
#' number of seasonal periods in the data is larger than `max_col`, the plot
#' will not include a colour. Use `max_col = 0` to never colour the lines, or Inf
#' to always colour the lines. If labels are used, then max_col will be ignored.
#' @param pal A colour palette to be used.
#' @param polar If TRUE, the season plot will be shown on polar coordinates.
#' @param labels Position of the labels for seasonal period identifier.
#' @param ... Additional arguments passed to geom_line()
#'
#' @return A ggplot object showing a seasonal plot of a time series.
#'
#' @references
#' Hyndman and Athanasopoulos (2019) Forecasting: principles and practice,
#'  3rd edition, OTexts: Melbourne, Australia. https://OTexts.com/fpp3/
#'
#' @examples
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   gg_season(Turnover)
#'
#' @importFrom ggplot2 ggplot aes geom_line
#' @export
gg_season <- function(data, y = NULL, period = NULL, facet_period = NULL,
                      max_col = 15, pal = scales::hue_pal()(9), polar = FALSE,
                      labels = c("none", "left", "right", "both"), ...){
  y <- guess_plot_var(data, !!enquo(y))

  labels <- match.arg(labels)
  check_gaps(data)
  idx <- index_var(data)
  n_key <- n_keys(data)
  keys <- key(data)
  ts_interval <- interval_to_period(interval(data))

  if(is.null(period)){
    period <- names(get_frequencies(period, data, .auto = "largest"))
  }
  if(is.numeric(period)){
    period <- period*ts_interval
  }
  period <- lubridate::as.period(period)
  if(period <= ts_interval){
    abort("The data must contain at least one observation per seasonal period.")
  }

  if(!is.null(facet_period)){
    if(is.numeric(facet_period)){
      facet_period <- facet_period*ts_interval
    }
    facet_period <- lubridate::as.period(facet_period)

    if(facet_period <= ts_interval){
      abort("The data must contain at least one observation per seasonal period.")
    }
  }

  data <- as_tibble(data)
  data[c("facet_id", "id")] <- time_identifier(data[[idx]], period,
                                               within = facet_period, interval = ts_interval)
  data[idx] <- time_offset_origin(data[[idx]], period)

  if(polar){
    extra_x <- data %>%
      group_by(!!sym("facet_id"), !!sym("id")) %>%
      summarise(
        !!idx := max(as.Date(!!sym(idx))) + ts_interval - .Machine$double.eps,
        !!expr_text(y) := (!!y)[[which.min(!!sym(idx))]]
      ) %>%
      group_by(!!sym("facet_id")) %>%
      mutate(!!expr_text(y) := dplyr::lead(!!y)) %>%
      filter(!is.na(!!y))
    data <- dplyr::bind_rows(data, extra_x)
  }

  num_ids <- length(unique(data[["id"]]))

  mapping <- aes(x = !!sym(idx), y = !!y, colour = unclass(!!sym("id")), group = !!sym("id"))

  p <- ggplot(data, mapping) +
    geom_line(...) +
    ggplot2::scale_color_gradientn(colours = pal,
                                   breaks = if (num_ids < max_col) seq_len(num_ids) else ggplot2::waiver(),
                                   labels = function(idx) levels(data$id)[idx]) +
    ggplot2::labs(colour = NULL)

  if(num_ids < max_col){
    p <- p + ggplot2::guides(colour = ggplot2::guide_legend())
  }

  if(!is.null(facet_period)){
    p <- p + facet_grid(rows = vars(!!!keys),
                        cols = vars(!!sym("facet_id")),
                        scales = ifelse(n_key > 1, "free", "free_x"))
  }
  else if(n_key > 1){
    p <- p + facet_grid(
      rows = vars(!!!lapply(keys, function(x) expr(format(!!x)))),
      scales = "free_y")
  }

  if(inherits(data[[idx]], "Date")){
    p <- p + ggplot2::scale_x_date(breaks = function(limit){
      breaks <- if(suppressMessages(len <- period/ts_interval) <= 12){
        ggplot2::scale_x_date()$trans$breaks(limit, n = len)
      } else{
        ggplot2::scale_x_date()$trans$breaks(limit)
      }
      unique(time_offset_origin(within_bounds(breaks, limit), period))
    }, labels = within_time_identifier)
  } else if(inherits(data[[idx]], "POSIXct")){
    p <- p + ggplot2::scale_x_datetime(breaks = function(limit){
      breaks <- if(period == lubridate::weeks(1)){
        ggplot2::scale_x_datetime()$trans$breaks(limit, n = 7)
      }
      else{
        ggplot2::scale_x_datetime()$trans$breaks(limit)
      }
      unique(time_offset_origin(within_bounds(breaks, limit), period))
    }, labels = within_time_identifier)
  } else {
    scale_fn <- paste0("scale_x_", ggplot2::scale_type(data[[idx]])[1])
    scale_fn <- if(exists(scale_fn, parent.frame(), mode = "function")){
      get(scale_fn, parent.frame(), mode = "function")
    } else {
      get(scale_fn, asNamespace("feasts"), mode = "function")
    }
    p <- p + scale_fn(
      breaks = function(limit){
        breaks <- if(suppressMessages(len <- period/ts_interval) <= 12){
          ggplot2::scale_x_date()$trans$breaks(as.Date(limit), n = len)
        } else{
          scale_fn()$trans$breaks(limit)
        }
        unique(time_offset_origin(within_bounds(breaks, limit), period))
      }, labels = within_time_identifier)
  }

  if(polar){
    p <- p + ggplot2::coord_polar()
  }

  if(labels != "none"){
    if(labels == "left"){
      label_pos <- expr(min(!!sym(idx)))
    }
    else if(labels == "right"){
      label_pos <- expr(max(!!sym(idx)))
    }
    else{
      label_pos <- expr(range(!!sym(idx)))
    }
    labels_x <- data %>%
      group_by(!!!syms(c("facet_id", "id"))) %>%
      filter(!!sym(idx) %in% !!label_pos)

    p <- p + ggplot2::geom_text(aes(label = !!sym("id")), data = labels_x, hjust = "outward") +
      ggplot2::guides(colour = "none")
  }

  p
}

#' Seasonal subseries plots
#'
#' A seasonal subseries plot facets the time series by each season in the
#' seasonal period. These facets form smaller time series plots consisting of
#' data only from that season. If you had several years of monthly data, the
#' resulting plot would show a separate time series plot for each month. The
#' first subseries plot would consist of only data from January. This case is
#' given as an example below.
#'
#' The horizontal lines are used to represent the mean of each facet, allowing
#' easy identification of seasonal differences between seasons. This plot is
#' particularly useful in identifying changes in the seasonal pattern over time.
#'
#' similar to a seasonal plot ([`gg_season()`]), and
#'
#' @inheritParams gg_season
#'
#' @return A ggplot object showing a seasonal subseries plot of a time series.
#'
#' @references
#' Hyndman and Athanasopoulos (2019) Forecasting: principles and practice,
#'  3rd edition, OTexts: Melbourne, Australia. https://OTexts.com/fpp3/
#'
#' @examples
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   gg_subseries(Turnover)
#'
#' @importFrom ggplot2 facet_grid
#' @export
gg_subseries <- function(data, y = NULL, period = NULL, ...){
  y <- guess_plot_var(data, !!enquo(y))
  n_key <- n_keys(data)
  keys <- key(data)
  check_gaps(data)
  idx <- index(data)
  ts_interval <- interval_to_period(interval(data))

  if(is.null(period)){
    period <- names(get_frequencies(period, data, .auto = "largest"))
  }
  if(is.numeric(period)){
    period <- period*ts_interval
  }
  period <- lubridate::as.period(period)
  if(period <= 1){
    abort("The data must contain at least one observation per seasonal period.")
  }

  data <- as_tibble(data) %>%
    mutate(
      id = time_offset_origin(!!idx, !!period),
      !!idx := !!idx,
      .yint = !!y
    ) %>%
    group_by(!!sym("id"), !!!keys) %>%
    mutate(.yint = mean(!!sym(".yint"), na.rm = TRUE))

  fct_labeller <- if(inherits(data[["id"]], c("yearquarter", "yearmonth", "yearweek", "POSIXt", "Date"))){
    within_time_identifier
  } else if(is.numeric(data[["id"]])) {
    function(x) format(x - 1969)
  }
  else {
    format
  }

  p <- ggplot(data, aes(x = !!idx, y = !!y)) +
    geom_line(...) +
    facet_grid(
      rows = vars(!!!lapply(keys, function(x) expr(format(!!x)))),
      cols = vars(fct_labeller(!!sym("id"))),
      scales = "free_y") +
    geom_hline(aes(yintercept = !!sym(".yint")), colour = "blue")

  if(inherits(data[[expr_text(idx)]], "Date")){
    p <- p + ggplot2::scale_x_date(labels = within_time_identifier)
  } else if(inherits(data[[expr_text(idx)]], "POSIXct")){
    p <- p + ggplot2::scale_x_datetime(labels = within_time_identifier)
  } else {
    scale_fn <- paste0("scale_x_", ggplot2::scale_type(data[[expr_text(idx)]])[1])
    scale_fn <- if(exists(scale_fn, parent.frame(), mode = "function")){
      get(scale_fn, parent.frame(), mode = "function")
    } else {
      get(scale_fn, asNamespace("feasts"), mode = "function")
    }
    p <- p + scale_fn(labels = within_time_identifier)
  }

  p + ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90))
}


#' Lag plots
#'
#' A lag plot shows the time series against lags of itself. It is often coloured
#' the seasonal period to identify how each season correlates with others.
#'
#' @inheritParams gg_season
#' @param lags A vector of lags to display as facets.
#' @param geom The geometry used to display the data.
#' @param arrow Arrow specification to show the direction in the lag path. If
#' TRUE, an appropriate default arrow will be used. Alternatively, a user
#' controllable arrow created with [`grid::arrow()`] can be used.
#' @param ... Additional arguments passed to the geom.
#'
#' @return A ggplot object showing a lag plot of a time series.
#'
#' @examples
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   gg_lag(Turnover)
#'
#' @importFrom ggplot2 ggplot aes geom_path geom_abline facet_wrap
#' @export
gg_lag <- function(data, y = NULL, period = NULL, lags = 1:9,
                   geom = c("path", "point"),
                   arrow = FALSE, ...){
  if(isTRUE(arrow)){
    arrow <- grid::arrow(length = grid::unit(0.05, "npc"))
  }
  else if (isFALSE(arrow)){
    arrow <- NULL
  }

  y <- guess_plot_var(data, !!enquo(y))
  geom <- match.arg(geom)
  lag_geom <- switch(geom, path = geom_path, point = function(..., arrow) geom_point(...))

  if(n_keys(data) > 1){
    abort("The data provided to contains more than one time series. Please filter a single time series to use `gg_lag()`")
  }

  period <- get_frequencies(period, data, .auto = "smallest")

  period_units <- period*default_time_units(interval(data))

  lag_exprs <- map(lags, function(lag) expr(lag(!!y, !!lag))) %>%
    set_names(paste0(".lag_", lags))

  idx <- index(data)

  data <- data %>%
    as_tibble %>%
    mutate(
      season = within_time_identifier(!!idx - period_units*(tz_units_since(!!idx)%/%period_units)),
      !!!lag_exprs)

  num_na <- eval_tidy(expr(sum(is.na(!!y))), data = data)
  if(num_na > 0){
    warn(sprintf("Removed %i rows containing missing values (gg_lag).", num_na))
  }

  data <- data %>%
    gather(".lag", ".value", !!names(lag_exprs)) %>%
    mutate(.lag = factor(!!sym(".lag"), levels = names(lag_exprs), labels = paste("lag", lags))) %>%
    filter(!is.na(!!sym(".value")) & !is.na(!!y))

  mapping <- aes(x = !!sym(".value"), y = !!y)
  if(period > 1){
    mapping$colour <- sym("season")
  }

  data %>%
    ggplot(mapping) +
    geom_abline(colour = "gray", linetype = "dashed") +
    lag_geom(..., arrow = arrow) +
    facet_wrap(~ .lag) +
    ggplot2::theme(aspect.ratio = 1) +
    xlab(paste0("lag(", as_string(y), ", n)"))
}

#' Ensemble of time series displays
#'
#' Plots a time series along with its ACF along with an customisable third
#' graphic of either a PACF, histogram, lagged scatterplot or spectral density.
#'
#' @param plot_type type of plot to include in lower right corner. By default
#' (`"auto"`) a season plot will be shown for seasonal data, a spectrum plot
#' will be shown for non-seasonal data without missing values, and a PACF will
#' be shown otherwise.
#' @inheritParams gg_season
#' @inheritParams ACF
#'

#' @return A list of ggplot objects showing useful plots of a time series.
#'
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#'
#' @seealso \code{\link[stats]{plot.ts}}, \code{\link{ACF}},
#' \code{\link[stats]{spec.ar}}
#'
#' @references Hyndman and Athanasopoulos (2019) \emph{Forecasting: principles
#' and practice}, 3rd edition, OTexts: Melbourne, Australia.
#' \url{https://OTexts.com/fpp3/}
#'
#' @examples
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   gg_tsdisplay(Turnover)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_histogram ylim
#' @importFrom stats na.exclude complete.cases
#' @export
gg_tsdisplay <- function(data, y = NULL, plot_type = c("auto", "partial", "season", "histogram", "scatter", "spectrum"),
                         lag_max = NULL){
  if(n_keys(data) > 1){
    abort("The data provided to contains more than one time series. Please filter a single time series to use `gg_tsdisplay()`")
  }
  require_package("grid")

  y <- guess_plot_var(data, !!enquo(y))

  plot_type <- match.arg(plot_type)
  if(plot_type == "auto"){
    period <- get_frequencies(NULL, data, .auto = "all")
    if(all(period <= 1)){
      plot_type <- if(any(is.na(data[[expr_text(y)]]))) "partial" else "spectrum"
    }
    else{
      plot_type <- "season"
    }
  }

  # Set up grid for plots
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))

  p1 <- ggplot(data, aes(x = !!index(data), y = !!y)) +
    geom_line() +
    geom_point()

  p2 <- autoplot(ACF(data, !!y, lag_max = lag_max))

  if(plot_type == "partial"){
    p3 <- autoplot(PACF(data, !!y, lag_max = lag_max))

    # Match y-axis range across ACF and PACF
    p2_yrange <- ggplot2::layer_scales(p2)$y$range$range
    p3_yrange <- ggplot2::layer_scales(p3)$y$range$range
    yrange <- range(c(p2_yrange, p3_yrange))
    p2 <- p2 + ylim(yrange)
    p3 <- p3 + ylim(yrange)
  } else if(plot_type == "season"){
    p3 <- gg_season(data, !!y)
  } else if(plot_type == "histogram"){
    p3 <- ggplot(data, aes(x = !!y)) +
      geom_histogram(bins = min(500, grDevices::nclass.FD(na.exclude(data[[expr_text(y)]])))) +
      ggplot2::geom_rug()
  } else if(plot_type == "scatter"){
    p3 <- data %>%
      mutate(!!paste0(expr_text(y),"_lag") := lag(!!y, 1)) %>%
      .[complete.cases(.),] %>%
      ggplot(aes(y = !!y, x = !!sym(paste0(expr_text(y),"_lag")))) +
      geom_point() +
      xlab(expression(Y[t - 1])) + ylab(expression(Y[t]))
  } else if(plot_type == "spectrum"){
    spec <- safely(stats::spec.ar)(eval_tidy(y, data), plot = FALSE)

    p3 <- if (is.null(spec[["result"]])){
      if(spec$error$message == "missing values in object"){
        warn("Spectrum plot could not be shown as the data contains missing values. Consider using a different `plot_type`.")
      }
      else {
        warn(sprintf("Spectrum plot could not be shown as an error occurred: %s", spec$error$message))
      }
      ggplot() + ggplot2::labs(x = "frequency", y = "spectrum")
    } else {
      spec[["result"]] %>%
        {tibble(spectrum = drop(.$spec), frequency = .$freq)} %>%
        ggplot(aes(x = !!sym("frequency"), y = !!sym("spectrum"))) +
        geom_line() +
        ggplot2::scale_y_log10()
    }
  }

  structure(list(p1, p2, p3), class = c("gg_tsensemble", "gg"))
}

#' Ensemble of time series residual diagnostic plots
#'
#' Plots the residuals using a time series plot, ACF and histogram.
#'
#' @param data A mable containing one model with residuals.
#' @param ... Additional arguments passed to [`gg_tsdisplay()`].
#'
#' @return A list of ggplot objects showing a useful plots of a time series model's residuals.
#'
#' @seealso [`gg_tsdisplay()`]
#'
#' @references Hyndman and Athanasopoulos (2019) \emph{Forecasting: principles
#' and practice}, 3rd edition, OTexts: Melbourne, Australia.
#' \url{https://OTexts.com/fpp3/}
#'
#' @examples
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#'
#' tsibbledata::aus_production %>%
#'   model(ETS(Beer)) %>%
#'   gg_tsresiduals()
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_histogram ylim
#' @importFrom stats na.exclude complete.cases
#' @export
gg_tsresiduals <- function(data, ...){
  if(!fabletools::is_mable(data)){
    abort("gg_tsresiduals() must be used with a mable containing only one model.")
  }

  data <- stats::residuals(data)
  if(n_keys(data) > 1){
    abort("gg_tsresiduals() must be used with a mable containing only one model.")
  }

  gg_tsdisplay(data, !!sym(".resid"), plot_type = "histogram", ...)
}

#' @export
`+.gg_tsensemble` <- function(e1, e2){
  e1[[1]] <- e1[[1]] + e2
  e1
}

#' @export
print.gg_tsensemble <- function(x, ...){
  print(x[[1]], vp = grid::viewport(layout.pos.row = c(1, 1), layout.pos.col = c(1, 2)))
  print(x[[2]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(x[[3]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
}

#' Plot characteristic ARMA roots
#'
#' Produces a plot of the inverse AR and MA roots of an ARIMA model.
#' Inverse roots outside the unit circle are shown in red.
#'
#' Only models which compute ARMA roots can be visualised with this function.
#' That is to say, the `glance()` of the model contains `ar_roots` and `ma_roots`.
#'
#' @param data A mable containing models with AR and/or MA roots.
#'
#' @return A ggplot object the characteristic roots from ARMA components.
#'
#' @examples
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibble)
#' library(dplyr)
#'
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   model(ARIMA(Turnover ~ pdq(0,1,1) + PDQ(0,1,1))) %>%
#'   gg_arma()
#' }
#' @export
gg_arma <- function(data){
  if(!fabletools::is_mable(data)){
    abort("gg_arma() must be used with a mable containing models that compute ARMA roots")
  }

  fcts <- c(key(data), sym(".model"))

  data <- data %>%
    fabletools::glance() %>%
    gather("type", "root", !!sym("ar_roots"), !!sym("ma_roots")) %>%
    unnest_tbl("root") %>%
    filter(!is.na(!!sym("root"))) %>%
    mutate(type = factor(!!sym("type"), levels = c("ar_roots", "ma_roots"),
                         labels = c("AR roots", "MA roots")),
           UnitCircle = factor(abs(1/!!sym("root")) > 1, levels = c(TRUE, FALSE),
                               labels = c("Outside", "Within")))

  ggplot(data, aes(x = Re(1/!!sym("root")), y = Im(1/!!sym("root")),
                  colour = !!sym("UnitCircle"))) +
    ggplot2::annotate(
      "path", x = cos(seq(0, 2 * pi, length.out = 100)),
      y = sin(seq(0, 2 * pi, length.out = 100))
    ) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    ggplot2::coord_fixed(ratio = 1) +
    facet_grid(vars(!!!fcts), vars(!!sym("type")))
}
