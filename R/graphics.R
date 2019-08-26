format_time <- function(x, format, ...){
  if(format == "%Y W%V"){
    return(format(yearweek(x)))
  }
  out <- format(x, format = format)
  if (grepl("%q", format)) {
    qtr <- 1 + as.numeric(format(as.Date(x), "%m"))%/%3
    out <- split(out, qtr) %>% imap(function(x, rpl) gsub("%q", rpl, x)) %>% unsplit(qtr)
  }
  factor(out, levels = unique(out[order(x)]))
}

tz_units_since <- function(x){
  if(!is.null(attr(x, "tz"))){
    x <- as.POSIXct(`attr<-`(as.POSIXlt(x), "tzone", "UTC"))
  }
  units_since(x)
}

# Find minimum largest identifier for each group
# 1. Find largest homogenous descriptor within groups
# 2. Return if descriptor is distinct across groups
# 3. If descriptor varies across groups, add it to list
# 4. Go to next largest descriptor and repeat from 2.
time_identifier <- function(idx, time_units){
  if(is.null(time_units)){
    return(rep(NA, length(idx)))
  }

  if(inherits(idx, "yearweek") && time_units == 52){
    grps <- format(idx, "%Y")
  }
  else{
    grps <- tz_units_since(idx) %/% time_units
  }
  idx_grp <- split(idx, grps)

  # Different origin for weeks
  wk_grps <- (ifelse(inherits(idx, "Date"), 3, 60*60*24*3) + tz_units_since(idx)) %/% time_units
  wk_idx_grp <- split(idx, wk_grps)

  formats <- list(
    Weekday = "%A",
    Monthday = "%d",
    Yearday = "%j",
    Week = "W%V",
    Month = "%b",
    Year = "%Y",
    Yearweek = "%Y W%V",
    Yearmonth = "%Y %b",
    Minute = "%M",
    Hour = "%H",
    HourMinute = "%H:%M",
    Time = "%X",
    Date = "%x",
    Datetime = "%x %X"
  )

  found_format <- FALSE
  for(fmt in formats){
    fmt_idx_grp <- if(grepl("W%V", fmt)) wk_idx_grp else idx_grp
    if(length(unique(format_time(fmt_idx_grp[[1]], format = fmt))) == 1){
      ids <- map(fmt_idx_grp, function(x) unique(format_time(x, format = fmt)))
      if(all(map_lgl(ids, function(x) length(x) == 1)) && length(unique(ids)) == length(fmt_idx_grp)){
        found_format <- TRUE
        break
      }
    }
  }

  if(found_format){
    format_time(idx, format = fmt)
  }
  else{
    # Default to time ranges
    map(idx_grp, function(x) rep(paste0(c(min(x), max(x)), collapse = " - "), length(x))) %>%
      unsplit(grps)
  }
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
    Yearweek = "%Y W%V",
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
#' @param polar If TRUE, the season plot will be shown on polar coordinates.
#' @param labels Position of the labels for seasonal period identifier.
#' @param ... Additional arguments passed to geom_line()
#'
#' @return A ggplot object showing a seasonal plot of a time series.
#'
#' @references
#' Hyndman and Athanasopoulos (2019) Forecasting: principles and practice,
#'  3rd edition, OTexts: Melbourne, Australia. https://OTexts.org/fpp3/
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
gg_season <- function(data, y = NULL, period = NULL, facet_period, max_col = 15,
                      polar = FALSE, labels = c("none", "left", "right", "both"),
                      ...){
  y <- guess_plot_var(data, !!enquo(y))

  labels <- match.arg(labels)
  check_gaps(data)
  idx <- index(data)
  idx_class <- class(data[[as_string(idx)]])
  n_key <- n_keys(data)
  keys <- key(data)
  ts_interval <- interval(data)
  ts_unit <- time_unit(ts_interval)

  period <- get_frequencies(period, data, .auto = "largest")
  if(period <= 1){
    abort("The data must contain at least one observation per seasonal period.")
  }
  period <- period*ts_unit

  if(!is_missing(facet_period)){
    facet_period <- get_frequencies(facet_period, data, .auto = "smallest")
    if(facet_period <= 1){
      abort("The data must contain at least one observation per seasonal period.")
    }
    facet_period <- facet_period*ts_unit
  }
  else{
    facet_period <- NULL
  }

  data <- as_tibble(data) %>%
    group_by(
      facet_id = time_identifier(!!idx, facet_period) %empty% NA,
      !!!key(data)
    ) %>%
    mutate(
      id = as.character(time_identifier(!!idx, period)),
      !!as_string(idx) := !!idx - period * ((tz_units_since(!!idx) +
        ifelse(inherits(!!idx, "Date"), 3, 60*60*24*3)*grepl("\\d{4} W\\d{2}|W\\d{2}",id[1])) %/% period)
    ) %>%
    ungroup() %>%
    mutate(id = ordered(!!sym("id")))

  if(polar){
    warn("Polar plotting is not fully supported yet, and the resulting graph may be incorrect.
This issue will be resolved once vctrs is integrated into dplyr.")
    extra_x <- data %>%
      group_by(!!sym("facet_id"), !!sym("id")) %>%
      summarise(
        !!expr_text(idx) := max(!!idx) + ts_unit - .Machine$double.eps,
        !!expr_text(y) := (!!y)[[which.min(!!idx)]]
      ) %>%
      group_by(!!sym("facet_id")) %>%
      mutate(!!expr_text(y) := dplyr::lead(!!y)) %>%
      filter(!is.na(!!y))
    data <- rbind(data, extra_x)
  }

  num_ids <- NROW(distinct(data, !!sym("id")))

  mapping <- aes(x = !!idx, y = !!y, colour = unclass(!!sym("id")), group = !!sym("id"))

  p <- ggplot(data, mapping) +
    geom_line(...) +
    ggplot2::scale_color_gradientn(colours = scales::hue_pal()(9),
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
    p <- p + facet_grid(rows = vars(!!!keys), scales = "free_y")
  }

  if(inherits(data[[expr_text(idx)]], "Date")){
    p <- p + ggplot2::scale_x_date(breaks = function(limit){
        limit <- add_class(limit, idx_class) # Fix dropped class from group_by+mutate
        if(period/ts_unit <= 12){
          seq(limit[1], length.out = ceiling(period)+1, by = ts_unit)
        } else{
          ggplot2::scale_x_date()$trans$breaks(limit)
        }
      }, labels = within_time_identifier)
  } else if(inherits(data[[expr_text(idx)]], "POSIXct")){
    p <- p + ggplot2::scale_x_datetime(breaks = function(limit){
      if(period == 7*60*60*24){
        limit <- limit - as.numeric(limit)%%(60*60*24)
        seq(as.POSIXct(as.Date(limit[1])), length.out = 8, by = "day")
      }
      else{
        ggplot2::scale_x_datetime()$trans$breaks(limit)
      }
    }, labels = within_time_identifier)
  }

  if(polar){
    p <- p + ggplot2::coord_polar()
  }

  if(labels != "none"){
    if(labels == "left"){
      label_pos <- expr(min(!!idx))
    }
    else if(labels == "right"){
      label_pos <- expr(max(!!idx))
    }
    else{
      label_pos <- expr(range(!!idx))
    }
    labels_x <- data %>%
      group_by(!!!syms(c("facet_id", "id"))) %>%
      filter(!!idx %in% !!label_pos)

    p <- p + ggplot2::geom_text(aes(label = !!sym("id")), data = labels_x) +
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
#'  3rd edition, OTexts: Melbourne, Australia. https://OTexts.org/fpp3/
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

  period <- get_frequencies(period, data, .auto = "smallest")
  if(period <= 1){
    abort("The data must contain at least one observation per seasonal period.")
  }
  period <- period*time_unit(interval(data))

  data <- as_tibble(data) %>%
    group_by(!!!keys) %>%
    mutate(
      id = time_identifier(!!idx, period),
      id = !!idx - period * ((tz_units_since(!!idx) +
        ifelse(inherits(!!idx, "Date"), 3, 60*60*24*3)*grepl("\\d{4} W\\d{2}|W\\d{2}",id[1])) %/% period),
      .yint = !!y
    ) %>%
    group_by(id, !!!keys) %>%
    mutate(.yint = mean(!!sym(".yint"), na.rm = TRUE))

  fct_labeller <- if(inherits(data[["id"]], c("POSIXt", "Date"))) within_time_identifier else ggplot2::label_value
  p <- ggplot(data, aes(x = !!idx, y = !!y)) +
    geom_line(...) +
    facet_grid(rows = vars(!!!keys), cols = vars(fct_labeller(!!sym("id"))),
               scales = "free_y") +
    geom_hline(aes(yintercept = !!sym(".yint")), colour = "blue")

  if(inherits(data[[expr_text(idx)]], "Date")){
    p <- p + ggplot2::scale_x_date(labels = within_time_identifier)
  } else if(inherits(data[[expr_text(idx)]], "POSIXct")){
    p <- p + ggplot2::scale_x_datetime(labels = within_time_identifier)
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
                   geom = c("path", "point"), ...){
  y <- guess_plot_var(data, !!enquo(y))
  geom <- match.arg(geom)
  lag_geom <- switch(geom, path = geom_path, point = geom_point)

  if(n_keys(data) > 1){
    abort("The data provided to contains more than one time series. Please filter a single time series to use `gg_lag()`")
  }

  period <- get_frequencies(period, data, .auto = "smallest")

  period_units <- period*time_unit(interval(data))

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

  mapping <- aes(x = !!y, y = !!sym(".value"))
  if(period > 1){
    mapping$colour <- sym("season")
  }

  data %>%
    ggplot(mapping) +
    geom_abline(colour = "gray", linetype = "dashed") +
    lag_geom(...) +
    facet_wrap(~ .lag) +
    ylab(paste0("lag(", as_string(y), ", n)"))
}

#' Ensemble of time series displays
#'
#' Plots a time series along with its ACF along with an customisable third
#' graphic of either a PACF, histogram, lagged scatterplot or spectral density.
#'
#' @param plot_type type of plot to include in lower right corner. By default
#' (`"auto"`), a season plot will be shown for seasonal data, and a spectrum plot
#' will be shown for non-seasonal data.
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
#' \url{https://OTexts.org/fpp3/}
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

  plot_type <- match.arg(plot_type)
  if(plot_type == "auto"){
    period <- get_frequencies(NULL, data, .auto = "all")
    if(all(period <= 1)){
      plot_type <- "spectrum"
    }
    else{
      plot_type <- "season"
    }
  }

  # Set up grid for plots
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))

  y <- guess_plot_var(data, !!enquo(y))

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
    p3 <- stats::spec.ar(data[[expr_text(y)]], plot = FALSE) %>%
      {tibble(spectrum = .$spec[,1], frequency = .$freq)} %>%
      ggplot(aes(x = !!sym("frequency"), y = !!sym("spectrum"))) +
      geom_line() +
      ggplot2::scale_y_log10()
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
#' \url{https://OTexts.org/fpp3/}
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
