format_time <- function(x, format, ...){
  if(format == "%Y W%V"){
    return(format(yearweek(x)))
  }
  out <- format(x, format = format)
  if (grepl("%q", format)) {
    qtr <- 1 + as.numeric(format(as.Date(x), "%m"))%/%3
    out <- split(out, qtr) %>% imap(function(x, rpl) gsub("%q", rpl, x)) %>% unsplit(qtr)
  }
  out
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

  grps <- units_since(idx) %/% time_units
  idx_grp <- split(idx, grps)

  # Different origin for weeks
  wk_grps <- (60*60*24*3 + units_since(idx)) %/% time_units
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
    if(sum(duplicated(format(x[-length(x)], format = fmt))) == 0){
      break
    }
  }

  out <- format_time(y, format = fmt)
  if(fmt == "%b"){
    out <- factor(out, levels = month.abb)
  }
  out
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

#' @inherit forecast::ggseasonplot
#'
#' @param data A tidy time series object (tsibble)
#' @param ... Additional arguments passed to methods
#' @param y The variable to plot (a bare expression). If NULL, it will
#' automatically selected from the data.
#' @param period The seasonal period to display.
#' @param facet_period A secondary seasonal period to facet by
#' (typically smaller than period).
#' @param polar If TRUE, the season plot will be shown on polar coordinates.
#' @param labels Position of the labels for seasonal period identifier.
#'
#' @examples
#' library(tsibble)
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   gg_season(Turnover)
#'
#' @importFrom ggplot2 ggplot aes geom_line
#' @export
gg_season <- function(data, y = NULL, period = NULL,
                                facet_period, polar = FALSE,
                                labels = c("none", "left", "right", "both"), ...){
  y <- guess_plot_var(data, !!enquo(y))

  labels <- match.arg(labels)
  check_gaps(data)
  idx <- index(data)
  ts_unit <- time_unit(interval(data))

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
      facet_id = time_identifier(!!idx, facet_period) %empty% NA
    ) %>%
    mutate(
      id = time_identifier(!!idx, period),
      !!as_string(idx) := !!idx - period * ((units_since(!!idx) + 60*60*24*3*grepl("\\d{4} W\\d{2}|W\\d{2}",id[1])) %/% period)
    )

  if(polar){
    extra_x <- data %>%
      group_by(!!sym("facet_id"), !!sym("id")) %>%
      summarise(
        !!expr_text(idx) := max(!!idx) + ts_unit - .Machine$double.eps,
        !!expr_text(y) := (!!y)[[which.min(!!idx)]]
      ) %>%
      group_by(!!sym("facet_id")) %>%
      mutate(!!expr_text(y) := tsibble::lead(!!y)) %>%
      filter(!is.na(!!y))
    data <- rbind(data, extra_x)
  }

  p <- ggplot(data, aes(x = !!idx, y = !!y, colour = !!sym("id"))) +
    geom_line()

  if(!is.null(facet_period)){
    p <- p + facet_grid(~ facet_id, scales = "free_x")
  }

  if(inherits(data[[expr_text(idx)]], "Date")){
    p <- p + ggplot2::scale_x_date(labels = within_time_identifier)
  } else if(inherits(data[[expr_text(idx)]], "POSIXct")){
    p <- p + ggplot2::scale_x_datetime(breaks = function(limit){
      if(period == 7*60*60*24){
        limit <- limit - as.numeric(limit)%%(60*60*24)
        seq(limit[1], length.out = 8, by = "day")
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

#' @inherit forecast::ggsubseriesplot
#'
#' @inheritParams gg_season
#'
#' @examples
#' library(tsibble)
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

  check_gaps(data)
  idx <- index(data)

  period <- get_frequencies(period, data, .auto = "smallest")
  if(period <= 1){
    abort("The data must contain at least one observation per seasonal period.")
  }
  period <- period*time_unit(interval(data))

  data <- as_tibble(data) %>%
    mutate(
      id = time_identifier(!!idx, period),
      id = !!idx - period * ((units_since(!!idx) + 60*60*24*3*grepl("\\d{4} W\\d{2}|W\\d{2}",id[1])) %/% period),
      id = within_time_identifier(id)
    ) %>%
    group_by(id) %>%
    mutate(.yint = mean(!!y, na.rm = TRUE))

  p <- ggplot(data, aes(x = !!idx, y = !!y)) +
    geom_line() +
    facet_grid(~ id) +
    geom_hline(aes(yintercept = !!sym(".yint")), colour = "blue")

  if(inherits(data[[expr_text(idx)]], "Date")){
    p <- p + ggplot2::scale_x_date(labels = within_time_identifier)
  } else if(inherits(data[[expr_text(idx)]], "POSIXct")){
    p <- p + ggplot2::scale_x_datetime(labels = within_time_identifier)
  }

  p + ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90))
}


#' @inheritParams gg_season
#' @inherit forecast::gglagplot
#'
#' @param lags A vector of lags to display as facets.
#' @param geom The geometry used to display the data.
#'
#' @examples
#' library(tsibble)
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

  period <- get_frequencies(period, data, .auto = "smallest")

  period_units <- period*time_unit(interval(data))

  lag_exprs <- map(lags, function(lag) expr(lag(!!y, !!lag))) %>%
    set_names(paste0(".lag_", lags))

  idx <- index(data)

  data <- data %>%
    as_tibble %>%
    mutate(
      season = within_time_identifier(!!idx - period_units*(units_since(!!idx)%/%period_units)),
      !!!lag_exprs) %>%
    gather(".lag", ".value", !!names(lag_exprs)) %>%
    mutate(.lag = factor(!!sym(".lag"), levels = names(lag_exprs), labels = paste("lag", lags))) %>%
    filter(!is.na(!!sym(".value")) | is.na(!!y))

  mapping <- aes(x = !!y, y = !!sym(".value"))
  if(period > 1){
    mapping$colour <- sym("season")
  }
  data %>%
    ggplot(mapping) +
    geom_abline(colour = "gray", linetype = "dashed") +
    lag_geom() +
    facet_wrap(~ .lag) +
    ylab(paste0("lag(", as_string(y), ", n)"))
}

#' Ensemble of time series displays
#'
#' Plots a time series along with its ACF along with an customisable third
#' graphic of either a PACF, lagged scatterplot or spectral density.
#'
#' @param plot_type type of plot to include in lower right corner.
#' @param ... Arguments for subsequent plotting methods.
#' @inheritParams gg_season
#' @inheritParams ACF
#'
#' @return A ggplot2 plot.
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#'
#' @seealso \code{\link[stats]{plot.ts}}, \code{\link{ACF}},
#' \code{\link[stats]{spec.ar}}
#'
#' @references Hyndman and Athanasopoulos (2018) \emph{Forecasting: principles
#' and practice}, 2nd edition, OTexts: Melbourne, Australia.
#' \url{https://OTexts.org/fpp2/}
#'
#' @examples
#' library(tsibble)
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
gg_tsdisplay <- function(data, y = NULL, plot_type = c("partial", "histogram", "scatter", "spectrum"),
                               lag_max = NULL, ...){
  require_package("grid")

  # Set up grid for plots
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))

  plot_type <- match.arg(plot_type)
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

  print(p1, vp = grid::viewport(layout.pos.row = c(1, 1), layout.pos.col = c(1, 2)))
  print(p2, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p3, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
  invisible(NULL)
}
