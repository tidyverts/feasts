format_time <- function(x, format, ...){
  out <- format(x, format)
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

  formats <- list(
    Weekday = "%A",
    Monthday = "%d",
    Yearday = "%j",
    Week = "%V",
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
    if(length(unique(format(idx_grp[[1]], format = fmt))) == 1){
      ids <- map(idx_grp, function(x) unique(format(x, format = fmt)))
      if(all(map_lgl(ids, function(x) length(x) == 1)) && length(unique(ids)) == length(idx_grp)){
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
    Week = "%V",
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
    if(sum(duplicated(format(x, format = fmt))) == 0){
      break
    }
  }

  out <- format_time(y, format = fmt)
  if(fmt == "%b"){
    out <- factor(out, levels = month.abb)
  }
  out
}

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
#' @param period The seasonal period to display.
#' @param facet_period A secondary seasonal period to facet by
#' (typically smaller than period).
#' @param polar If TRUE, the seasonplot will be shown on polar coordinates.
#' @param labels Position of the labels for seasonal period identifier.
#' @rdname ggseasonplot
#' @importFrom ggplot2 ggplot aes geom_line
#' @export
ggseasonplot.tbl_ts <- function(x, var = NULL, period = NULL,
                                facet_period, polar = FALSE,
                                labels = c("none", "left", "right", "both"), ...){
  var <- guess_plot_var(x, !!enquo(var))

  labels <- match.arg(labels)
  check_gaps(x)
  idx <- index(x)
  ts_unit <- time_unit(interval(x))

  period <- get_frequencies(period, x, .auto = "largest")
  if(period <= 1){
    abort("The data must contain at least one observation per seasonal period.")
  }
  period <- period*ts_unit

  if(!is_missing(facet_period)){
    facet_period <- get_frequencies(facet_period, x, .auto = "smallest")
    if(facet_period <= 1){
      abort("The data must contain at least one observation per seasonal period.")
    }
    facet_period <- facet_period*ts_unit
  }
  else{
    facet_period <- NULL
  }

  x <- as_tibble(x) %>%
    group_by(
      facet_id = time_identifier(!!idx, facet_period) %empty% NA
    ) %>%
    mutate(
      id = time_identifier(!!idx, period),
      !!as_string(idx) := !!idx - period * (units_since(!!idx) %/% period)
    )

  if(polar){
    extra_x <- x %>%
      group_by(!!sym("facet_id"), !!sym("id")) %>%
      summarise(
        !!expr_text(idx) := max(!!idx) + ts_unit - .Machine$double.eps,
        !!expr_text(var) := (!!var)[[which.min(!!idx)]]
      ) %>%
      group_by(!!sym("facet_id")) %>%
      mutate(!!expr_text(var) := tsibble::lead(!!var)) %>%
      filter(!is.na(!!var))
    x <- rbind(x, extra_x)
  }

  p <- ggplot(x, aes(x = !!idx, y = !!var, colour = !!sym("id"))) +
    geom_line()

  if(!is.null(facet_period)){
    p <- p + facet_grid(~ facet_id, scales = "free_x")
  }

  if(inherits(x[[expr_text(idx)]], "Date")){
    p <- p + ggplot2::scale_x_date(labels = within_time_identifier)
  } else if(inherits(x[[expr_text(idx)]], "POSIXct")){
    p <- p + ggplot2::scale_x_datetime(labels = within_time_identifier)
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
    labels_x <- x %>%
      group_by(facet_id, id) %>%
      filter(!!idx %in% !!label_pos)

    p <- p + ggplot2::geom_text(aes(label = !!sym("id")), data = labels_x) +
      ggplot2::guides(colour = "none")
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
ggsubseriesplot.tbl_ts <- function(x, var = NULL, period = NULL, ...){
  var <- guess_plot_var(x, !!enquo(var))

  check_gaps(x)
  idx <- index(x)

  period <- get_frequencies(period, x, .auto = "smallest")
  if(period <= 1){
    abort("The data must contain at least one observation per seasonal period.")
  }
  period_units <- period*time_unit(interval(x))

  x <- as_tibble(x) %>%
    mutate(
      id = !!idx - period_units*(units_since(!!idx)%/%period_units),
      id = within_time_identifier(id)
    ) %>%
    group_by(id) %>%
    mutate(.yint = mean(!!var))

  p <- ggplot(x, aes(x = !!idx, y = !!var)) +
    geom_line() +
    facet_grid(~ id) +
    geom_hline(aes(yintercept = !!sym(".yint")), colour = "blue")

  if(inherits(x[[expr_text(idx)]], "Date")){
    p <- p + ggplot2::scale_x_date(labels = within_time_identifier)
  } else if(inherits(x[[expr_text(idx)]], "POSIXct")){
    p <- p + ggplot2::scale_x_datetime(labels = within_time_identifier)
  }

  p + ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90))
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
gglagplot.tbl_ts <- function(x, var = NULL, period = NULL, lags = 1:9, ...){
  var <- guess_plot_var(x, !!enquo(var))

  period <- get_frequencies(period, x, .auto = "smallest")
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
    mutate(.lag = factor(!!sym(".lag"), levels = names(lag_exprs), labels = paste("lag", lags))) %>%
    filter(!is.na(!!sym(".value")) | is.na(!!var))

  x %>%
    ggplot(aes(x = !!var, y = !!sym(".value"), colour = !!sym("season"))) +
    geom_abline(colour = "gray", linetype = "dashed") +
    geom_path() +
    facet_wrap(~ .lag)
}

#' Ensemble of time series displays
#'
#' Plots a time series along with its ACF along with an customisable third
#' graphic of either a PACF, lagged scatterplot or spectral density.
#'
#' @param plot_type type of plot to include in lower right corner.
#' @param ... Arguments for subsequent plotting methods.
#' @inheritParams ggseasonplot
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
#' @rdname ggtsdisplay
#' @importFrom ggplot2 ggplot aes geom_point geom_histogram ylim
#' @importFrom stats na.exclude complete.cases
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
    p3 <- stats::spec.ar(x[[expr_text(var)]], plot = FALSE) %>%
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
