add_class <- function(x, new_class){
  `class<-`(x, union(new_class, class(x)))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

require_package <- function(pkg){
  if(!requireNamespace(pkg, quietly = TRUE)){
    abort(
      sprintf('The `%s` package must be installed to use this functionality. It can be installed with install.packages("%s")', pkg, pkg)
    )
  }
}

`%empty%` <- function(x, y) {
  if (length(x) == 0) y else x
}

lag <- function(x, n){
  if (n == 0)
    return(x)
  xlen <- length(x)
  n <- pmin(n, xlen)
  out <- c(rep(NA, n), x[seq_len(xlen - n)])
  out
}

nest_keys <- function(.data, nm = "data"){
  out <- unclass(key_data(.data))
  key <- key_vars(.data)
  row_indices <- out[[length(out)]]
  out[[length(out)]] <- NULL
  col_nest <- -match(key, colnames(.data))
  if(is_empty(col_nest)){
    col_nest <- NULL
  }
  idx <- index_var(.data)
  idx2 <- index2_var(.data)
  ordered <- is_ordered(.data)
  regular <- is_regular(.data)
  out[[nm]] <- map(row_indices, function(x, i, j){
    out <- if(is.null(j)) x[i,] else x[i,j]
    build_tsibble_meta(
      out,
      key_data = as_tibble(list(.rows = list(seq_along(i)))),
      index = idx, index2 = idx2, ordered = ordered,
      interval = if(length(i) > 1 && regular) interval_pull(out[[idx]]) else interval(.data)
    )
  }, x = as_tibble(.data), j = col_nest)
  as_tibble(out)
}

unnest_tbl <- function(.data, tbl_col, .sep = NULL){
  row_indices <- rep.int(seq_len(NROW(.data)), map_int(.data[[tbl_col[[1]]]], NROW))

  nested_cols <- map(tbl_col, function(x){
    lst_col <- .data[[x]]
    if(is.data.frame(lst_col[[1]])){
      dplyr::bind_rows(!!!set_names(lst_col, rep(x, length(lst_col))))
    }
    else{
      list2(!!x := unlist(lst_col))
    }
  })

  if(!is.null(.sep)){
    nested_cols <- map2(
      nested_cols, tbl_col,
      function(x, nm) set_names(x, paste(nm, colnames(x), sep = .sep))
    )
  }

  dplyr::bind_cols(
    .data[row_indices, setdiff(names(.data), tbl_col), drop = FALSE], # Parent cols
    !!!nested_cols # Nested cols
  )
}

unnest_tsbl <- function(.data, tsbl_col, parent_key = NULL, interval = NULL){
  tsbl <- .data[[tsbl_col]][[1L]]
  if (!is_tsibble(tsbl)) {
    abort("Unnested column is not a tsibble object.")
  }
  idx <- index(tsbl)
  idx_chr <- as_string(idx)
  key <- c(parent_key, key_vars(tsbl))

  .data <- unnest_tbl(.data, tsbl_col)

  class(.data[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(.data, key = !!key, index = !!idx,
                index2 = !!index2(tsbl), ordered = is_ordered(tsbl),
                interval = interval%||%interval(tsbl))
}

interval_to_period <- function(interval){
  with(interval, lubridate::years(year) +
         lubridate::period(3*quarter + month, units = "month") + lubridate::weeks(week) +
         lubridate::days(day) + lubridate::hours(hour) + lubridate::minutes(minute) +
         lubridate::seconds(second) + lubridate::milliseconds(millisecond) +
         lubridate::microseconds(microsecond) + lubridate::nanoseconds(nanosecond))
}

round_period <- function(period){
  if(!lubridate::is.period(period)) return(period)
  if(!is.null(attr(period, "second"))){
    attr(period, "minute") <- attr(period, "minute")%||%0 + attr(period, "second")%/%60
    attr(period, "second") <- attr(period, "second")%%60
  }

  if(!is.null(attr(period, "minute"))){
    attr(period, "hour") <- attr(period, "hour")%||%0 + attr(period, "minute")%/%60
    attr(period, "minute") <- attr(period, "minute")%%60
  }

  if(!is.null(attr(period, "hour"))){
    attr(period, "day") <- attr(period, "day") + attr(period, "hour")%/%24
    attr(period, "hour") <- attr(period, "hour")%%24
  }

  if(!is.null(attr(period, "month"))){
    attr(period, "year") <- attr(period, "year") + attr(period, "month")%/%12
    attr(period, "month") <- attr(period, "month")%%12
  }
  period
}

floor_tsibble_date <- function(x, unit, ...){
  UseMethod("floor_tsibble_date")
}
floor_tsibble_date.default <- function(x, unit, ...){
  unit <- round_period(unit)
  if(unit == lubridate::weeks(1)){
    unit <- "week"
  }
  lubridate::floor_date(x, unit, week_start = 1)
}
floor_tsibble_date.yearquarter <- function(x, unit, ...){
  yearquarter(lubridate::floor_date(as_date(x), round_period(unit), ...))
}
floor_tsibble_date.yearmonth <- function(x, unit, ...){
  yearmonth(lubridate::floor_date(as_date(x), round_period(unit), ...))
}
floor_tsibble_date.yearweek <- function(x, unit, ...){
  unit <- round_period(unit)
  if ((unit@year > 0) && (unit@day > 0)) {
    abort("Specify a period of either years or weeks to plot, not both.")
  }
  x <- lubridate::as_date(x)
  mth <- lubridate::month(x)
  wk <- as.numeric(strftime(x, "%V"))
  year <- lubridate::year(x) - (mth == 1 & wk == 53) + (mth == 12 & wk == 1)

  if (unit@year > 0) {
    year <- year - (year - 1970)%%unit@year
    wk <- "01"
    x <- paste0(year, " W", wk)
  }
  else if (unit@day > 0) {
    unit <- unit@day
    if(unit%%7 > 0){
      warn("A period with fractional weeks has been specified, rounding to the nearest week")
      unit <- round(unit/7)*7
    }
    x <- as.numeric(as_date(x)) + 3
    x <- structure((x %/% unit)*unit, class = "Date") - 3
  }
  yearweek(x)
}

time_origin <- function(x){
  # Set origin at 1973-01-01 for weekday starting on Monday
  origin <- structure(94694400, class = c("POSIXct", "POSIXt"), tzone = "UTC")

  if (inherits(x, "yearweek")) tsibble::yearweek(origin)
  else if (inherits(x, "yearmonth")) tsibble::yearmonth(origin)
  else if (inherits(x, "yearquarter")) tsibble::yearquarter(origin)
  else if (inherits(x, "Date")) as.Date(origin)
  else origin
}

#' @importFrom lubridate years year month as_date
time_offset_origin <- function(x, period, origin = time_origin(x)){
  x_start <- floor_tsibble_date(x, period)

  if (inherits(x, "yearweek")) {
    tsibble::yearweek(as_date(origin) + (x - x_start))
  } else if (inherits(x, "yearmonth")) {
    tsibble::yearmonth(as_date(origin) + years(year(x) - year(x_start)) + months(month(x) - month(x_start)))
  } else if (inherits(x, "yearquarter")) {
    tsibble::yearquarter(as_date(origin) + years(year(x) - year(x_start)) + months(month(x) - month(x_start)))
  }
  else origin + (x - x_start)
}
