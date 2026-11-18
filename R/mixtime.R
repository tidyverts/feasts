# Helpers for representing results with mixtime time vectors

# The time unit called `name` (e.g. "year" or "week") from the calendar of `x`,
# or NULL if there is no such unit. Weeks come from the ISO 8601 calendar when
# the time's own calendar has none (e.g. the week of a Date).
time_unit_named <- function(x, name, n = 1L) {
  if (is.null(name) || !nzchar(name)) return(NULL)
  # Unitless times (e.g. plain integers) have no calendar
  if (is.null(mixtime::time_calendar(x))) return(NULL)
  cals <- list(mixtime::time_calendar(x), mixtime::cal_isoweek)
  for (cal in cals) {
    unit <- tryCatch(cal[[name]], error = function(e) NULL)
    if (is.function(unit)) return(unit(n))
  }
  NULL
}

# The cycle-free position of each time point in `x` within `cycle`, e.g. the
# month of the year of a monthly index. The result is the same position in
# any cycle (e.g. "Mar" rather than "Mar 2020").
cyclical_free_position <- function(x, cycle) {
  chronon <- mixtime::chronon_glb(x)
  count <- as.numeric(x)

  # Positions within a cycle are read on the local (wall clock) time line.
  tz <- mixtime::tz_name(x)
  if (!all(is.na(tz))) {
    # The offset is a duration (e.g. 5.5 hours), counted in chronons of `x`.
    count <- count + mixtime::tz_offset(x) / mixtime::duration(1L, chronon = chronon)
    # The same chronon without its timezone: rebuilding it from its own
    # class leaves the `tz` property at its (naive) default.
    chronon <- S7::S7_class(chronon)(chronon@n, calendar = chronon@calendar)
  }

  # Whole chronons since the start of the cycle (e.g. 10:30 is hour 10),
  # numbered as cyclical_time() reads positions (e.g. months from 1, hours
  # from 0).
  pos <- mixtime::chronon_divmod(from = chronon, to = cycle, x = count)$mod
  pos <- floor(pos) + mixtime::cyclical_labels(chronon, cycle)$start
  mixtime::cyclical_time(
    pos, chronon = !!chronon, cycle = !!cycle,
    calendar = mixtime::time_calendar(chronon)
  )
}

# `x` as a mixtime vector, or NULL if it can't be converted (e.g. a legacy
# tsibble index such as a yearmonth or Date can be converted).
as_mixtime_index <- function(x) {
  if (is.null(x) || mixtime::is_mixtime(x)) return(x)
  tryCatch(mixtime::as_mixtime(x), error = function(e) NULL)
}

# The position of observation `obs` within the seasonal period `period` named
# `name` (e.g. "year"). For a time `.index` this is the cycle-free time of
# that observation (e.g. the month of the year), otherwise (no index, or no
# calendar period) it is a cyclical time of unitless observations, which has
# no temporal meaning beyond the position in the cycle (from 0).
seasonal_position <- function(obs, period, name, .index = NULL) {
  .index <- as_mixtime_index(.index)
  if (!is.null(.index)) {
    cycle <- time_unit_named(.index, name)
    if (!is.null(cycle)) {
      return(cyclical_free_position(.index[obs], cycle))
    }
  }
  pos <- unname(obs %% period)
  if (period != trunc(period)) return(pos)
  mixtime::cyclical_time(
    as.integer(pos), chronon = mixtime::mt_unit(1L),
    cycle = mixtime::mt_unit(as.integer(period))
  )
}

# The time of observation `idx` in `.index`, in the class of the index (e.g. a
# Date for a Date index), or `idx` itself if there is no index.
index_position <- function(idx, .index = NULL) {
  if (is.null(.index)) idx else .index[idx]
}

# Combine a list of scalar feature values into a named numeric vector, or a
# one-row tibble when some of them are time vectors.
feature_result <- function(x) {
  if (all(map_lgl(x, function(v) is.numeric(v) && !mixtime::is_mixtime(v)))) {
    return(unlist(x))
  }
  tibble::as_tibble(x)
}
