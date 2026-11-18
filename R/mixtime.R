# Helpers for representing results with mixtime time vectors

# The time unit called `name` (e.g. "year" or "week") from the calendar of `x`,
# or NULL if there is no such unit. Weeks come from the ISO 8601 calendar when
# the time's own calendar has none (e.g. the week of a Date).
time_unit_named <- function(x, name, n = 1L) {
  if (is.null(name) || !nzchar(name)) return(NULL)
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
    chronon <- time_unit_named(x, sub("^mixtime::tu_", "", class(chronon)[1L]), attr(chronon, "n"))
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

# The position of observation `obs` within the seasonal period `period` named
# `name` (e.g. "year"). For a mixtime `.index` this is the cycle-free time of
# that observation (e.g. the month of the year), otherwise it is the
# observation number modulo the period.
seasonal_position <- function(obs, period, name, .index = NULL) {
  if (mixtime::is_mixtime(.index)) {
    cycle <- time_unit_named(.index, name)
    if (!is.null(cycle)) {
      return(cyclical_free_position(.index[obs], cycle))
    }
  }
  unname(obs %% period)
}

# The time of observation `idx` in a mixtime `.index`, otherwise `idx` itself.
index_position <- function(idx, .index = NULL) {
  if (mixtime::is_mixtime(.index)) .index[idx] else idx
}

# Combine a list of scalar feature values into a named numeric vector, or a
# one-row tibble when some of them are time vectors.
feature_result <- function(x) {
  if (!any(map_lgl(x, mixtime::is_mixtime))) {
    return(unlist(x))
  }
  tibble::as_tibble(x)
}
