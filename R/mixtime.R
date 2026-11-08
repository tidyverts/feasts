# Helpers for representing results with mixtime time vectors

# Is `x` a mixtime time unit (granule), such as a tsibble's interval?
is_granule <- function(x) {
  inherits(x, "mixtime::mt_unit")
}

# Does the installed mixtime support cycle-free cyclical time (e.g. "March",
# of any year)? Until it does, positions within a cycle are numbers.
has_cyclical_free <- function() {
  exists("mt_cyclical_free", envir = asNamespace("mixtime"), inherits = FALSE)
}

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
    offset <- as.numeric(mixtime::tz_offset(x)) /
      as.numeric(mixtime::chronon_cardinality(mixtime::cal_gregorian$second(1L), chronon))
    count <- count + offset
    chronon <- time_unit_named(x, sub("^mixtime::tu_", "", class(chronon)[1L]), attr(chronon, "n"))
  }

  pos <- mixtime::chronon_divmod(from = chronon, to = cycle, x = count)$mod
  if (all(pos == trunc(pos), na.rm = TRUE)) pos <- as.integer(pos)
  mixtime::new_mixtime(
    mixtime::mt_cyclical_free(pos, chronon = chronon, cycle = cycle)
  )
}

# The position of observation `obs` within the seasonal period `period` named
# `name` (e.g. "year"). For a mixtime `.index` this is the cycle-free time of
# that observation (e.g. the month of the year), otherwise it is the
# observation number modulo the period.
seasonal_position <- function(obs, period, name, .index = NULL) {
  if (mixtime::is_mixtime(.index) && has_cyclical_free()) {
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
