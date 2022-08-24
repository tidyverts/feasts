globalVariables("self")

x13_valid_args <- function(...) {
  list(...)
}

specials_x13arimaseats <- fabletools::new_specials(
  common_xregs,
  arima = x13_valid_args,
  automdl = x13_valid_args,
  check = x13_valid_args,
  composite = x13_valid_args,
  estimate = x13_valid_args,
  force = x13_valid_args,
  forecast = x13_valid_args,
  history = x13_valid_args,
  metadata = x13_valid_args,
  identify = x13_valid_args,
  outlier = x13_valid_args,
  pickmdl = x13_valid_args,
  regression = x13_valid_args,
  seats = x13_valid_args,
  slidingspans = x13_valid_args,
  spectrum = x13_valid_args,
  transform = x13_valid_args,
  x11 = x13_valid_args,
  x11regression = x13_valid_args,
  xreg = fabletools::special_xreg(FALSE),
  .required_specials = NULL,
  .xreg_specials = names(common_xregs)
)

train_x13arimaseats <- function(.data, formula, specials, ...,
                                defaults, na.action = seasonal::na.x13){
  require_package("seasonal")
  stopifnot(is_tsibble(.data))
  series_name <- measured_vars(.data)

  xreg <- do.call("cbind", specials$xreg%||%list())
  specials <- specials[setdiff(names(specials), "xreg")]
  specials <- lapply(specials, do.call, what = "c")
  specification <- unlist(specials, recursive = FALSE)
  specification <- c(specification, rep_named(names(specials)[lengths(specials)==0], list("")))

  if(defaults == "none") {
    valid_spc <- c("arima", "automdl", "check", "composite", "estimate", "force",
                   "forecast", "history", "metadata", "identify", "outlier", "pickmdl",
                   "regression", "seats", "slidingspans", "spectrum", "transform",
                   "x11", "x11regression")
    missing_spc <- setdiff(valid_spc, names(specials))
    specification <- c(specification, set_names(vector("list", length(missing_spc)), missing_spc))
  } else if(defaults == "seasonal") {
    default_spc <- list(seats.noadmiss = "yes", transform.function = "auto",
                        regression.aictest = c("td", "easter"), outlier = "",
                        automdl = "")
    missing_spc <- setdiff(names(default_spc), names(specification))
    specification[missing_spc] <- default_spc[missing_spc]
  }

  # Add tsp
  y <- as.ts(.data)
  if(!is.null(xreg)) xreg <- ts(xreg, start = stats::start(y), frequency = stats::frequency(y))

  # Fit model via {seasonal} package
  fit <- seasonal::seas(x = y, xreg = xreg, na.action = na.action,
                        list = c(specification, list(...)))
  fit$call <- NULL
  fit$spc$series$title <- series_name

  structure(
    list(fit = fit, index = index_var(.data)),
    class = "feasts_x13arimaseats"
  )
}

#' @importFrom fabletools components as_dable
#' @export
components.feasts_x13arimaseats <- function(object, ...){
  fit <- object$fit
  period <- as.integer(fit$udg["freq"])
  series_name <- fit$spc$series$title
  method <- "X-13ARIMA-SEATS"

  .data <- as_tsibble(fit$x)
  colnames(.data) <- c(object$index, series_name)
  dcmp <- unclass(fit$data)
  if(is.null(dcmp)){
    abort("The X-13ARIMA-SEATS model does not contain a decomposition, are you missing a seasonal component?")
  }

  # with(as.data.frame(dcmp), .data$value - trend+seasonal+irregular)
  if(!is.na(fit$udg["seatsadj"])) {
    # SEATS
    # Relationship between components is a bit complicated to include for now.
    aliases <- list2(
      !!series_name := parse_expr("f(trend, seasonal, irregular)"),
      season_adjust = parse_expr("f(trend, irregular)")
    )
    seas_base <- 0
  } else {
    # X11
    method <- paste(method, "using X-11 adjustment")
    op <- switch(
      sub("(.+) seasonal adjustment", "\\1", fit$udg["samode"]),
      logarithmic = "*",
      multiplicative = "*",
      additive = "+",
      "auto-mode" = if(fit$udg["aictrans"] == "Log(y)") "*" else "+",
      "pseudo-add" = "pseudo-add"
    )
    y_expr <- if(op == "pseudo-add") {
      parse_expr("trend * (seasonal + irregular - 1)")
    } else {
      reduce(syms(c("trend", "seasonal", "irregular")),
             function(x,y) call2(op, x, y))
    }

    aliases <- list2(
      !!series_name := y_expr,
      season_adjust = call2(op, sym("trend"), sym("irregular"))
    )
    seas_base <- switch(op, `+` = 0, 1)
  }

  dcmp <- .data %>%
    mutate(
      trend = dcmp[,"trend"],
      seasonal = dcmp[,"adjustfac"],
      irregular = dcmp[,"irregular"],
      season_adjust = dcmp[,"seasonaladj"]
    )

  seasonalities <- list(
    seasonal = list(period = period, base = seas_base)
  )

  as_dable(dcmp, response = series_name,
           method = method, seasons = seasonalities,
           aliases = aliases)
}

#' @importFrom fabletools model_sum
#' @export
model_sum.feasts_x13arimaseats <- function(x){
  "X-13ARIMA-SEATS"
}

#' X-13ARIMA-SEATS Seasonal Adjustment
#'
#' X-13ARIMA-SEATS is a seasonal adjustment program developed and maintained by
#' the U.S. Census Bureau.
#'
#' The SEATS decomposition method stands for "Seasonal
#' Extraction in ARIMA Time Series", and is the default method for seasonally
#' adjusting the data. This decomposition method can extract seasonality from
#' data with seasonal periods of 2 (biannual), 4 (quarterly), 6 (bimonthly),
#' and 12 (monthly). This method is specified using the `seats()` function in
#' the model formula.
#'
#' Alternatively, the seasonal adjustment can be done using an enhanced X-11
#' decomposition method. The X-11 method uses weighted averages over a moving
#' window of the time series. This is used in combination with the RegARIMA
#' model to prepare the data for decomposition. To use the X-11 decomposition
#' method, the `x11()` function can be used in the model formula.
#'
#' @param formula Decomposition specification.
#' @param ... Other arguments passed to [seasonal::seas()].
#' @param defaults If defaults="seasonal", the default options of
#'   [seasonal::seas()] will be used, which should work well in most
#'   circumstances. Setting defaults="none" gives an empty model specification,
#'   which can be added to in the model formula.
#' @inheritParams seasonal::seas
#'
#' @section Specials:
#'
#' The specials of the X-13ARIMA-SEATS model closely follow the individual
#' specification options of the original function. Refer to
#' [Chapter 7 of the X-13ARIMA-SEATS Reference Manual](https://www.census.gov/ts/x13as/docX13AS.pdf#chapter.7)
#' for full details of the arguments.
#'
#' The available specials for this model are:
#'
#' #' \subsection{arima}{
#' The `arima` special is used to specify the ARIMA part of the regARIMA model.
#' This defines a pure ARIMA model if the `regression()` special absent and if
#' no exogenous regressors are specified. The lags of the ARIMA model can be
#' specified in the `model` argument, potentially along with `ar` and `ma`
#' coefficients.
#'
#' \preformatted{
#' arima(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{automdl}{
#' The `automdl` special is used to specify the ARIMA part of the regARIMA
#' model will be sought using an automatic model selection procedure
#' derived from the one used by TRAMO (see Gomez and Maravall (2001a)). The
#' maximum order of lags and differencing can be specified using `maxorder` and
#' `maxdiff` arguments. Models containing mixtures of AR and MA components can
#' be allowed or disallowed using the `mixed` argument.
#'
#' \preformatted{
#' automdl(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{check}{
#' The `check` special is used to produce statistics for diagnostic checking of
#' residuals from the estimated model. The computed statistics include ACF and
#' PACF of residuals, along with some statistical tests. These calculations are
#' included in the model object, but difficult to access. It is recommended that
#' these checks are done in R after estimating the model, and that this special
#' is not used.
#'
#' \preformatted{
#' check(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#'
#' \subsection{estimate}{
#' The `estimate` special is used to specify optimisation parameters and
#' estimation options for the regARIMA model specified by the `regression()`
#' and `arima()` specials. Among other options, the tolerance can be set with
#' `tol`, and maximum iterations can be set with `maxiter`.
#'
#' \preformatted{
#' estimate(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{force}{
#' The `force` is an optional special for invoking options that allow users to
#' force yearly totals of the seasonally adjusted series to equal those of the
#' original series for convenience.
#'
#' \preformatted{
#' force(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{forecast}{
#' The `forecast` special is used to specify options for forecasting and/or
#' backcasting the time series using the estimated model. This process is used
#' to enhance the decomposition procedure, especially its performance at the
#' start and end of the series. The number of forecasts to produce is specified
#' in the `maxlead` argument, and the number of backcasts in the `maxback`
#' argument.
#'
#' \preformatted{
#' forecast(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{history}{
#' The `history` special is an optional special for requesting a sequence of
#' runs from a sequence of truncated versions of the time series. Using this
#' special can substantially slow down the program.
#'
#' \preformatted{
#' history(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{metadata}{
#' The `metadata` special is used to insert metadata into the diagnostic summary
#' file. This is typically not needed when interacting with the program via R.
#'
#' \preformatted{
#' metadata(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{identify}{
#' The `identify` special is used to produce tables and line printer plots of
#' sample ACFs and PACFs for identifying the ARIMA part of a regARIMA model.
#'
#' \preformatted{
#' identify(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{outlier}{
#' The `outlier` special is used to perform automatic detection of additive
#' (point) outliers, temporary change outliers, level shifts, or any combination
#' of the three using the specified model. The `seasonal::seas()` defaults used
#' when `defaults="seasonal"` will include the default automatic detection of
#' outliers.
#'
#' \preformatted{
#' outlier(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{pickmdl}{
#' The `pickmdl` special is used to specify the ARIMA part of the regARIMA
#' model will be sought using an automatic model selectionprocedure
#' similar to the one used by X-11-ARIMA/88 (see Dagum 1988).
#'
#' \preformatted{
#' pickmdl(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{regression}{
#'
#' The `regression` special is used to specify including regression variables
#' in a regARIMA model, or for specifying regression variables whose
#' effects are to be removed by the `identify()` special to aid ARIMA model
#' identification. Any exogenous regressors specified in the model formula will
#' be passed into this specification via the `user` and `data` arguments. The
#' [seasonal::seas()] defaults used when `defaults="seasonal"` will set
#' `aictest = c("td", "easter")`, indicating that trading days and Easter
#' effects will be included conditional on AIC-based selection methods.
#'
#' \preformatted{
#' regression(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{seats}{
#' The `seats` special is optionally used to invoke the production of model
#' based signal extraction using SEATS, a seasonal adjustment program developed
#' by Victor Gomez and Agustin Maravall at the Bank of Spain.
#'
#' \preformatted{
#' seats(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{slidingspans}{
#' The optional `slidingspans` special is to provide sliding spans stability
#' analysis on the model. These compare different features of seasonal
#' adjustment output from overlapping subspans of the time series data.
#'
#' \preformatted{
#' slidingspans(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{spectrum}{
#' The optional `spectrum` special is used to provide a choice between two
#' spectrum diagnostics to detect seasonality or trading day effects in
#' monthly series.
#'
#' \preformatted{
#' spectrum(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{transform}{
#' The `transform` special is used to transform or adjust the series prior to
#' estimating a regARIMA model. This is comparable to transforming the response
#' on the formula's left hand side, but offers X-13ARIMA-SEATS specific
#' adjustment options.
#'
#' \preformatted{
#' transform(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{x11}{
#' The optional `x11` special is used to invoke seasonal adjustment by
#' an enhanced version of the methodology of the Census Bureau X-11 and X-11Q
#' programs. The user can control the type of seasonal adjustment decomposition
#' calculated (`mode`), the seasonal and trend moving averages used
#' (`seasonalma` and `trendma`), and the type of extreme value adjustment
#' performed during seasonal adjustment (`sigmalim`).
#'
#' \preformatted{
#' x11(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#' \subsection{x11regression}{
#' The `x11regression` special is used in conjunction with the `x11()` special
#' for series without missing observations. This special estimates calendar
#' effects by regression modeling of the irregular component with predefined or
#' user-defined regressors. Any exogenous regressors specified in the model
#' formula will be passed into this specification via the `user` and `data`
#' arguments.
#'
#' \preformatted{
#' x11regression(...)
#' }
#'
#' \tabular{ll}{
#'   `...`   \tab Arguments described in the reference manual linked below.
#' }
#' }
#'
#' @examples
#'
#' \donttest{
#' fit <- tsibbledata::aus_production %>%
#'   model(X_13ARIMA_SEATS(Beer))
#'
#' report(fit)
#' components(fit)
#'
#' # Additive X-11 decomposition
#' fit <- tsibbledata::aus_production %>%
#'   model(X_13ARIMA_SEATS(Beer ~ transform(`function` = "none") + x11(mode = "add")))
#'
#' report(fit)
#' components(fit)
#'
#' }
#'
#' @seealso [seasonal::seas()]
#'
#' @references
#'
#' Gomez, Victor, and Agustin Maravall. "Automatic modeling methods for
#' univariate series." A course in time series analysis (2001): 171-201.
#'
#' Dagum, E.B. (1988), The X11 ARIMA/88 Seasonal Adjustment Method - Foundations
#' And User’s Manual, Time Series Research and Analysis Division Statistics
#' Canada, Ottawa.
#'
#' Dagum, E. B., & Bianconcini, S. (2016) "Seasonal adjustment methods and real
#' time trend-cycle estimation". \emph{Springer}.
#'
#' X-13ARIMA-SEATS Documentation from the seasonal package's website:
#' http://www.seasonal.website/seasonal.html
#'
#' Official X-13ARIMA-SEATS manual: <https://www2.census.gov/software/x-13arima-seats/x13as/windows/documentation/docx13as.pdf>
#'
#' @importFrom fabletools new_model_class new_model_definition
#' @export
X_13ARIMA_SEATS <- function(formula, ..., na.action = seasonal::na.x13,
                            defaults = c("seasonal", "none")){
  defaults <- match.arg(defaults)
  dcmp <- new_model_class("x13arimaseats",
                          train = train_x13arimaseats,
                          specials = specials_x13arimaseats,
                          check = all_tsbl_checks)
  new_model_definition(dcmp, !!enquo(formula),
                       defaults = defaults, na.action = na.action, ...)
}

#' @importFrom fabletools report
#' @export
report.feasts_x13arimaseats <- function(object, ...){
  cat(utils::capture.output(summary(object$fit))[-(1:3)], sep = "\n")
}

#' @importFrom fabletools outliers
#' @export
outliers.feasts_x13arimaseats <- function(object, ...){
  which(!is.na(seasonal::outlier(object$fit)))
}
