
<!-- README.md is generated from README.Rmd. Please edit that file -->

# feasts <a href='https://feasts.tidyverts.org'><img src='man/figures/logo.png' align="right" height="138.5" /></a>

[![R build
status](https://github.com/tidyverts/feasts/workflows/R-CMD-check/badge.svg)](https://github.com/tidyverts/feasts/actions?workflow=R-CMD-check)
[![Coverage
status](https://codecov.io/gh/tidyverts/feasts/branch/master/graph/badge.svg)](https://app.codecov.io/gh/tidyverts/feasts?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/feasts)](https://cran.r-project.org/package=feasts)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)

## Overview

feasts provides a collection of tools for the analysis of time series
data. The package name is an acronym comprising of its key features:
*Feature Extraction And Statistics for Time Series*.

The package works with tidy temporal data provided by the
[tsibble](https://github.com/tidyverts/tsibble) package to produce time
series features, decompositions, statistical summaries and convenient
visualisations. These features are useful in understanding the behaviour
of time series data, and closely integrates with the tidy forecasting
workflow used in the [fable](https://github.com/tidyverts/fable)
package.

## Installation

You could install the **stable** version from
[CRAN](https://cran.r-project.org/package=feasts):

``` r
install.packages("feasts")
```

You can install the **development** version from
[GitHub](https://github.com/tidyverts/feasts) with:

``` r
# install.packages("remotes")
remotes::install_github("tidyverts/feasts")
```

## Usage

``` r
library(feasts)
library(tsibble)
library(tsibbledata)
library(dplyr)
library(ggplot2)
library(lubridate)
```

### Graphics

Visualisation is often the first step in understanding the patterns in
time series data. The package uses
[ggplot2](https://github.com/tidyverse/ggplot2) to produce customisable
graphics to visualise time series patterns.

``` r
aus_production %>% gg_season(Beer)
```

<img src="man/figures/README-graphics-1.png" width="100%" />

``` r
aus_production %>% gg_subseries(Beer)
```

<img src="man/figures/README-graphics-2.png" width="100%" />

``` r
aus_production %>% filter(year(Quarter) > 1991) %>% gg_lag(Beer)
```

<img src="man/figures/README-graphics-3.png" width="100%" />

``` r
aus_production %>% ACF(Beer) %>% autoplot()
```

<img src="man/figures/README-graphics-4.png" width="100%" />

### Decompositions

A common task in time series analysis is decomposing a time series into
some simpler components. The feasts package supports two common time
series decomposition methods:

-   Classical decomposition
-   STL decomposition

<!--
* X11 decomposition
* X-13ARIMA-SEATS decomposition
-->

``` r
dcmp <- aus_production %>%
  model(STL(Beer ~ season(window = Inf)))
components(dcmp)
#> # A dable: 218 x 7 [1Q]
#> # Key:     .model [1]
#> # :        Beer = trend + season_year + remainder
#>    .model                           Quarter  Beer trend season_year remainder season_adjust
#>    <chr>                              <qtr> <dbl> <dbl>       <dbl>     <dbl>         <dbl>
#>  1 STL(Beer ~ season(window = Inf)) 1956 Q1   284  272.        2.14     10.1           282.
#>  2 STL(Beer ~ season(window = Inf)) 1956 Q2   213  264.      -42.6      -8.56          256.
#>  3 STL(Beer ~ season(window = Inf)) 1956 Q3   227  258.      -28.5      -2.34          255.
#>  4 STL(Beer ~ season(window = Inf)) 1956 Q4   308  253.       69.0     -14.4           239.
#>  5 STL(Beer ~ season(window = Inf)) 1957 Q1   262  257.        2.14      2.55          260.
#>  6 STL(Beer ~ season(window = Inf)) 1957 Q2   228  261.      -42.6       9.47          271.
#>  7 STL(Beer ~ season(window = Inf)) 1957 Q3   236  263.      -28.5       1.80          264.
#>  8 STL(Beer ~ season(window = Inf)) 1957 Q4   320  264.       69.0     -12.7           251.
#>  9 STL(Beer ~ season(window = Inf)) 1958 Q1   272  266.        2.14      4.32          270.
#> 10 STL(Beer ~ season(window = Inf)) 1958 Q2   233  266.      -42.6       9.72          276.
#> # … with 208 more rows
```

``` r
components(dcmp) %>% autoplot()
```

<img src="man/figures/README-dcmp-plot-1.png" width="100%" />

### Feature extraction and statistics

Extract features and statistics across a large collection of time series
to identify unusual/extreme time series, or find clusters of similar
behaviour.

``` r
aus_retail %>%
  features(Turnover, feat_stl)
#> # A tibble: 152 × 11
#>    State             Indus…¹ trend…² seaso…³ seaso…⁴ seaso…⁵ spiki…⁶ linea…⁷ curva…⁸ stl_e…⁹ stl_e…˟
#>    <chr>             <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 Australian Capit… Cafes,…   0.989   0.562       0      10 5.15e-5   227.    48.5   0.281   0.187 
#>  2 Australian Capit… Cafes,…   0.993   0.629       0      10 9.73e-5   342.    77.8   0.320   0.218 
#>  3 Australian Capit… Clothi…   0.991   0.923       9      11 4.23e-6   131.    17.4   0.262   0.152 
#>  4 Australian Capit… Clothi…   0.993   0.957       9      11 1.29e-5   195.    19.3   0.262   0.193 
#>  5 Australian Capit… Depart…   0.977   0.980       9      11 2.21e-5   130.   -43.9  -0.254   0.119 
#>  6 Australian Capit… Electr…   0.992   0.933       9      11 2.68e-5   233.    -9.07  0.308   0.207 
#>  7 Australian Capit… Food r…   0.999   0.890       9      11 2.24e-4  1264.   199.    0.0866  0.268 
#>  8 Australian Capit… Footwe…   0.982   0.944       9      11 3.69e-6    64.0    1.95  0.152   0.176 
#>  9 Australian Capit… Furnit…   0.981   0.687       9       1 4.09e-5   141.   -21.6   0.200   0.0812
#> 10 Australian Capit… Hardwa…   0.992   0.900       9       4 1.32e-5   173.    45.1   0.102   0.0796
#> # … with 142 more rows, and abbreviated variable names ¹​Industry, ²​trend_strength,
#> #   ³​seasonal_strength_year, ⁴​seasonal_peak_year, ⁵​seasonal_trough_year, ⁶​spikiness, ⁷​linearity,
#> #   ⁸​curvature, ⁹​stl_e_acf1, ˟​stl_e_acf10
```

This allows you to visualise the behaviour of many time series (where
the plotting methods above would show too much information).

``` r
aus_retail %>%
  features(Turnover, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point() +
  facet_wrap(vars(State))
```

<img src="man/figures/README-features-plot-1.png" width="100%" />

Most of Australian’s retail industries are highly trended and seasonal
for all states.

It’s also easy to extract the most (and least) seasonal time series.

``` r
extreme_seasonalities <- aus_retail %>%
  features(Turnover, feat_stl) %>%
  filter(seasonal_strength_year %in% range(seasonal_strength_year))
aus_retail %>%
  right_join(extreme_seasonalities, by = c("State", "Industry")) %>%
  ggplot(aes(x = Month, y = Turnover)) +
  geom_line() +
  facet_grid(vars(State, Industry, scales::percent(seasonal_strength_year)),
             scales = "free_y")
```

<img src="man/figures/README-extreme-1.png" width="100%" />
