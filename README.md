
<!-- README.md is generated from README.Rmd. Please edit that file -->

# feasts

[![Travis build
status](https://travis-ci.org/tidyverts/feasts.svg?branch=master)](https://travis-ci.org/tidyverts/feasts)

## Overview

feasts provides a collection of tools for the analysis of time series
data. The package name is an acryonym comprising of its key features:
*Feature Extraction And Statistics for Time Series*.

The package works with tidy temporal data provided by the
[tsibble](https://github.com/tidyverts/tsibble) package to produce time
series features, decompositions, statistical summaries and convenient
visualisations. These features are useful in understanding the behaviour
of time series data, and closely integrates with the tidy forecasting
workflow used in the [fable](https://github.com/tidyverts/fable)
package.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tidyverts/feasts")
```

## Usage

``` r
library(tidyverse)
library(tsibbledata)
library(lubridate)
library(feasts)
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
some simpler components. The feasts package supports four common time
series decomposition methods:

  - Classical decomposition
  - STL decomposition
  - X11 decomposition
  - X-13ARIMA-SEATS decomposition

<!-- end list -->

``` r
aus_production %>% STL(Beer ~ season(window = Inf))
#> # A dable:           218 x 6 [1Q]
#> # STL Decomposition: Beer = trend + season_year + remainder
#>    Quarter  Beer trend season_year remainder seas_adjust
#>      <qtr> <dbl> <dbl>       <dbl>     <dbl>       <dbl>
#>  1 1956 Q1   284  272.        2.14     10.1         282.
#>  2 1956 Q2   213  264.      -42.6      -8.56        256.
#>  3 1956 Q3   227  258.      -28.5      -2.34        255.
#>  4 1956 Q4   308  253.       69.0     -14.4         239.
#>  5 1957 Q1   262  257.        2.14      2.55        260.
#>  6 1957 Q2   228  261.      -42.6       9.47        271.
#>  7 1957 Q3   236  263.      -28.5       1.80        264.
#>  8 1957 Q4   320  264.       69.0     -12.7         251.
#>  9 1958 Q1   272  266.        2.14      4.32        270.
#> 10 1958 Q2   233  266.      -42.6       9.72        276.
#> # … with 208 more rows
```

``` r
aus_production %>% STL(Beer ~ season(window = Inf)) %>% autoplot()
```

<img src="man/figures/README-dcmp-plot-1.png" width="100%" />

### Feature extraction and statistics

Extract features and statistics across a large collection of time series
to identify unusual/extreme time series, or find clusters of similar
behaviour.

``` r
aus_retail %>% 
  features(Turnover, stl_features)
#> # A tibble: 152 x 9
#>    State Industry Turnover_trend_… Turnover_season… Turnover_spike Turnover_linear… Turnover_curvat…
#>    <chr> <chr>               <dbl>            <dbl>          <dbl>            <dbl>            <dbl>
#>  1 Aust… Cafes, …            0.989            0.540     0.0000617             224.             48.5 
#>  2 Aust… Cafes, …            0.993            0.609     0.000116              336.             76.1 
#>  3 Aust… Clothin…            0.990            0.914     0.00000493            129.             16.8 
#>  4 Aust… Clothin…            0.991            0.947     0.0000260             192.             18.4 
#>  5 Aust… Departm…            0.975            0.977     0.0000285             130.            -42.8 
#>  6 Aust… Electri…            0.991            0.925     0.0000319             232.             -8.24
#>  7 Aust… Food re…            0.999            0.874     0.000250             1247.            197.  
#>  8 Aust… Footwea…            0.978            0.929     0.00000775             63.0             1.60
#>  9 Aust… Furnitu…            0.979            0.657     0.0000487             139.            -22.5 
#> 10 Aust… Hardwar…            0.992            0.892     0.0000156             170.             44.1 
#> # … with 142 more rows, and 2 more variables: Turnover_seasonal_peak.year <dbl>,
#> #   Turnover_seasonal_trough.year <dbl>
```

This allows you to visualise the behaviour of many time series (where
the plotting methods above would show too much information).

``` r
aus_retail %>% 
  features(Turnover, stl_features) %>% 
  ggplot(aes(x = Turnover_trend_strength, y = Turnover_seasonal_strength.year)) +
  geom_point() + 
  facet_wrap(vars(State))
```

<img src="man/figures/README-features-plot-1.png" width="100%" />

Most of Australian’s retail industries are highly trended and seasonal
for all states.

It’s also easy to extract the most (and least) seasonal time series.

``` r
extreme_seasonalities <- aus_retail %>% 
  features(Turnover, stl_features) %>% 
  filter(Turnover_seasonal_strength.year %in% range(Turnover_seasonal_strength.year))
aus_retail %>% 
  right_join(extreme_seasonalities, by = c("State", "Industry")) %>% 
  ggplot(aes(x = Month, y = Turnover)) + 
  geom_line() + 
  facet_grid(vars(State, Industry, scales::percent(Turnover_seasonal_strength.year)), 
             scales = "free_y")
```

<img src="man/figures/README-extreme-1.png" width="100%" />
