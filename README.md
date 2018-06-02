
<!-- README.md is generated from README.Rmd. Please edit that file -->
tsibblestats
============

[![Travis build status](https://travis-ci.org/tidyverts/tsibblestats.svg?branch=master)](https://travis-ci.org/tidyverts/tsibblestats)

tsibblestats provides example datasets for use with the tidyverts family of packages.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tidyverts/tsibblestats)
```

Example
-------

``` r
library(tsibblestats)
library(tsibbledata)
elecdemand %>% ACF(Temperature)
#> # A tibble: 341 x 2
#>          lag   acf
#>        <lag> <dbl>
#>  1  30MINUTE 0.985
#>  2  60MINUTE 0.947
#>  3  90MINUTE 0.894
#>  4 120MINUTE 0.832
#>  5 150MINUTE 0.765
#>  6 180MINUTE 0.695
#>  7 210MINUTE 0.625
#>  8 240MINUTE 0.557
#>  9 270MINUTE 0.492
#> 10 300MINUTE 0.432
#> # ... with 331 more rows
```