
<!-- README.md is generated from README.Rmd. Please edit that file -->

# birdscanR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/birdscanR)](https://CRAN.R-project.org/package=birdscanR)
[![R-CMD-check](https://github.com/Rafnuss/birdscanR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Rafnuss/birdscanR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/Rafnuss/birdscanR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Rafnuss/birdscanR/actions/workflows/pkgdown.yaml)
[![test-coverage](https://github.com/Rafnuss/birdscanR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Rafnuss/birdscanR/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The goal of birdscanR is to …

## Installation

You can install the development version of birdscanR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BirdScanCommunity/birdscanR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(birdscanR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
