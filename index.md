# birdscanR

BirdscanR is an R package to extract data from Birdscan MR1 SQL
vertical-looking radar databases, filter, and process them to Migration
Traffic Rates (# objects per hour and km) or density (# objects per km3)
of, for example birds, and insects. Object classifications in the
‘Birdscan MR1’ databases are based on the dataset of [Haest et
al. (2021)](https://doi.org10.5281/zenodo.5734960). Migration Traffic
Rates and densities can be calculated separately for different height
bins (with a height resolution of choice) as well as over time periods
of choice (e.g., 1/2 hour, 1 hour, 1 day, day/night, the full time
period of observation, and anything in between). Two plotting functions
are also included to explore the data in the SQL databases and the
resulting Migration Traffic Rate results. For details on the Migration
Traffic Rate calculation procedures, see [Schmid et
al. (2019)](https://doi.org/10.1111/ecog.04025).

To get started, see:

- [Get
  started](https://birdscancommunity.github.io/birdscanR/articles/birdscanR.html):
  an introduction to the package’s main functionalities.
- [Function
  reference](https://birdscancommunity.github.io/birdscanR/reference/index.html):
  overview of all functions.
- [More HowTo
  Guides](https://birdscancommunity.github.io/birdscanR/articles/):
  Additional guides to get you up and going with the different
  functionality of the package.

## Installation

Install the latest released version from CRAN:

``` r

install.packages("birdscanR")
```

Or the development version from
[GitHub](https://github.com/BirdScanCommunity/birdscanR):

``` r

# install.packages("devtools")
devtools::install_github("BirdScanCommunity/birdscanR")
```

## Meta

- We welcome
  [contributions](https://BirdScanCommunity.github.io/birdscanR/CONTRIBUTING.html)
  including bug reports.
- License: GPL-3
- Get [citation
  information](https://BirdScanCommunity.github.io/birdscanR/authors.html#citation)
  for birdscanR in R doing `citation("birdscanR")`.
- Please note that this project is released with a [Contributor Code of
  Conduct](https://BirdScanCommunity.github.io/birdscanR/CODE_OF_CONDUCT.html).
  By participating in this project you agree to abide by its terms.
