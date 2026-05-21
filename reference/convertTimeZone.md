# Converts timestamps from radar time zone to an user-defined time zone

Converts timestamps from radar time zone to an user-defined time zone

## Usage

``` r
convertTimeZone(
  data = NULL,
  colNames = "",
  originTZ = "Etc/GMT0",
  targetTZ = "Etc/GMT0"
)
```

## Arguments

- data:

  a data frame containing BirdScan data

- colNames:

  a character vector containing valid column names, as present in `data`

- originTZ:

  character, the time zone name of data to be converted (default is
  "etc/GMT0")

- targetTZ:

  character, the time zone name to convert data into (default is
  "etc/GMT0")

## Value

a data frame identical to `data`, any columns declared in `colNames`
will have their name changed with a suffix (`_originTZ` or `_targetTZ`)
added.

## See also

Other manipulation functions:
[`addDayNightInfoPerEcho()`](https://birdscancommunity.github.io/birdscanR/reference/addDayNightInfoPerEcho.md),
[`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md),
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md),
[`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md),
[`filterSpeedFeature37()`](https://birdscancommunity.github.io/birdscanR/reference/filterSpeedFeature37.md),
[`mergeVisibilityAndManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/mergeVisibilityAndManualBlindTimes.md),
[`reclassToBats()`](https://birdscancommunity.github.io/birdscanR/reference/reclassToBats.md),
[`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md)

## Author

Fabian Hertner, Birgen Haest

## Examples

``` r
# \donttest{
# Load example data
# ===========================================================================
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"))

# Add day/night info to echo data
# ===========================================================================
echoData = convertTimeZone(
  data     = dbData$echoData,
  colNames = c("time_stamp"),
  originTZ = "Etc/GMT0",
  targetTZ = "Etc/GMT-2"
)
#> Warning: no data to convert to time zone (function: convertTimeZone)
# }
```
