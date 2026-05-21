# Filter outliers in Speed feature (collection.feature37)

Filter outliers in Speed feature (collection.feature37)

## Usage

``` r
filterSpeedFeature37(echoData = NULL, minEchoDuration = 5)
```

## Arguments

- echoData:

  valid echodata

- minEchoDuration:

  minimum duration of a echo to allow speed feature

## Value

echoData with the filtered speed feature 37

## See also

Other manipulation functions:
[`addDayNightInfoPerEcho()`](https://birdscancommunity.github.io/birdscanR/reference/addDayNightInfoPerEcho.md),
[`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md),
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md),
[`convertTimeZone()`](https://birdscancommunity.github.io/birdscanR/reference/convertTimeZone.md),
[`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md),
[`mergeVisibilityAndManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/mergeVisibilityAndManualBlindTimes.md),
[`reclassToBats()`](https://birdscancommunity.github.io/birdscanR/reference/reclassToBats.md),
[`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md)

## Author

Fabian Hertner, <fabian.hertner@swiss-birdradar.com>; Birgen Haest,
<birgen.haest@vogelwarte.ch>

## Examples

``` r
# \donttest{
# Load example data
# ===========================================================================
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"))

# Filter speed feature 37
# ===========================================================================
minEchoDuration = 5
dbData$echoData = filterSpeedFeature37(
  echoData        = dbData$echoData,
  minEchoDuration = minEchoDuration
)
# }
```
