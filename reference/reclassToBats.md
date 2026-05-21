# integrate bat classification

Reclassifies echoes based on bat classification.

## Usage

``` r
reclassToBats(
  echoData = NULL,
  batProbabilitiesAndMtrFactors = NULL,
  reclassToBatCutoff = -1
)
```

## Arguments

- echoData:

  echodata dataframe, output from extractDbData

- batProbabilitiesAndMtrFactors:

  probabilities of bat classification, output from extractDbData'

- reclassToBatCutoff:

  Threshold (0..1), classification of echoes with bat probability higher
  than reclassToBatCutoff will be set to 'bat'

## Value

echoData dataframe

## See also

Other manipulation functions:
[`addDayNightInfoPerEcho()`](https://birdscancommunity.github.io/birdscanR/reference/addDayNightInfoPerEcho.md),
[`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md),
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md),
[`convertTimeZone()`](https://birdscancommunity.github.io/birdscanR/reference/convertTimeZone.md),
[`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md),
[`filterSpeedFeature37()`](https://birdscancommunity.github.io/birdscanR/reference/filterSpeedFeature37.md),
[`mergeVisibilityAndManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/mergeVisibilityAndManualBlindTimes.md),
[`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md)

## Author

Fabian Hertner

## Examples

``` r
# \donttest{
# Load example data
# ===========================================================================
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"))

# Reclass To Bats
# ===========================================================================
dbData$echoData = reclassToBats(
  echoData                     = dbData$echoData,
  batProbabilitiesAndMtrFactors = dbData$batProbabilitiesAndMtrFactors,
  reclassToBatCutoff           = 0.5
)
# }
```
