# Example Birdscan MR1 data extract from Sempach, Switzerland

A complete data extract from a Birdscan MR1 weather radar, as returned
by
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).
Collected at Sempach, Switzerland (47.13°N, 8.19°E) on September 24–25,
2024. Load with:
`readRDS(system.file("extdata", "CH_Sempach_2024_SEP24_25_DataExtract.rds", package = "birdscanR"))`

## Format

A named list as returned by
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md),
with components:

- echoData:

  data.frame with one row per detected echo

- protocolData:

  data.frame with protocol/collection intervals

- siteData:

  data.frame with radar site metadata

- visibilityData:

  data.frame with automatic visibility/blind times

- timeBinData:

  data.frame with raw time-bin table from the database

- availableClasses:

  character vector of classification labels

- availableBatClasses:

  character vector of bat classification labels

- rfFeatures:

  data.frame with RF feature metadata

- TimeZone:

  data.frame with radarTimeZone and targetTimeZone

- classProbabilitiesAndMtrFactors:

  data.frame with class probabilities

- batProbabilitiesAndMtrFactors:

  data.frame with bat class probabilities

- sunriseSunset:

  data.frame with sunrise/sunset and civil twilight times

## See also

Other sample data:
[`classAbbreviations`](https://birdscancommunity.github.io/birdscanR/reference/classAbbreviations.md),
[`loadManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/loadManualBlindTimes.md),
[`manualBlindTimes`](https://birdscancommunity.github.io/birdscanR/reference/manualBlindTimes.md)

## Examples

``` r
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"))
```
