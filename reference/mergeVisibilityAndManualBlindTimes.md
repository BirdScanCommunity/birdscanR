# mergeVisibilityAndManualBlindTimes

Function to merge manual blind times with blind times from visibility
table. For further processing the radar (visibility) and manual blind
times have to be merged with `mergeVisibilityAndManualBlindTimes()`.
This function will add a blind time type to the radar/visibility blind
times. Blind times during the block time (usually 60s) at the beginning
of each protocol are given the type 'protocolChange', the rest of the
radar blind times are given the type “visibility”. After that the
visibility and manual blind times will be merged. In case manual blind
times and radar blind times are overlapping, radar blind times with type
“visibility” will be overwritten, but not radar blind times with type
“protocolChange”.

## Usage

``` r
mergeVisibilityAndManualBlindTimes(
  visibilityData,
  manualBlindTimes = NULL,
  protocolData
)
```

## Arguments

- visibilityData:

  dataframe with the visibility data from the data list created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).

- manualBlindTimes:

  dataframe with the manual blind times created by the function
  ‘loadManualBlindTimes’.

- protocolData:

  dataframe with the protocol data from the data list created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  or a subset of it created by the function
  [`filterProtocolData()`](https://birdscancommunity.github.io/birdscanR/reference/filterProtocolData.md).

## Value

dataframe with overall blind times

## See also

Other manipulation functions:
[`addDayNightInfoPerEcho()`](https://birdscancommunity.github.io/birdscanR/reference/addDayNightInfoPerEcho.md),
[`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md),
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md),
[`convertTimeZone()`](https://birdscancommunity.github.io/birdscanR/reference/convertTimeZone.md),
[`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md),
[`filterSpeedFeature37()`](https://birdscancommunity.github.io/birdscanR/reference/filterSpeedFeature37.md),
[`reclassToBats()`](https://birdscancommunity.github.io/birdscanR/reference/reclassToBats.md),
[`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md)

## Author

Fabian Hertner, Birgen Haest, Baptiste Schmid

## Examples

``` r
# \donttest{
# Load example data
# ===========================================================================
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"))

# Get manual blind times
# ===========================================================================
data(manualBlindTimes)
tmpFile = tempfile(fileext = ".csv")
write.table(manualBlindTimes, file = tmpFile, sep = ",",
            row.names = FALSE, col.names = FALSE)
cManualBlindTimes = loadManualBlindTimes(
  filePath     = tmpFile,
  blindTimesTZ = "Etc/GMT0",
  targetTZ     = "Etc/GMT0"
)

# Merge manual and automatic blind times
# ===========================================================================
blindTimes = mergeVisibilityAndManualBlindTimes(
  visibilityData   = dbData$visibilityData,
  manualBlindTimes = cManualBlindTimes,
  protocolData     = dbData$protocolData
)
#> Warning: CHECK and correct : some time periods overlapp in manualBlindTimes
# }
```
