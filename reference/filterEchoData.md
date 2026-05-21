# filterEchoData

With `filterEchoData()` the echo data can be filtered by several
parameters. The function returns the filtered echo data.

## Usage

``` r
filterEchoData(
  echoData = NULL,
  timeRangeTargetTZ = NULL,
  targetTimeZone = "Etc/GMT0",
  protocolData = NULL,
  classSelection = NULL,
  classProbCutOff = NULL,
  altitudeRange_AGL = NULL,
  manualBlindTimes = NULL,
  echoValidator = FALSE
)
```

## Arguments

- echoData:

  `data.frame` with the echo data from the data list created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).

- timeRangeTargetTZ:

  Character vector of length 2, with start and end of time range,
  formatted as "%Y-%m-%d %H:%M". Echoes outside the time range will be
  excluded.

- targetTimeZone:

  "Etc/GMT0" String specifying the target time zone. Default is
  "Etc/GMT0".

- protocolData:

  `data.frame` with the protocol data from the data list created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  or a subset of it created by
  [`filterProtocolData()`](https://birdscancommunity.github.io/birdscanR/reference/filterProtocolData.md).
  Echoes not detected during the listed protocols will be excluded.

- classSelection:

  character string vector with the classes that should be included.

- classProbCutOff:

  numeric cutoff value for class probabilities. Echoes with a lower
  class probability will be excluded.

- altitudeRange_AGL:

  numeric vector of length 2 with start and end of the altitude range.
  Echoes outside the altitude range will be excluded.

- manualBlindTimes:

  dataframe with the manual blind times created by the function
  [`loadManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/loadManualBlindTimes.md).

- echoValidator:

  logical, if set to TRUE, echoes labelled by the echo validator as
  “non-bio scatterer” will be excluded. If set to FALSE, all echoes are
  included.

## Value

returns the filtered echo data in the same format as provided in the
parameter `echoData`.

## See also

Other filter functions:
[`filterData()`](https://birdscancommunity.github.io/birdscanR/reference/filterData.md),
[`filterProtocolData()`](https://birdscancommunity.github.io/birdscanR/reference/filterProtocolData.md)

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

# Set input settings for filtering of the data
# ===========================================================================
timeRangeData  = c("2024-09-24 00:00", "2024-09-25 23:59")
targetTimeZone = "Etc/GMT0"
classSelection = c(
  "passerine_type", "wader_type", "swift_type",
  "large_bird", "unid_bird", "bird_flock"
)
classProbCutoff  = NULL
altitudeRange    = c(50, 1000)
cManualBlindTimes = NULL
useEchoValidator = FALSE

# Filter the echo data
# ===========================================================================
filteredEchoData = filterEchoData(
  echoData          = dbData$echoData,
  timeRangeTargetTZ = timeRangeData,
  targetTimeZone    = targetTimeZone,
  protocolData      = dbData$protocolData,
  classSelection    = classSelection,
  classProbCutOff   = classProbCutoff,
  altitudeRange_AGL = altitudeRange,
  manualBlindTimes  = cManualBlindTimes,
  echoValidator     = useEchoValidator
)
# }
```
