# filterData

With `filterData()` both the echo and protocol data can be filtered
based on several parameters. The function returns the filtered echo and
protocol data.

## Usage

``` r
filterData(
  echoData = NULL,
  protocolData = NULL,
  pulseTypeSelection = NULL,
  rotationSelection = NULL,
  timeRangeTargetTZ = NULL,
  targetTimeZone = "Etc/GMT0",
  classSelection = NULL,
  classProbCutOff = NULL,
  altitudeRange_AGL = NULL,
  manualBlindTimes = NULL,
  echoValidator = FALSE
)
```

## Arguments

- echoData:

  `data.frame` with the echo data from the data list created with
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).

- protocolData:

  `data.frame` with the protocol data from the data list created with
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  or a subset of it created by
  [`filterProtocolData()`](https://birdscancommunity.github.io/birdscanR/reference/filterProtocolData.md).
  Echoes not detected during the listed protocols will be excluded.

- pulseTypeSelection:

  character vector with the pulse types which should be included in the
  subset. Options: “S”, “M”, “L” (short-, medium-, long-pulse). Default
  is NULL: no filtering applied based on pulseType.

- rotationSelection:

  numeric vector to select the operation modes with and/or without
  antenna rotation. Options: 0, 1. (0 = no rotation, 1 = rotation).
  Default is NULL: no filtering applied based on rotation mode.

- timeRangeTargetTZ:

  Character vector of length 2, with start and end of time range,
  formatted as "%Y-%m-%d %H:%M". Echoes outside the time range will be
  excluded.

- targetTimeZone:

  "Etc/GMT0" String specifying the target time zone. Default is
  "Etc/GMT0".

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

returns the filtered echo and protocol data in the same format as
provided in the parameters `echoData` and `protocolData`.

## See also

Other filter functions:
[`filterEchoData()`](https://birdscancommunity.github.io/birdscanR/reference/filterEchoData.md),
[`filterProtocolData()`](https://birdscancommunity.github.io/birdscanR/reference/filterProtocolData.md)

## Author

Birgen Haest

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
pulseLengthSelection = "S"
rotationSelection    = 1
timeRangeData        = c("2024-09-24 00:00", "2024-09-25 23:59")
targetTimeZone       = "Etc/GMT0"
classSelection = c(
  "passerine_type", "wader_type", "swift_type",
  "large_bird", "unid_bird", "bird_flock"
)
classProbCutoff  = NULL
altitudeRange    = c(50, 1000)
cManualBlindTimes = NULL
useEchoValidator = FALSE

# Filter the data
# ===========================================================================
filteredData = filterData(
  echoData           = dbData$echoData,
  protocolData       = dbData$protocolData,
  pulseTypeSelection = pulseLengthSelection,
  rotationSelection  = rotationSelection,
  timeRangeTargetTZ  = timeRangeData,
  targetTimeZone     = targetTimeZone,
  classSelection     = classSelection,
  classProbCutOff    = classProbCutoff,
  altitudeRange_AGL  = altitudeRange,
  manualBlindTimes   = cManualBlindTimes,
  echoValidator      = useEchoValidator
)
#> Filtering protocol Data..
#> Filtering echo Data..
# }
```
