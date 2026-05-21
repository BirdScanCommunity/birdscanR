# plotExploration

This function creates a time series plot showing all of the observed
echoes at their respective altitudes. These plots are helpful to roughly
visually explore your data (and for example spot oddities).

## Usage

``` r
plotExploration(
  echoData = NULL,
  timeRange = NULL,
  targetTimeZone = "Etc/GMT0",
  manualBlindTimes = NULL,
  visibilityData = NULL,
  protocolData = NULL,
  sunriseSunset = NULL,
  maxAltitude = NULL,
  filePath = NULL
)
```

## Arguments

- echoData:

  dataframe with the echo data from the data list created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  or a subset of it created by the function ‘filterEchoData’

- timeRange:

  optional list of string vectors length 2, start and end time of the
  time ranges that should be plotted. The date/time format is
  “yyyy-MM-dd hh:mm”. If not set, all echo data is plotted in one plot.
  Note: Too long time-ranges may produce an error if the created image
  is too large and the function can’t allocate the file.

- targetTimeZone:

  "Etc/GMT0" String specifying the target time zone. Default is
  "Etc/GMT0".

- manualBlindTimes:

  optional dataframe with the manual blind times created by
  [`loadManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/loadManualBlindTimes.md).
  If not set, manual blind times are not shown in the plot.

- visibilityData:

  optional dataframe with the visibility data created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).
  If not set, visibility data are not shown in the plot.

- protocolData:

  optional dataframe with the protocol data used to filter the echoes,
  created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  or a subset of it created by
  [`filterProtocolData()`](https://birdscancommunity.github.io/birdscanR/reference/filterProtocolData.md).
  If not set, periods without a protocol are not shown in the plot.

- sunriseSunset:

  optional dataframe with sunrise/sunset, civil, and nautical twilight
  times created by
  [`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md).
  If not set, day/night times are not shown in the plot.

- maxAltitude:

  optional numeric, fixes the maximum value of the y-Scale of the plot
  to the given value. If negative or not set, the y-Scale is
  auto-scaled.

- filePath:

  character string, path of the directory where the plot should be
  saved.
  [`savePlotToFile()`](https://birdscancommunity.github.io/birdscanR/reference/savePlotToFile.md)
  is used to save the plots as png files with an auto-generated
  filename.

## Value

png files stored in the directory specified in 'filePath'

## See also

Other plot functions:
[`createTimeRangeForPlot()`](https://birdscancommunity.github.io/birdscanR/reference/createTimeRangeForPlot.md),
[`plotLongitudinalMTR()`](https://birdscancommunity.github.io/birdscanR/reference/plotLongitudinalMTR.md)

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

# Set manual blind times to NULL (no manual blind times)
# ===========================================================================
cManualBlindTimes = NULL

# Make Plot
# ===========================================================================
timeRangePlot = list(
  c("2024-09-24 00:00", "2024-09-24 23:59"),
  c("2024-09-25 00:00", "2024-09-25 23:59")
)
plotExploration(
  echoData         = dbData$echoData,
  timeRange        = timeRangePlot,
  targetTimeZone   = "Etc/GMT0",
  manualBlindTimes = cManualBlindTimes,
  visibilityData   = dbData$visibilityData,
  protocolData     = dbData$protocolData,
  sunriseSunset    = dbData$sunriseSunset,
  maxAltitude      = -1,
  filePath         = "./"
)
# }
```
