# plotLongitudinalMTR

Plots a time series of MTR values as a bar plot. For each bar the spread
(first and third Quartile) is shown as error bars as well as the numbers
of echoes. Periods with no observation are indicated with grey, negative
bars.

## Usage

``` r
plotLongitudinalMTR(
  mtr,
  maxMTR,
  timeRange = NULL,
  targetTimeZone = "Etc/GMT0",
  plotClass = "allClasses",
  propObsTimeCutoff = 0.2,
  plotSpread = TRUE,
  filePath = NULL
)
```

## Arguments

- mtr:

  data frame with MTR values created by
  [`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md).

- maxMTR:

  optional numeric variable, fixes the maximum value of the y-Scale of
  the plot to the given value. If negative or not set, the y-Scale is
  auto-scaled.

- timeRange:

  optional list of string vectors length 2, start and end time of the
  time ranges that should be plotted. The date/time format is
  “yyyy-MM-dd hh:mm”.

- targetTimeZone:

  "Etc/GMT0" String specifying the target time zone. Default is
  "Etc/GMT0".

- plotClass:

  character string with the class of which the MTR data should be
  plotted. If not set or set to “allClasses”, MTR of all classes will be
  plotted.

- propObsTimeCutoff:

  numeric between 0 and 1. If the MTR is computed per day and night,
  time bins with a proportional observation time smaller than
  propObsTimeCutoff are ignored when combining the time bins. If the MTR
  is computed for each time bin, the parameter is ignored.

- plotSpread:

  logical, choose if the spread (first and third quartile) should be
  plotted.

- filePath:

  character string, path of the directory where the plot should be
  saved.
  [`savePlotToFile()`](https://birdscancommunity.github.io/birdscanR/reference/savePlotToFile.md)
  is used to save the plots as png files with an auto-generated
  filename.

## Value

png files stored in the directory specified with 'filePath'

## See also

Other plot functions:
[`createTimeRangeForPlot()`](https://birdscancommunity.github.io/birdscanR/reference/createTimeRangeForPlot.md),
[`plotExploration()`](https://birdscancommunity.github.io/birdscanR/reference/plotExploration.md)

## Author

Fabian Hertner, Birgen Haest

## Examples

``` r
# \donttest{
# Load example data
# ===========================================================================
dbData         = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"))
dbName         = "CH_Sempach_2024_SEP24_25"
targetTimeZone = "Etc/GMT0"
timeRangeData  = c("2024-09-24 00:00", "2024-09-25 23:59")

# Set manual blind times to NULL (no manual blind times)
# ===========================================================================
cManualBlindTimes = NULL

# Compute migration traffic rate
# ===========================================================================
classSelection.mtr = c("passerine_type")
mtrData = computeMTR(
  dbName                       = dbName,
  echoes                       = dbData$echoData,
  classSelection               = classSelection.mtr,
  altitudeRange                = c(25, 1025),
  altitudeBinSize              = 50,
  timeRange                    = timeRangeData,
  timeBinDuration_sec          = 1800,
  timeZone                     = targetTimeZone,
  sunriseSunset                = dbData$sunriseSunset,
  sunOrCivil                   = "civil",
  protocolData                 = dbData$protocolData,
  visibilityData               = dbData$visibilityData,
  manualBlindTimes             = cManualBlindTimes,
  saveBlindTimes               = FALSE,
  blindTimesOutputDir          = getwd(),
  blindTimeAsMtrZero           = NULL,
  propObsTimeCutoff            = 0,
  computePerDayNight           = FALSE,
  computeAltitudeDistribution  = TRUE
)
#> Creating altitude bins..
#> Creating time bins..
#> Warning: 'createTimeBins()' was called with an input time range that ends later than 2 days before the last sunrise/sunset in the sunriseSunset dataset. The end of the timerange was adjusted to the last date in the sunriseSunset dataset - 2 days.
#> Calculating blind times..
#> Computing observation times for each timebin..
#> 93 echoes above the defined altitude range, thus excldued from the MTR calculation.

# Make Plot
# ===========================================================================
timeRangePlot = list(
  c("2024-09-24 00:00", "2024-09-24 23:59"),
  c("2024-09-25 00:00", "2024-09-25 23:59")
)
plotLongitudinalMTR(
  mtr               = mtrData,
  maxMTR            = -1,
  timeRange         = timeRangePlot,
  targetTimeZone    = "Etc/GMT0",
  plotClass         = "allClasses",
  propObsTimeCutoff = 0.2,
  plotSpread        = TRUE,
  filePath          = "./"
)
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_col()`).
# }
```
