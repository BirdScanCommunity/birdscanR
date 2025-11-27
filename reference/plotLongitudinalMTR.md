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
if (FALSE) { # \dontrun{
# Set server, database, and other input settings
# ===========================================================================
dbServer = "MACHINE\\SERVERNAME" # Set the name of your SQL server
dbName = "db_Name" # Set the name of your database
dbDriverChar = "SQL Server" # Set either "SQL Server" or "PostgreSQL"
mainOutputDir = file.path(".", "results")
radarTimeZone = "Etc/GMT0"
targetTimeZone = "Etc/GMT0"
listOfRfFeaturesToExtract = c(167, 168)
siteLocation = c(47.494427, 8.716432)
sunOrCivil = "civil"
timeRangeData = c("2021-01-15 00:00", "2021-01-31 00:00")

# Get data
# ===========================================================================
dbData = extractDbData(
  dbDriverChar = dbDriverChar,
  dbServer = dbServer,
  dbName = dbName,
  saveDbToFile = TRUE,
  dbDataDir = mainOutputDir,
  radarTimeZone = radarTimeZone,
  targetTimeZone = targetTimeZone,
  listOfRfFeaturesToExtract = listOfRfFeaturesToExtract,
  siteLocation = siteLocation,
  sunOrCivil = sunOrCivil
)

# Get sunrise/sunset
# ===========================================================================
sunriseSunset = twilight(
  timeRange = timeRangeData,
  latLon = c(47.494427, 8.716432),
  timeZone = targetTimeZone
)

# Get manual blind times
# ===========================================================================
data(manualBlindTimes)
cManualBlindTimes = manualBlindTimes

# Compute migration traffic rate
# ===========================================================================
classSelection.mtr = c("insect")
mtrData = computeMTR(
  dbName = dbName,
  echoes = dbData$echoData,
  classSelection = classSelection.mtr,
  altitudeRange = c(25, 1025),
  altitudeBinSize = 50,
  timeRange = timeRangeData,
  timeBinDuration_sec = 1800,
  timeZone = targetTimeZone,
  sunriseSunset = sunriseSunset,
  sunOrCivil = "civil",
  protocolData = dbData$protocolData,
  visibilityData = dbData$visibilityData,
  manualBlindTimes = cManualBlindTimes,
  saveBlindTimes = FALSE,
  blindTimesOutputDir = getwd(),
  blindTimeAsMtrZero = NULL,
  propObsTimeCutoff = 0,
  computePerDayNight = FALSE,
  computeAltitudeDistribution = TRUE
)

# Make Plot
# ===========================================================================
timeRangePlot = list(
  c("2021-01-15 00:00", "2021-01-22 00:00"),
  c("2021-01-23 00:00", "2021-01-31 00:00")
)
plotExplorationplotLongitudinalMTR(
  mtr = mtrData,
  maxMTR = -1,
  timeRange = timeRangePlot,
  targetTimeZone = "Etc/GMT0",
  plotClass = "allClasses",
  propObsTimeCutoff = 0.2,
  plotSpread = TRUE,
  filePath = "./"
)
} # }
```
