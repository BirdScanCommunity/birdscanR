# savePlotToFile

Saves created plots as .png.

## Usage

``` r
savePlotToFile(
  plot = NULL,
  filePath = NULL,
  plotType = NULL,
  plotWidth_mm = NULL,
  plotHeight_mm = NULL,
  timeRange = NULL,
  classSelection = NULL,
  altitudeRange = NULL,
  classAbbreviations = NULL
)
```

## Arguments

- plot:

  'ggplot' plot to be saved

- filePath:

  character string, path of the directory, e.g.
  "your-project-directory/Data/MTR". If the directory does not exist it
  will be created if possible.

- plotType:

  character string, name/description of the plot, used to create the
  filename. If not set, the pulse type selection will not be appended to
  the filename

- plotWidth_mm:

  numeric, width of the plot in mm. If not set, the size of the png will
  be set automatically.

- plotHeight_mm:

  numeric, height of the plot in mm. If not set, the size of the png
  will be set automatically.

- timeRange:

  POSIXct vector of size 2, time range of the plot, used to create the
  filename. If not set, the pulse type selection will not be appended to
  the filename

- classSelection:

  character string vector, classes that were used to create the plot,
  used to create the filename. If not set, the pulse type selection will
  not be appended to the filename

- altitudeRange:

  numeric vector of size 2, altitude range used to create the plot, used
  to create the filename. If not set, the pulse type selection will not
  be appended to the filename

- classAbbreviations:

  Two-column dataframe with character first column named 'class' and
  character second 'abbr', containing the full names of the classes and
  their abbreviations to use in the output filename. Default = NULL,
  meaning the abbreviations will be used that are stored in the package;
  See data(classAbbreviations).

## Value

No return value, used to save plots to file.

## See also

Other write file functions:
[`compileData()`](https://birdscancommunity.github.io/birdscanR/reference/compileData.md),
[`saveMTR()`](https://birdscancommunity.github.io/birdscanR/reference/saveMTR.md)

## Author

Fabian Hertner, Birgen Haest

## Examples

``` r
if (FALSE) { # \dontrun{
#' # Set server, database, and other input settings
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

# Get manual blindtimes
# ===========================================================================
data("manualBlindTimes")
cManualBlindTimes = manualBlindTimes

# Make Plot
# ===========================================================================
timeRangePlot = list(
  c("2021-01-15 00:00", "2021-01-22 00:00"),
  c("2021-01-23 00:00", "2021-01-31 00:00")
)
cPlot = plotExploration(
  echoData = dbData$echoData,
  timeRange = timeRangePlot,
  targetTimeZone = "Etc/GMT0",
  manualBlindTimes = cManualBlindTimes,
  visibilityData = dbData$visibilityData,
  protocolData = dbData$protocolData,
  sunriseSunset = dbData$sunriseSunset,
  maxAltitude = -1,
  filePath = "./"
)

# Save plot
# ===========================================================================
savePlotToFile(
  plot = cPlot,
  filePath = "./",
  plotType = "S",
  plotWidth_mm = 400,
  plotHeight_mm = 200
)
} # }
```
