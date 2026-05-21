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
# \donttest{
# Load example data
# ===========================================================================
dbData         = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"))
mainOutputDir  = file.path(".", "results")

# Set manual blind times to NULL (no manual blind times)
# ===========================================================================
cManualBlindTimes = NULL

# Make Plot
# ===========================================================================
timeRangePlot = list(
  c("2024-09-24 00:00", "2024-09-24 23:59"),
  c("2024-09-25 00:00", "2024-09-25 23:59")
)
cPlot = plotExploration(
  echoData         = dbData$echoData,
  timeRange        = timeRangePlot,
  targetTimeZone   = "Etc/GMT0",
  manualBlindTimes = cManualBlindTimes,
  visibilityData   = dbData$visibilityData,
  protocolData     = dbData$protocolData,
  sunriseSunset    = dbData$sunriseSunset,
  maxAltitude      = -1,
  filePath         = mainOutputDir
)

# Save plot
# ===========================================================================
savePlotToFile(
  plot          = cPlot,
  filePath      = mainOutputDir,
  plotType      = "S",
  plotWidth_mm  = 400,
  plotHeight_mm = 200
)
# }
```
