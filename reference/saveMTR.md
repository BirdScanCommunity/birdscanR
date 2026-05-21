# saveMTR

Saves MTR data to a .rds file in the directory `filepath`. If the
directory is not existing it will be created if possible.

## Usage

``` r
saveMTR(
  mtr,
  filepath,
  fileName = NULL,
  fileNamePrefix = NULL,
  dbName = NULL,
  rotSelection = NULL,
  pulseTypeSelection = NULL,
  classAbbreviations = NULL
)
```

## Arguments

- mtr:

  dataframe with MTR values created by
  [`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md)

- filepath:

  character string, path of the directory. If the directory does not
  exist it will be created if possible.

- fileName:

  Filename (string) for the file. If not set, the filename will be built
  using the input of the variables 'filenamePrefix', 'dbName',
  'classAbbreviations', and other info in the 'mtr' data. If set,
  overrides the automatic filename creation.

- fileNamePrefix:

  prefix of the filename (string). If not set, "mtr" is used. Different
  information about the MTR data will be appended to the filename.

- dbName:

  character string, name of the database. Used to create the filename,
  if 'fileName' is not provided.

- rotSelection:

  numeric vector, rotation selection which was used to filter protocols.
  Used to create the filename, if 'fileName' is not provided. If not
  set, the rotation selection will not be appended to the filename.

- pulseTypeSelection:

  character vector, pulse type selection which was used to filter
  protocols. Used to create the filename, if 'fileName' is not provided.
  If not set, the pulse type selection will not be appended to the
  filename.

- classAbbreviations:

  Two-column dataframe with character first column named 'class' and
  character second 'abbr', containing the full names of the classes and
  their abbreviations to use in the output filename. Default = NULL,
  meaning the abbreviations will be used that are stored in the package;
  See data(classAbbreviations). Used to create the filename, if
  'fileName' is not provided.

## Value

No return value, used to save MTR to file.

## See also

Other write file functions:
[`compileData()`](https://birdscancommunity.github.io/birdscanR/reference/compileData.md),
[`savePlotToFile()`](https://birdscancommunity.github.io/birdscanR/reference/savePlotToFile.md)

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
mainOutputDir  = file.path(".", "results")
targetTimeZone = "Etc/GMT0"
timeRangeData  = c("2024-09-24 00:00", "2024-09-25 23:59")

# Set manual blind times to NULL (no manual blind times)
# ===========================================================================
cManualBlindTimes = NULL

# Compute migration traffic rate
# ===========================================================================
classSelection.mtr = c("passerine_type")
mtrData = computeMTR(
  dbName                      = dbName,
  echoes                      = dbData$echoData,
  classSelection              = classSelection.mtr,
  altitudeRange               = c(25, 1025),
  altitudeBinSize             = 50,
  timeRange                   = timeRangeData,
  timeBinDuration_sec         = 1800,
  timeZone                    = targetTimeZone,
  sunriseSunset               = dbData$sunriseSunset,
  sunOrCivil                  = "civil",
  protocolData                = dbData$protocolData,
  visibilityData              = dbData$visibilityData,
  manualBlindTimes            = cManualBlindTimes,
  saveBlindTimes              = FALSE,
  blindTimesOutputDir         = mainOutputDir,
  blindTimeAsMtrZero          = NULL,
  propObsTimeCutoff           = 0,
  computePerDayNight          = FALSE,
  computeAltitudeDistribution = TRUE
)
#> Creating altitude bins..
#> Creating time bins..
#> Warning: 'createTimeBins()' was called with an input time range that ends later than 2 days before the last sunrise/sunset in the sunriseSunset dataset. The end of the timerange was adjusted to the last date in the sunriseSunset dataset - 2 days.
#> Calculating blind times..
#> Computing observation times for each timebin..
#> 93 echoes above the defined altitude range, thus excldued from the MTR calculation.

# Save the MTR data to file
# ===========================================================================
saveMTR(
  mtr      = mtrData,
  filepath = mainOutputDir,
  dbName   = dbName
)
# }
```
