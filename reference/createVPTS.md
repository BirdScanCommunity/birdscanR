# createVPTS

This function creates VPTS CSV output files in line with the ALOFT data
standard, described [here](https://aloftdata.eu/vpts-csv/). Note that
this function only works on Birdscan MR1 database versions \>= 1.7.0.4
as the variable feature37.speed is required for the density calculation.

## Usage

``` r
createVPTS(
  dbName,
  outputDir,
  echoes,
  classSelection = c("passerine_type", "wader_type", "swift_type", "large_bird",
    "unid_bird", "bird_flock"),
  altitudeRange = c(50, 1500),
  altitudeBinSize = 50,
  timeRange,
  timeBinDuration_sec = 900,
  timeZone = "Etc/GMT0",
  protocolData,
  visibilityData,
  siteData,
  sunriseSunset,
  manualBlindTimes = NULL,
  saveBlindTimes = FALSE,
  blindTimesOutputDir = getwd(),
  blindTimeAsMtrZero = NULL,
  propObsTimeCutoff = 0.2
)
```

## Arguments

- dbName:

  Character string, containing the name of the database you are
  processing

- outputDir:

  Character variable indicating where you want the VPTS files to be
  stored. The function will create a subdirectory called "vpts" within
  the specified `outputDir`.

- echoes:

  dataframe with the echo data from the data list created with
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  or a subset of it created by the function

- classSelection:

  character string vector with all classes which should be used to
  calculate the density. The density and number of Echoes will be
  calculated for each class as well as for all classes together.

- altitudeRange:

  numeric vector of length 2 with the start and end of the altitude
  range in meter a.g.l.

- altitudeBinSize:

  numeric, size of the altitude bins in meter.

- timeRange:

  Character vector of length 2, with start and end of time range,
  formatted as "%Y-%m-%d %H:%M"

- timeBinDuration_sec:

  duration of timeBins in seconds (numeric). for values \<= 0 a duration
  of 1 hour will be set

- timeZone:

  time zone in which the time bins should be created as string, e.g.
  "Etc/GMT0"

- protocolData:

  dataframe with the protocol data from the data list created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  or a subset of it created by the function
  [`filterProtocolData()`](https://birdscancommunity.github.io/birdscanR/reference/filterProtocolData.md).

- visibilityData:

  dataframe with the visibility data from the data list created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).

- siteData:

  A data frame holding the site table, as extracted with
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  or
  [`getSiteTable()`](https://birdscancommunity.github.io/birdscanR/reference/getSiteTable.md).

- sunriseSunset:

  dataframe with sunrise/sunset, and civil and nautical dawn/dusk.
  Computed with
  [`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md).

- manualBlindTimes:

  dataframe with the manual blind times created by the function
  [`loadManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/loadManualBlindTimes.md).

- saveBlindTimes:

  Logical, determines whether to save the blind times to a file.
  Default: False.

- blindTimesOutputDir:

  Character string containing the path to save the blind times to.
  Default: 'your-working-directory'

- blindTimeAsMtrZero:

  character string vector with the blind time types which should be
  treated as observation time with MTR zero.

- propObsTimeCutoff:

  numeric between 0 and 1. Time bins with a proportional observation
  time smaller than `propObsTimeCutoff` are set to NA.

## Value

File path to the created VPTS CSV file.

## See also

Other manipulation functions:
[`addDayNightInfoPerEcho()`](https://birdscancommunity.github.io/birdscanR/reference/addDayNightInfoPerEcho.md),
[`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md),
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md),
[`convertTimeZone()`](https://birdscancommunity.github.io/birdscanR/reference/convertTimeZone.md),
[`filterSpeedFeature37()`](https://birdscancommunity.github.io/birdscanR/reference/filterSpeedFeature37.md),
[`mergeVisibilityAndManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/mergeVisibilityAndManualBlindTimes.md),
[`reclassToBats()`](https://birdscancommunity.github.io/birdscanR/reference/reclassToBats.md),
[`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md)

## Author

Birgen Haest

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

# Create vpts files
# ===========================================================================
  vptsDir = createVPTS(dbName                       = dbName,
                       outputDir                    = mainOutputDir,
                       echoes                       = dbData$echoData,
                       altitudeRange                = c(25, 1025),
                       altitudeBinSize              = 50,
                       timeRange                    = timeRangeData,
                       timeBinDuration_sec          = 1800,
                       timeZone                     = targetTimeZone,
                       protocolData                 = dbData$protocolData,
                       visibilityData               = dbData$visibilityData,
                       siteData                     = dbData$siteData,
                       sunriseSunset                = dbData$sunriseSunset,
                       manualBlindTimes             = cManualBlindTimes,
                       saveBlindTimes               = FALSE,
                       blindTimesOutputDir          = mainOutputDir,
                       blindTimeAsMtrZero           = NULL,
                       propObsTimeCutoff            = 0.2)
#> Creating altitude bins..
#> Creating time bins..
#> Warning: 'createTimeBins()' was called with an input time range that ends later than 2 days before the last sunrise/sunset in the sunriseSunset dataset. The end of the timerange was adjusted to the last date in the sunriseSunset dataset - 2 days.
#> Calculating blind times..
#> Subsetting echo data..
#> Computing observation times for each timebin..
#> 35 echoes above the defined altitude range, thus excldued from the VPTS calculation.
#> Computing densities in each time-altitude bin..
#> Computing number of animals (n_dbz,), number of birds with speed and direction values (n)mean flux directon (dd, NOTE: not mean circular direction, but  mean flux direction taking into account individuals' flight speeds), mean flux speeds (ff, NOTE: not mean ground speed of the individual birds but speed flux size taking into account individual's directions), mean u and v components and mtr-weighted average rcs (rcs) of animal movements in each time-altitude bin..
#> Writing daily VPTS csv files to ./results/vpts..
# }
```
