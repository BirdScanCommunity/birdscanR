# computeDensity

This function will estimate the density (expressed as \#objects / km3)
based on the observations in your database. Note that this function only
works properly on Birdscan MR1 database versions \>= 1.7.0.4 as the
variable feature37.speed is required for the density calculation.

## Usage

``` r
computeDensity(
  dbName,
  echoes,
  classSelection,
  altitudeRange,
  altitudeBinSize,
  timeRange,
  timeBinDuration_sec,
  timeZone,
  sunriseSunset,
  sunOrCivil = "civil",
  crepuscule = "nauticalSolar",
  protocolData,
  visibilityData,
  manualBlindTimes = NULL,
  saveBlindTimes = FALSE,
  blindTimesOutputDir = getwd(),
  blindTimeAsMtrZero = NULL,
  propObsTimeCutoff = 0,
  computePerDayNight = FALSE,
  computePerDayCrepusculeNight = FALSE,
  computeAltitudeDistribution = TRUE
)
```

## Arguments

- dbName:

  Character string, containing the name of the database you are
  processing

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

- sunriseSunset:

  dataframe with sunrise/sunset, and civil and nautical dawn/dusk.
  Computed with
  [`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md).

- sunOrCivil:

  sunrise/sunset or civil dawn/dusk used to split day and night.
  Supported values: "sun" or "civil". Default: "civil"

- crepuscule:

  optional character variable, Set to “nauticalSolar” to use the time
  between nautical dusk/dawn and sunrise/sunset times to define the
  crepuscular period, or to "nauticalCivil" to use the time between
  nautical and civil dusk/dawn to define the crepuscular period, or to
  "civilSolar" to use the time between civil dusk/dawn and
  sunrise/sunset times to define the crepuscular period. Default is
  "nauticalSolar".

- protocolData:

  dataframe with the protocol data from the data list created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  or a subset of it created by the function
  [`filterProtocolData()`](https://birdscancommunity.github.io/birdscanR/reference/filterProtocolData.md).

- visibilityData:

  dataframe with the visibility data from the data list created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).

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

- computePerDayNight:

  logical, TRUE: density is computed per day and night. The time bins of
  each day and night will be combined and the mean density is computed
  for each day and night. The spread (first and third Quartile) for each
  day and night are also computed. The spread is dependent on the chosen
  time bin duration/amount of time bins; When FALSE: density is computed
  for each time bin. This option computes the density for each time bin
  defined in the time bin dataframe. The time bins that were split due
  to sunrise/sunset during the time bin will be combined to one bin.

- computePerDayCrepusculeNight:

  logical, TRUE: density is computed per crepusculeMorning, day,
  crepusculeEvening, and night. The time bins of each of these diel
  phases will be combined and the mean density is computed for each
  phase. The spread (first and third Quartile) for each phase is also
  computed. The spread is dependent on the chosen time bin
  duration/amount of time bins; When FALSE: density is computed for each
  time bin. This option computes the density for each time bin defined
  in the time bin dataframe. The time bins that were split due to
  sunrise/sunset during the time bin will be combined to one bin.
  Default = FALSE.

- computeAltitudeDistribution:

  logical, TRUE: compute the mean height and altitude distribution of
  density for the pre-defined quantiles 0.05, 0.25, 0.5, 0.75, 0.95

## Value

Density

## See also

Other manipulation functions:
[`addDayNightInfoPerEcho()`](https://birdscancommunity.github.io/birdscanR/reference/addDayNightInfoPerEcho.md),
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md),
[`convertTimeZone()`](https://birdscancommunity.github.io/birdscanR/reference/convertTimeZone.md),
[`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md),
[`filterSpeedFeature37()`](https://birdscancommunity.github.io/birdscanR/reference/filterSpeedFeature37.md),
[`mergeVisibilityAndManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/mergeVisibilityAndManualBlindTimes.md),
[`reclassToBats()`](https://birdscancommunity.github.io/birdscanR/reference/reclassToBats.md),
[`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md)

## Author

Birgen Haest, Fabian Hertner, Baptiste Schmid

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

# Compute density
# ===========================================================================
  classSelection.density = c("passerine_type")
  densityData = computeDensity(dbName                       = dbName,
                               echoes                       = dbData$echoData,
                               classSelection               = classSelection.density,
                               altitudeRange                = c(25, 1025),
                               altitudeBinSize              = 50,
                               timeRange                    = timeRangeData,
                               timeBinDuration_sec          = 1800,
                               timeZone                     = targetTimeZone,
                               sunriseSunset                = dbData$sunriseSunset,
                               sunOrCivil                   = "civil",
                               crepuscule                   = "nauticalSolar",
                               protocolData                 = dbData$protocolData,
                               visibilityData               = dbData$visibilityData,
                               manualBlindTimes             = cManualBlindTimes,
                               saveBlindTimes               = FALSE,
                               blindTimesOutputDir          = getwd(),
                               blindTimeAsMtrZero           = NULL,
                               propObsTimeCutoff            = 0,
                               computePerDayNight           = FALSE,
                               computePerDayCrepusculeNight = FALSE,
                               computeAltitudeDistribution  = TRUE)
#> Creating altitude bins..
#> Creating time bins..
#> Warning: 'createTimeBins()' was called with an input time range that ends later than 2 days before the last sunrise/sunset in the sunriseSunset dataset. The end of the timerange was adjusted to the last date in the sunriseSunset dataset - 2 days.
#> Calculating blind times..
#> Computing observation times for each timebin..
#> 93 echoes above the defined altitude range, thus excldued from the Density calculation.
# }
```
