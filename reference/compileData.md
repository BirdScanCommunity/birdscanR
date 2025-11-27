# compileData

`compileData()` filters database-extracts and save metadata used to
compute MTR
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md).
It takes the output from
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
and trunks the needed dataset to the restricted settings, e.g. time
frame, pulse type.

## Usage

``` r
compileData(
  echoData = NULL,
  protocolData = NULL,
  blindTimesData = NULL,
  sunriseSunsetData = NULL,
  radarSiteData = NULL,
  dbName = NULL,
  pulseTypeSelection = NULL,
  rotationSelection = NULL,
  timeRangeTargetTZ = NULL,
  targetTimeZone = "Etc/GMT0",
  classSelection = NULL,
  classProbCutOff = NULL,
  altitudeRange_AGL = NULL,
  echoValidator = FALSE,
  filePath = NULL,
  tagOutputFile = c(NULL, NULL),
  saveCSV = FALSE
)
```

## Arguments

- echoData:

  dataframe with the echo data from the data list created with
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).

- protocolData:

  dataframe with the protocol data from the data list created with
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).
  Echoes not detected during the listed protocols will be excluded.

- blindTimesData:

  dataframe with the manual blind times created by
  [`loadManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/loadManualBlindTimes.md).
  It include the automated blind times induced by changes in measurement
  protocol, and blind time added manually to remove periods of
  incoherent data collection.

- sunriseSunsetData:

  dataframe with sunrise/sunset, and civil and nautical dawn/dusk.
  Computed with
  [`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md).

- radarSiteData:

  dataframe/vector with the database site table.

- dbName:

  Name of the database. Can be a useful meta data.

- pulseTypeSelection:

  character vector with the pulse types which should be included in the
  subset. Options: `S`, `M`, `L`, i.e. short-, medium-, long-pulse,
  respectively. Default is NULL: no filtering applied based on
  pulseType.

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

- echoValidator:

  logical, If set to FALSE - default -, no additional filters is
  applied; if set to TRUE, echoes labelled by the echo validator as
  "non-bio scatterer" will be excluded.

- filePath:

  If given, the data-list is saved as RDS.

- tagOutputFile:

  Vector of two elements for prefix & suffix to file name, given
  `filePath` is not NULL.

- saveCSV:

  if true, save tables as CSV in a folder nested in `filePath`.

## Value

Returns filtered data table - echo, protocol, blindTimes, sunriseSunset,
radarSite - and necessary parameters as input for
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md).

## See also

Other write file functions:
[`saveMTR()`](https://birdscancommunity.github.io/birdscanR/reference/saveMTR.md),
[`savePlotToFile()`](https://birdscancommunity.github.io/birdscanR/reference/savePlotToFile.md)

## Author

Baptiste Schmid, Fabian Hertner, Birgen Haest
