# Converts timestamps from radar time zone to an user-defined time zone

Converts timestamps from radar time zone to an user-defined time zone

## Usage

``` r
convertTimeZone(
  data = NULL,
  colNames = "",
  originTZ = "Etc/GMT0",
  targetTZ = "Etc/GMT0"
)
```

## Arguments

- data:

  a data frame containing BirdScan data

- colNames:

  a character vector containing valid column names, as present in `data`

- originTZ:

  character, the time zone name of data to be converted (default is
  "etc/GMT0")

- targetTZ:

  character, the time zone name to convert data into (default is
  "etc/GMT0")

## Value

a data frame identical to `data`, any columns declared in `colNames`
will have their name changed with a suffix (`_originTZ` or `_targetTZ`)
added.

## See also

Other manipulation functions:
[`addDayNightInfoPerEcho()`](https://birdscancommunity.github.io/birdscanR/reference/addDayNightInfoPerEcho.md),
[`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md),
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md),
[`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md),
[`filterSpeedFeature37()`](https://birdscancommunity.github.io/birdscanR/reference/filterSpeedFeature37.md),
[`mergeVisibilityAndManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/mergeVisibilityAndManualBlindTimes.md),
[`reclassToBats()`](https://birdscancommunity.github.io/birdscanR/reference/reclassToBats.md),
[`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md)

## Author

Fabian Hertner, Birgen Haest

## Examples

``` r
if (FALSE) { # \dontrun{
# Set server, database, and other input settings for data extraction
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

# Add day/night info to echo data
# ===========================================================================
echoData = convertTimeZone(
  data = dbData$echoData,
  colNames = c("time_stamp"),
  originTZ = "Etc/GMT0",
  targetTZ = "Etc/GMT-2"
)
} # }
```
