# integrate bat classification

Reclassifies echoes based on bat classification.

## Usage

``` r
reclassToBats(
  echoData = NULL,
  batProbabilitiesAndMtrFactors = NULL,
  reclassToBatCutoff = -1
)
```

## Arguments

- echoData:

  echodata dataframe, output from extractDbData

- batProbabilitiesAndMtrFactors:

  probabilities of bat classification, output from extractDbData'

- reclassToBatCutoff:

  Threshold (0..1), classification of echoes with bat probability higher
  than reclassToBatCutoff will be set to 'bat'

## Value

echoData dataframe

## See also

Other manipulation functions:
[`addDayNightInfoPerEcho()`](https://birdscancommunity.github.io/birdscanR/reference/addDayNightInfoPerEcho.md),
[`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md),
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md),
[`convertTimeZone()`](https://birdscancommunity.github.io/birdscanR/reference/convertTimeZone.md),
[`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md),
[`filterSpeedFeature37()`](https://birdscancommunity.github.io/birdscanR/reference/filterSpeedFeature37.md),
[`mergeVisibilityAndManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/mergeVisibilityAndManualBlindTimes.md),
[`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md)

## Author

Fabian Hertner

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
  sunOrCivil = sunOrCivil,
  crepuscule = "nauticalSolar"
)
#'
# Reclass To Bats
# ===========================================================================
dbData$echoData = reclassToBats(
  echoData = dbData$echoData,
  batProbabilitiesAndMtrFactors =
    dbData$batProbabilitiesAndMtrFactors,
  reclassToBatCutoff = 0.5
)
} # }
```
