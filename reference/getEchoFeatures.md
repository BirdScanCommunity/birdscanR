# Get BirdScan echo features

Load echo rffeature map from 'Birdscan MR1' 'SQL' database.

## Usage

``` r
getEchoFeatures(
  dbConnection,
  dbDriverChar,
  listOfRfFeaturesToExtract,
  echoIDRange = NULL
)
```

## Arguments

- dbConnection:

  A valid database connection.

- dbDriverChar:

  This was the name of the driver, and is now automatically detected,
  therefore it should be omitted into the future.

- listOfRfFeaturesToExtract:

  Either NULL (i.e., don't extract any of the rf features), "all" (i.e.,
  extract all rf features) or a vector of the feature numbers to
  extract. Default is NULL. Feature IDs can be found in the 'rfFeatures'
  table in the sql database.

- echoIDRange:

  NULL A two-element vector of integers to subset the rf feature
  extraction to a range of echoIDs. Default is to extract for all
  echoes.

## Value

A list of the features extracted

## See also

Other read SQL database functions:
[`QUERY()`](https://birdscancommunity.github.io/birdscanR/reference/QUERY.md),
[`dbConnectBirdscanSQL()`](https://birdscancommunity.github.io/birdscanR/reference/dbConnectBirdscanSQL.md),
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md),
[`getBatClassification()`](https://birdscancommunity.github.io/birdscanR/reference/getBatClassification.md),
[`getCollectionTable()`](https://birdscancommunity.github.io/birdscanR/reference/getCollectionTable.md),
[`getEchoValidationTable()`](https://birdscancommunity.github.io/birdscanR/reference/getEchoValidationTable.md),
[`getProtocolTable()`](https://birdscancommunity.github.io/birdscanR/reference/getProtocolTable.md),
[`getRadarTable()`](https://birdscancommunity.github.io/birdscanR/reference/getRadarTable.md),
[`getRfClassification()`](https://birdscancommunity.github.io/birdscanR/reference/getRfClassification.md),
[`getSiteTable()`](https://birdscancommunity.github.io/birdscanR/reference/getSiteTable.md),
[`getTimeBinsTable()`](https://birdscancommunity.github.io/birdscanR/reference/getTimeBinsTable.md),
[`getVisibilityTable()`](https://birdscancommunity.github.io/birdscanR/reference/getVisibilityTable.md)

## Author

Fabian Hertner, Birgen Haest

## Examples

``` r
if (FALSE) { # \dontrun{
# Set server and database settings
# ==========================================================================
# Using and Microsoft SQL database
# ========================================================================
dbServer = "MACHINE\\SERVERNAME" # Set the name of your SQL server
dbName = "db_Name" # Set the name of your database
dbDriverChar = "SQL Server" # Set to "SQL Server"

# Using a PostgreSQL
# ========================================================================
dbServer = "cloud.birdradar.com" # Set the name or IP of your postgreSQL
dbName = "db_Name" # Set the name of your database
dbDriverChar = "PostgreSQL" # Set to "PostgreSQL"

# Open the connection with the database
# ==========================================================================
dbConnection = dbConnectBirdscanSQL(
  dbDriverChar = dbDriverChar,
  dbServer     = dbServer,
  dbName       = dbName,
)

# Set list of Rf features you also want to extract
# Vector with RF features to extract. Feature IDs can be found in the
# 'rfFeatures' table in the sql database.
# Example: Get wing beat frequency and credibility: c(167, 168)
# Set to NULL to not extract any.
# Set to "all" to extract all features.
# ===========================================================================
listOfRfFeaturesToExtract = c(167, 168)

echoFeatures = getEchoFeatures(
  dbConnection,
  listOfRfFeaturesToExtract
)
} # }
```
