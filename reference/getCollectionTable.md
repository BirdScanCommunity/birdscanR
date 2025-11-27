# Get BirdScan collection table

Load collection from 'Birdscan MR1' 'SQL' database.

## Usage

``` r
getCollectionTable(dbConnection, dbDriverChar, timeInterval = NULL)
```

## Arguments

- dbConnection:

  A valid database connection.

- dbDriverChar:

  This was the name of the driver, and is now automatically detected,
  therefore it should be omitted into the future.

- timeInterval:

  Null An optional vector of timestamps (either as `Date` or `POSIXct`)
  to limit the data retrieved from the collections table. The filtering
  is done based on the original radar timezone.

## Value

A dataframe with the collection table

## See also

Other read SQL database functions:
[`QUERY()`](https://birdscancommunity.github.io/birdscanR/reference/QUERY.md),
[`dbConnectBirdscanSQL()`](https://birdscancommunity.github.io/birdscanR/reference/dbConnectBirdscanSQL.md),
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md),
[`getBatClassification()`](https://birdscancommunity.github.io/birdscanR/reference/getBatClassification.md),
[`getEchoFeatures()`](https://birdscancommunity.github.io/birdscanR/reference/getEchoFeatures.md),
[`getEchoValidationTable()`](https://birdscancommunity.github.io/birdscanR/reference/getEchoValidationTable.md),
[`getProtocolTable()`](https://birdscancommunity.github.io/birdscanR/reference/getProtocolTable.md),
[`getRadarTable()`](https://birdscancommunity.github.io/birdscanR/reference/getRadarTable.md),
[`getRfClassification()`](https://birdscancommunity.github.io/birdscanR/reference/getRfClassification.md),
[`getSiteTable()`](https://birdscancommunity.github.io/birdscanR/reference/getSiteTable.md),
[`getTimeBinsTable()`](https://birdscancommunity.github.io/birdscanR/reference/getTimeBinsTable.md),
[`getVisibilityTable()`](https://birdscancommunity.github.io/birdscanR/reference/getVisibilityTable.md)

## Author

Fabian Hertner, Birgen Haest, Bart Kranstauber

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

collectionTable = getCollectionTable(dbConnection)
} # }
```
