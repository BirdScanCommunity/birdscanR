# Get a BirdScan echo validation table

Gets the echoValidationTable from an already connected database.

## Usage

``` r
getEchoValidationTable(dbConnection, dbDriverChar)
```

## Arguments

- dbConnection:

  A valid database connection.

- dbDriverChar:

  This was the name of the driver, and is now automatically detected,
  therefore it should be omitted into the future.

## Value

A dataframe called echovalidationTable

## See also

Other read SQL database functions:
[`QUERY()`](https://birdscancommunity.github.io/birdscanR/reference/QUERY.md),
[`dbConnectBirdscanSQL()`](https://birdscancommunity.github.io/birdscanR/reference/dbConnectBirdscanSQL.md),
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md),
[`getBatClassification()`](https://birdscancommunity.github.io/birdscanR/reference/getBatClassification.md),
[`getCollectionTable()`](https://birdscancommunity.github.io/birdscanR/reference/getCollectionTable.md),
[`getEchoFeatures()`](https://birdscancommunity.github.io/birdscanR/reference/getEchoFeatures.md),
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

echovalidationTable = getEchoValidationTable(dbConnection)
} # }
```
