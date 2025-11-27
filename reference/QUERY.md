# Query 'SQL' database

Run an 'SQL' query on an already connected database.

## Usage

``` r
QUERY(dbConnection, dbDriverChar, query, as.is = FALSE)
```

## Arguments

- dbConnection:

  A valid database connection.

- dbDriverChar:

  This was the name of the driver, and is now automatically detected,
  therefore it should be omitted into the future.

- query:

  an 'SQL' string with your query

- as.is:

  If TRUE, leaves data as it is

## Value

the result of the query

## See also

Other read SQL database functions:
[`dbConnectBirdscanSQL()`](https://birdscancommunity.github.io/birdscanR/reference/dbConnectBirdscanSQL.md),
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md),
[`getBatClassification()`](https://birdscancommunity.github.io/birdscanR/reference/getBatClassification.md),
[`getCollectionTable()`](https://birdscancommunity.github.io/birdscanR/reference/getCollectionTable.md),
[`getEchoFeatures()`](https://birdscancommunity.github.io/birdscanR/reference/getEchoFeatures.md),
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
# ===========================================================================
dbServer = "MACHINE\\SERVERNAME" # Set the name of your SQL server
dbName = "db_Name" # Set the name of your database
dbDriverChar = "SQL Server" # Set either "SQL Server" or "PostgreSQL"

# Open the connection with the database
# ===========================================================================
dsn = paste0(
  "driver=", dbDriverChar, ";server=", dbServer,
  ";database=", dbName,
  ";uid=", rstudioapi::askForPassword("Database user"),
  ";pwd=", rstudioapi::askForPassword("Database password")
)
dbConnection = RODBC::odbcDriverConnect(dsn)

QUERY(
  dbConnection = dbConnection,
  query = "Select * From collection order by row asc"
)
} # }
```
