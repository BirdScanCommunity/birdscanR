# Connect to a Birdscan SQL database

Connects to a Birdscan SQL database, be it a Microsoft SQL or postgreSQL
database3

## Usage

``` r
dbConnectBirdscanSQL(
  dbDriverChar = "SQL Server",
  dbServer = NULL,
  dbName = NULL,
  dbUser = NULL,
  dbPwd = NULL,
  dbPort = 5432
)
```

## Arguments

- dbDriverChar:

  'SQL Server' The name of the driver. Should be either 'SQL Server' or
  'PostgreSQL'.

- dbServer:

  NULL The name of the SQL Server. For a 'PostgreSQL' this can be a the
  host address or the internal IP.

- dbName:

  NULL The name of the SQL database

- dbUser:

  NULL The username of the SQL server

- dbPwd:

  NULL The password for the user name

- dbPort:

  5432 The dbPort parameter specifies the TCP/IP port number on which
  the PostgreSQL server is listening for connections (default is 5432)

## Value

A database connection to your target database

## See also

Other read SQL database functions:
[`QUERY()`](https://birdscancommunity.github.io/birdscanR/reference/QUERY.md),
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

Birgen Haest

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
} # }
```
