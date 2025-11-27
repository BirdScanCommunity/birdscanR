# Get manual visibility table

Load visibility table from an already connected 'Birdscan MR1' 'SQL'
database.

## Usage

``` r
getManualVisibilityTable(dbConnection, dbDriverChar)
```

## Arguments

- dbConnection:

  A valid database connection.

- dbDriverChar:

  This was the name of the driver, and is now automatically detected,
  therefore it should be omitted into the future.

## Value

A `data.frame` with the manual visibility table.

## Author

Baptiste Schmid, Birgen Haest

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

manualVisibilityTable = getManualVisibilityTable(dbConnection)
} # }
```
