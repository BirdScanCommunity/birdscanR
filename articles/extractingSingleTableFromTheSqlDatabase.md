# Guide to extracting specific tables from a Birdscan MR1 SQL database

------------------------------------------------------------------------

## Summary

Instead of using
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
to extract all relevant data at once to calculate densities or Migration
Traffic Rates, one sometimes just wants to have a look at certain
specific tables in the database like the `site`, `radar` or any of the
other tables in the ‘Birdscan MR1’ ‘SQL’ database. This vignette
provides some examples of how the different ‘get’ functions can be used
to extract these different tables:

- [`getBatClassification()`](https://birdscancommunity.github.io/birdscanR/reference/getBatClassification.md):
  extract the bat classification table

- [`getCollectionTable()`](https://birdscancommunity.github.io/birdscanR/reference/getCollectionTable.md):
  extract the echo collection table

- [`getEchoFeatures()`](https://birdscancommunity.github.io/birdscanR/reference/getEchoFeatures.md):
  extract the echo features table

- [`getEchoValidationTable()`](https://birdscancommunity.github.io/birdscanR/reference/getEchoValidationTable.md):
  extract the echo validation table

- [`getProtocolTable()`](https://birdscancommunity.github.io/birdscanR/reference/getProtocolTable.md):
  extract the protocol table

- [`getRadarTable()`](https://birdscancommunity.github.io/birdscanR/reference/getRadarTable.md):
  extract the radar table

- [`getRfClassification()`](https://birdscancommunity.github.io/birdscanR/reference/getRfClassification.md):
  extract the RF classification table

- [`getSiteTable()`](https://birdscancommunity.github.io/birdscanR/reference/getSiteTable.md):
  extract the site table

- [`getTimeBinsTable()`](https://birdscancommunity.github.io/birdscanR/reference/getTimeBinsTable.md):
  extract the time bins table (note: These are not the time bins used in
  [`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md)
  and
  [`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md))

- [`getVisibilityTable()`](https://birdscancommunity.github.io/birdscanR/reference/getVisibilityTable.md):
  extract the visibility table

------------------------------------------------------------------------

## Before we get started

We first load the birdscanR package:

``` r

library(birdscanR)
```

Then we set our inputs:

``` r

# Set main output directory
# =============================================================================
mainOutputDir = file.path(".", "results")
```

Open the connection to the SQL database:

``` r

# Set server and database settings
# =============================================================================
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
```

------------------------------------------------------------------------

## Examples

### Extracting the bat classification table

Use
[`getBatClassification()`](https://birdscancommunity.github.io/birdscanR/reference/getBatClassification.md)
to extract the table with the bat vs nonbat classifications.

``` r

# load collection table
# =============================================================================
message("Extracting the bat classification table from DB...")
batClassifications = getBatClassification(dbConnection, timeInterval)
```

### Extracting the echo collection table

Use
[`getCollectionTable()`](https://birdscancommunity.github.io/birdscanR/reference/getCollectionTable.md)
to extract the echo data from the collection table. The parameter
`timeInterval` can be used to restrict extraction to a specific time
interval only.

``` r

# load collection table
# =============================================================================
message("Extracting collection table from DB...")
timeInterval = NULL # e.g. c(as.Date("2024-03-01"),as.Date("2024-03-15"))
echoData = getCollectionTable(dbConnection, timeInterval)
```

### Extracting specific echo features form the echo_rffeature_map table

Use
[`getEchoFeatures()`](https://birdscancommunity.github.io/birdscanR/reference/getEchoFeatures.md)
to extract additional features stored for each sample. For a full list
of available features, and the corresponding feature numbers, see the
`rffeatures` table in the SQL dataset. There are currently 190 features
available. Use `c(0, 189)` to extract all features from the
`echo_rffeature_map` table.

``` r

# Example: Extract the wing beat frequency and credibility
# =============================================================================
message("Extracting rffeatures table from DB...")
listOfRfFeaturesToExtract = c(167, 168)
echoRfFeatureMap = getEchoFeatures(dbConnection, dbDriverChar,
  listOfRfFeaturesToExtract = listOfRfFeaturesToExtract
)

# Add the newly extracted features to the existing echoData
# =============================================================================
echoData %>% left_join(echoData, echoRfFeatureMap, by = join_by(row == echo))
```

### Extracting information on the study site

Use
[`getSiteTable()`](https://birdscancommunity.github.io/birdscanR/reference/getSiteTable.md)
to extract the `site` table that holds information about the site where
the data was collected like the start and end dates of the radar
campaign, latitude, longitude, altitude, and other details.

``` r

# Extract the site  table from the SQL database
# =============================================================================
message("Extracting site table from DB...")
siteTable = getSiteTable(dbConnection)
```

------------------------------------------------------------------------
