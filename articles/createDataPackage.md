# Create and publish an MR1 data package

------------------------------------------------------------------------

## Summary

An MR1 data package holds the data of one radar campaign
(e.g. `CH_Sempach_2024`) in a standardised, self-describing format: the
[Frictionless Data Package](https://datapackage.org). It consists of one
CSV file per table and a `datapackage.json` file that describes the
tables (column types, descriptions, units, and how the tables link to
each other) and the campaign (title, contributors, licence, embargo,
location, period, …).

Each campaign package is published as its own dataset on Zenodo, with
its own DOI. These campaign datasets are grouped in owner collections
(e.g. all campaigns of the Swiss Ornithological Institute) and in a
community collection (all BirdScan MR1 campaigns), so users can cite the
level that matches their study.

The workflow has 6 steps:

1.  Extract the data from the SQL database.

2.  (optional) Load the manual blind times.

3.  Describe the campaign in a metadata file.

4.  Create the data package.

5.  Validate the data package.

6.  Upload the data package to Zenodo.

------------------------------------------------------------------------

## Before we get started

We first load the birdscanR package and set an output directory.

``` r

library(birdscanR)

# Set main output directory
# =============================================================================
mainOutputDir = tempdir()
```

------------------------------------------------------------------------

## Step 1: Extract the data from the SQL database

The data package is created from the output of
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).
For a real campaign, you extract the data from your database as
described in the vignette “Introduction to birdscanR”. Extract all data
of the campaign, with all the echo features you want to publish:

``` r

# Extract the data from the SQL database
# =============================================================================
dbData = extractDbData(
  dbDriverChar              = "SQL Server",
  dbServer                  = "MACHINE\\SERVERNAME",
  dbName                    = "CH_Sempach_2024",
  saveDbToFile              = TRUE,
  dbDataDir                 = mainOutputDir,
  radarTimeZone             = "Etc/GMT0",
  targetTimeZone            = "Etc/GMT0",
  listOfRfFeaturesToExtract = c(167, 168),
  siteLocation              = c(47.12764, 8.192569),
  sunOrCivil                = "civil",
  crepuscule                = "nauticalSolar"
)
```

In this vignette, we use the sample data extract that ships with the
package: two days (24-25 September 2024) of data from the radar at
Sempach, Switzerland.

``` r

# Load example data
# =============================================================================
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))
dbName = "CH_Sempach_2024_SEP24_25"
```

------------------------------------------------------------------------

## Step 2: (optional) Load the manual blind times

Manual blind times are periods in which the radar data should not be
used, e.g. because of rain or technical problems, that were noted by the
radar operator. They are stored in a csv file and loaded with
[`loadManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/loadManualBlindTimes.md).
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md)
merges them with the blind times registered by the radar, so that the
package holds one blind time table.

The csv file has one row per blind time and three columns without
header: start and stop time (`yyyy-mm-dd hh:mm:ss`) and the type of
blind time. The package contains an example (`data(manualBlindTimes)`)
with three blind times during the sample data: rain on the morning of 24
September, a technical outage in the night of 24-25 September, and
clutter on the afternoon of 25 September. We write it to a csv file, as
you would have it for a real campaign, and load it:

``` r

# Write the example manual blind times to a csv file (normally you already
# have this file)
# =============================================================================
data(manualBlindTimes)
manualBlindTimes
#>                    V1                  V2      V3
#> 1 2024-09-24 05:00:00 2024-09-24 07:03:30    rain
#> 2 2024-09-24 22:12:00 2024-09-25 05:30:00    tech
#> 3 2024-09-25 15:20:00 2024-09-25 15:34:00 clutter
manualBlindTimesFile = file.path(mainOutputDir, "manualBlindTimes_Sempach.csv")
write.table(manualBlindTimes,
  file = manualBlindTimesFile, sep = ",",
  row.names = FALSE, col.names = FALSE
)

# Load the manual blind times
# =============================================================================
cManualBlindTimes = loadManualBlindTimes(
  filePath     = manualBlindTimesFile,
  blindTimesTZ = "Etc/GMT0",
  targetTZ     = "Etc/GMT0"
)
```

If there are no manual blind times, set `cManualBlindTimes = NULL`.

------------------------------------------------------------------------

## Step 3: Describe the campaign in a metadata file

The description of the campaign is stored in a YAML file, one per
campaign. Write a template with
[`createDataPackageMetadataTemplate()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackageMetadataTemplate.md)
and fill it in with a text editor:

``` r

# Write the metadata template
# =============================================================================
metadataFile = file.path(mainOutputDir, "CH_Sempach_2024_metadata.yaml")
createDataPackageMetadataTemplate(filePath = metadataFile, overwrite = TRUE)
```

The template contains the following fields (shown here with the example
values of the template):

    # ==============================================================================
    # MR1 data package metadata (one file per campaign)
    # ==============================================================================
    # Fill in this file and pass its path to createDataPackage(metadata = ...).
    # The values below are an example for the Sempach test dataset; replace them.
    # Fields marked (required) must be present. Fields left empty or removed are
    # derived from the data where possible (see comments).
    # Property names follow the Frictionless Data Package standard
    # (https://datapackage.org) where one exists.

    # Campaign identifier (required): <country code>_<site>_<year>
    # Used as package name (in lower case) and as output folder name.
    campaignID: CH_Sempach_2024

    # Title and description of the campaign dataset (required)
    title: "BirdScan MR1 radar data, Sempach (Switzerland), 2024"
    description: >-
      Echo data recorded with a BirdScan MR1 vertical-looking radar at Sempach,
      Switzerland, with measurement protocols, blind times, sunrise/sunset times
      and radar/site parameters. Created with the R package birdscanR.

    # Version of this data package (required). Increase it when the data are corrected.
    version: "1.0.0"

    # DOI of the dataset on Zenodo, as URL (optional). Leave empty for the first
    # upload: uploadDataPackageToZenodo() reserves a DOI. Fill it in once the
    # record is published, e.g. https://doi.org/10.5281/zenodo.1234567
    # With a DOI here, uploadDataPackageToZenodo() creates a new version of that
    # record instead of a new record (increase 'version' as well).
    id:

    # Owner of the data (required). ror: optional ROR identifier URL.
    owner:
      name: Swiss Ornithological Institute
      ror: https://ror.org/03mcsbr76

    # Licence (required). One of: CC-BY-4.0, CC-BY-NC-4.0
    licenses:
      - name: CC-BY-4.0
        path: https://creativecommons.org/licenses/by/4.0/
        title: Creative Commons Attribution 4.0 International

    # End of the embargo (YYYY-MM-DD). Data files stay restricted on Zenodo until
    # this date; metadata are always public. Leave empty to use the default:
    # 5 years after the end of the data in the package.
    embargo:

    # Keywords (optional)
    keywords:
      - bird migration
      - radar aeroecology
      - BirdScan MR1
      - vertical-looking radar

    # Contributors (required, at least one ContactPerson).
    #   title:        full name as it should appear in citations (required)
    #   givenName / familyName: used for citations and for merging collections
    #   path:         ORCID as URL, e.g. https://orcid.org/0000-0000-0000-0000
    #                 (strongly recommended: used to merge creators across campaigns)
    #   email:        optional
    #   organization: affiliation
    #   roles:        one or more DataCite contributor types:
    #                 ContactPerson, ProjectLeader, DataCollector, DataCurator,
    #                 Researcher, RightsHolder, Supervisor
    #   embargoRequestType: how to handle data requests during the embargo:
    #                 1 = contact me (potential collaborator), 2 = citation only
    contributors:
      - title: Birgen Haest
        givenName: Birgen
        familyName: Haest
        path: https://orcid.org/0000-0002-8739-6460
        organization: Swiss Ornithological Institute
        roles: [ContactPerson, DataCurator]
        embargoRequestType: 1

    # Related identifiers (optional), DataCite relation types, e.g. IsPartOf
    # (owner collection), IsCitedBy, IsDocumentedBy, IsDerivedFrom.
    relatedIdentifiers:
      - relationType: IsCompiledBy
        relatedIdentifier: https://github.com/BirdScanCommunity/birdscanR
        relatedIdentifierType: URL

    # Suggested citation (optional). Leave empty to generate it from the
    # contributors, the year of creation and the title.
    bibliographicCitation:

A few points to consider when you fill it in:

- **campaignID**: `<country code>_<site>_<year>`,
  e.g. `CH_Sempach_2024`. It is used as the name of the package and of
  the output folder.
- **licenses**: `CC-BY-4.0` or `CC-BY-NC-4.0`.
- **embargo**: the date until which the data files stay restricted on
  Zenodo. The metadata (and thus the existence of the dataset, its
  contributors and how to contact them) are always public. If you leave
  it empty, the embargo ends 5 years after the last data in the package.
- **contributors**: list everyone who should get credit for the dataset.
  Each contributor needs one or more roles out of `ContactPerson`,
  `ProjectLeader`, `DataCollector`, `DataCurator`, `Researcher`,
  `RightsHolder` and `Supervisor`. At least one contributor must be the
  `ContactPerson`. Give the ORCID of each contributor whenever possible:
  it is used to merge the contributors of all campaigns into the
  creators of the collections.
- **embargoRequestType**: how a contributor wants to be involved when
  someone requests the data during the embargo: `1` = contact me
  (potential collaborator), `2` = citation only.

You can also build or adjust the metadata in R, and pass a list instead
of a file. Here, for example, we add a second contributor:

``` r

# Adjust the metadata in R
# =============================================================================
metadata = yaml::read_yaml(metadataFile)
metadata$contributors[[2]] = list(
  title              = "Baptiste Schmid",
  givenName          = "Baptiste",
  familyName         = "Schmid",
  path               = "https://orcid.org/0000-0002-7736-7527",
  organization       = "Swiss Ornithological Institute",
  roles              = c("ProjectLeader", "DataCollector"),
  embargoRequestType = 1
)
```

The metadata are checked when the data package is created. If something
is missing or wrong,
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md)
stops and lists all problems, e.g.:

``` r

badMetadata = metadata
badMetadata$title = NULL
badMetadata$contributors[[2]]$roles = "Author"
createDataPackage(dbExtract = dbData, metadata = badMetadata)
#> Error in `createDataPackage()`:
#> ! The package metadata are not valid:
#> - 'title' is required and must be a single, non-empty string.
#> - Contributor 2 (Baptiste Schmid): invalid role(s) Author. Allowed: ContactPerson, ProjectLeader, DataCollector, DataCurator, Researcher, RightsHolder, Supervisor.
```

------------------------------------------------------------------------

## Step 4: Create the data package

[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md)
selects the data of the campaign, applies the filters you set, and
builds the data package. The filters are the same as in
[`filterProtocolData()`](https://birdscancommunity.github.io/birdscanR/reference/filterProtocolData.md)
and
[`filterEchoData()`](https://birdscancommunity.github.io/birdscanR/reference/filterEchoData.md);
they are stored in the package metadata, so users know how the data were
selected. For a data package, you normally keep all data of the campaign
and only exclude what should not be published (e.g. a test period at the
start of the campaign).

``` r

# Create the MR1 data package
# =============================================================================
dataPackage = createDataPackage(
  dbExtract         = dbData,
  metadata          = metadata,
  manualBlindTimes  = cManualBlindTimes,
  dbName            = dbName,
  timeRangeTargetTZ = c("2024-09-24 00:00", "2024-09-25 23:59"),
  outputDirPath     = mainOutputDir
)
dataPackage
#> A Data Package (version 1.0) with 5 resources:
#> • site_data
#> • protocol_data
#> • echo_data
#> • blind_times
#> • sunrise_sunset
#> Use `unclass()` to print the Data Package as a list.
```

### What is in the data package?

The package holds five tables (resources):

| Resource | Content |
|----|----|
| `echo_data` | Echoes, with their features, classification and MTR factors |
| `protocol_data` | Measurement periods (protocols) with their radar settings |
| `blind_times` | Periods in which the radar could not detect echoes |
| `sunrise_sunset` | Sunrise, sunset and twilight times at the site |
| `site_data` | Location of the radar and radar/calibration parameters |

Read a table as a data frame with
[`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html):

``` r

# Read the blind times
# =============================================================================
blindTimes = frictionless::read_resource(dataPackage, "blind_times")
head(blindTimes)
#> # A tibble: 6 × 4
#>   type           start_targetTZ      stop_targetTZ       protocolID
#>   <chr>          <dttm>              <dttm>                   <int>
#> 1 protocolChange 2024-09-24 00:00:03 2024-09-24 00:01:04      14443
#> 2 protocolChange 2024-09-24 00:40:04 2024-09-24 00:41:03      14444
#> 3 protocolChange 2024-09-24 01:00:03 2024-09-24 01:01:04      14445
#> 4 protocolChange 2024-09-24 01:40:05 2024-09-24 01:41:03      14446
#> 5 visibility     2024-09-24 01:44:57 2024-09-24 02:00:02      14446
#> 6 protocolChange 2024-09-24 02:40:03 2024-09-24 02:41:03      14448
table(blindTimes$type)
#> 
#>        clutter protocolChange           rain           tech     visibility 
#>              1             89              6             15             10
```

Each table has a schema that describes its columns, and how it links to
the other tables: each echo belongs to a measurement period
(`protocolID`), and each measurement period to a site (`siteID`).

``` r

# Show the schema of the protocol table
# =============================================================================
protocolSchema = frictionless::get_schema(dataPackage, "protocol_data")
#> Warning: `get_schema()` was deprecated in frictionless 1.3.0.
#> ℹ Please use `schema()` instead.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
data.frame(
  name        = sapply(protocolSchema$fields, function(x) x$name),
  type        = sapply(protocolSchema$fields, function(x) x$type),
  description = sapply(protocolSchema$fields, function(x) x$description)
)
#>                  name     type
#> 1          protocolID  integer
#> 2              siteID  integer
#> 3  startTime_originTZ   string
#> 4  startTime_targetTZ datetime
#> 5   stopTime_originTZ   string
#> 6   stopTime_targetTZ datetime
#> 7           pulseType   string
#> 8              rotate  integer
#> 9                 stc   number
#> 10          threshold   number
#> 11          blockTime   number
#> 12    softwareVersion   string
#>                                                                                                                                                                                        description
#> 1  ID of the measurement period (protocol). A new protocol starts at each change of the operation mode of the radar (pulse type, rotation, ...). Primary key, linked to echo_data and blind_times.
#> 2                                                                                                                                                                     Site ID. Links to site_data.
#> 3                                                                                                             Start of the measurement period in the radar time zone, written as local clock time.
#> 4                                                                                                                Start of the measurement period in the target time zone (since 2020 usually UTC).
#> 5                                                                                                               End of the measurement period in the radar time zone, written as local clock time.
#> 6                                                                                                                  End of the measurement period in the target time zone (since 2020 usually UTC).
#> 7                                                                                       Operation mode, pulse type: 'S' short, 'M' medium or 'L' long pulse. See site_data for the pulse duration.
#> 8                                      Operation mode, antenna rotation: 1 when the radar is in rotation mode, 0 when not. Flight speed and direction are only available when the antenna rotates.
#> 9                                                                            STC altitude: altitude up to which the sensitivity time control applies. Key parameter for the MTR factor of an echo.
#> 10                                                                                                                                     STC threshold. Key parameter for the MTR factor of an echo.
#> 11                                              Time the radar is blind after a protocol change (start of the measurement period). Recorded as blind time of type 'protocolChange' in blind_times.
#> 12                                                                                                   Radar software version during the measurement period. Can differ from the classifier version.
str(protocolSchema[c("primaryKey", "foreignKeys")])
#> List of 2
#>  $ primaryKey : 'AsIs' chr "protocolID"
#>  $ foreignKeys:List of 1
#>   ..$ :List of 2
#>   .. ..$ fields   : 'AsIs' chr "siteID"
#>   .. ..$ reference:List of 2
#>   .. .. ..$ resource: chr "site_data"
#>   .. .. ..$ fields  : 'AsIs' chr "siteID"
```

Besides the metadata from the YAML file,
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md)
adds the location and period of the campaign, the applied filters, the
provenance of the data (database, birdscanR and classifier versions),
and a suggested citation:

``` r

# Show some of the package metadata
# =============================================================================
dataPackage$embargo
#> [1] "2029-09-25"
dataPackage$temporal
#> $start
#> [1] "2024-09-24"
#> 
#> $end
#> [1] "2024-09-25"
dataPackage$spatial
#> $type
#> [1] "Point"
#> 
#> $coordinates
#> [1]  8.192569 47.127641
dataPackage$bibliographicCitation
#> [1] "Haest B, Schmid B (2026) BirdScan MR1 radar data, Sempach (Switzerland), 2024. Version 1.0.0."
str(dataPackage$`mr1dp:provenance`)
#> List of 8
#>  $ database            : chr "CH_Sempach_2024_SEP24_25"
#>  $ birdscanR           : chr "0.3.0.9033"
#>  $ radarType           : chr "BirdScanMR1"
#>  $ radarSerialNo       : int 220
#>  $ radarTimeZone       : chr "Etc/GMT0"
#>  $ targetTimeZone      : chr "Etc/GMT0"
#>  $ classifierVersion   : 'AsIs' chr "20231002_161535"
#>  $ batClassifierVersion: 'AsIs' chr "20230919_135920"
```

### The files on disk

Because we set `outputDirPath`, the package was written to a folder
named after the `campaignID`. This folder is what you upload to Zenodo:

``` r

packageDir = file.path(mainOutputDir, "CH_Sempach_2024")
list.files(packageDir)
#> [1] "blind_times.csv.gz"    "datapackage.json"      "echo_data.csv.gz"     
#> [4] "protocol_data.csv.gz"  "site_data.csv.gz"      "sunrise_sunset.csv.gz"
```

Anyone can read the package back, with R or any other software that
supports Frictionless Data Packages:

``` r

# Read the data package from disk
# =============================================================================
dataPackageFromDisk = frictionless::read_package(file.path(packageDir, "datapackage.json"))
echoData = frictionless::read_resource(dataPackageFromDisk, "echo_data")
dim(echoData)
#> [1] 13974   218
```

------------------------------------------------------------------------

## Step 5: Validate the data package

Before you upload the package, check it with
[`validateDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/validateDataPackage.md).
It checks the package metadata, that all tables are present and
readable, that the columns match their schema, that the keys between the
tables are intact (e.g. every echo belongs to an existing measurement
period), and that the content is plausible (e.g. start times before stop
times, echoes within the period of the package). Validate the files on
disk, as these are what you upload:

``` r

# Validate the data package on disk
# =============================================================================
validation = validateDataPackage(packageDir)
#> Warning: `resources()` was deprecated in frictionless 1.3.0.
#> ℹ Please use `resource_names()` instead.
#> ℹ The deprecated feature was likely used in the birdscanR package.
#>   Please report the issue at
#>   <https://github.com/BirdScanCommunity/birdscanR/issues>.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
validation
#> The MR1 data package is valid.
```

Errors mean that the package is incomplete or inconsistent: fix them
before you upload it. Warnings, e.g. a column without description, do
not prevent the upload. All issues are also available as a data frame in
`validation$issues`.

------------------------------------------------------------------------

## Step 6: Upload the data package to Zenodo

The last step is to upload the data package to
[Zenodo](https://zenodo.org).
[`uploadDataPackageToZenodo()`](https://birdscancommunity.github.io/birdscanR/reference/uploadDataPackageToZenodo.md)
uploads it as a **draft** record: it never publishes the record. You
check the draft on Zenodo, and publish it there yourself.

### Zenodo account and token

You need a Zenodo account and a personal access token: on Zenodo, go to
*Account* \> *Applications* \> *Personal access tokens*, and create a
token with the scopes `deposit:write` and `deposit:actions`. The token
gives access to your Zenodo account, so never write it in a script.
Store it as environment variable `ZENODO_TOKEN` in your `.Renviron`
file, which you can open with `usethis::edit_r_environ()`:

    ZENODO_TOKEN=<your token>

Restart R afterwards.
[`uploadDataPackageToZenodo()`](https://birdscancommunity.github.io/birdscanR/reference/uploadDataPackageToZenodo.md)
reads the token from there.

### Test in the Zenodo sandbox first

The [Zenodo sandbox](https://sandbox.zenodo.org) is a copy of Zenodo for
testing: records there are not real and are removed regularly. It needs
its own account and token; store that token as `ZENODO_SANDBOX_TOKEN`.
The BirdScan MR1 community does not exist in the sandbox, so set
`community = NULL`:

``` r

# Upload the data package as draft to the Zenodo sandbox
# =============================================================================
draft = uploadDataPackageToZenodo(
  packageDir = packageDir,
  sandbox    = TRUE,
  community  = NULL
)
draft$url
```

### Upload to Zenodo

When the sandbox draft looks right, upload the package to Zenodo itself:

``` r

# Upload the data package as draft to Zenodo
# =============================================================================
draft = uploadDataPackageToZenodo(packageDir = packageDir)
draft$url
draft$doi
```

[`uploadDataPackageToZenodo()`](https://birdscancommunity.github.io/birdscanR/reference/uploadDataPackageToZenodo.md):

1.  validates the package with
    [`validateDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/validateDataPackage.md),
    and stops if it is not valid;
2.  creates a draft record on Zenodo and reserves a DOI for it;
3.  writes the DOI as `id` into the local `datapackage.json` and adds it
    to the suggested citation, so the published package carries its own
    DOI;
4.  uploads `datapackage.json` and the CSV files;
5.  fills in the metadata of the record from `datapackage.json`:

| Zenodo field | Taken from |
|:---|:---|
| Title, version | `title`, `version` |
| Description | `description`, site, period and suggested citation |
| Creators | all contributors, with ORCID and affiliation |
| Contributors | each contributor with each of their roles; the owner as rights holder |
| Licence | `licenses` |
| Access | embargoed until `embargo` (files restricted, metadata public), or open if the embargo has passed |
| Additional notes | the contact person(s) for data requests during the embargo |
| Keywords | `keywords` |
| Related works | `relatedIdentifiers` |
| Locations, dates | site coordinates, and the period of the data (`Collected`) |
| Communities | the BirdScan MR1 community (`birdscanmr1community`) |

### Publish the record

Open the draft (`draft$url`), check the metadata and files, and click
*Publish*. Publishing cannot be undone: the files of a published record
can no longer be changed, only replaced by a new version. On
publication, Zenodo registers the DOI, and sends the record to the
curators of the BirdScan MR1 community for inclusion.

Finally, add the DOI as `id` to the metadata file of the campaign, so
that new versions of the package keep it:

``` yaml
id: https://doi.org/10.5281/zenodo.<record ID>
```

### Correct the data: upload a new version

Files of a published record cannot be changed. When the data of a
campaign are corrected, publish a **new version** instead. It gets its
own DOI, and all versions share the *concept DOI* of the record, which
always points to the latest version. Create the package again with:

- the DOI of the published record as `id` in the metadata file, and
- a higher `version`, e.g. `"1.0.1"` for a correction or `"2.0.0"` for a
  large change.

[`uploadDataPackageToZenodo()`](https://birdscancommunity.github.io/birdscanR/reference/uploadDataPackageToZenodo.md)
then sees the DOI and creates a draft of a new version of that record,
instead of a new record:

``` r

# Upload a corrected data package as new version
# =============================================================================
draft = uploadDataPackageToZenodo(packageDir = packageDir)
draft$newVersion
draft$doi
```

The files of the previous version are replaced by the new ones. Links
that were added to the previous version on Zenodo, e.g. to the
collection the campaign belongs to (see below), are kept. Again, check
the draft on Zenodo and publish it there.

### Link records: add related identifiers

Metadata of a published record can be changed without a new version. To
add links to other records, such as an `IsPartOf` link from a campaign
to the owner collection it belongs to, use
[`addZenodoRelatedIdentifiers()`](https://birdscancommunity.github.io/birdscanR/reference/addZenodoRelatedIdentifiers.md).
It opens an edit of the record and adds the links; existing links are
kept, and links the record already has are not added twice:

``` r

# Link a campaign to its owner collection
# =============================================================================
addZenodoRelatedIdentifiers(
  doi = "https://doi.org/10.5281/zenodo.<record ID>",
  relatedIdentifiers = list(list(
    relationType      = "IsPartOf",
    relatedIdentifier = "https://doi.org/10.5281/zenodo.<collection record ID>"
  ))
)
```

By default, the edit is not published: check it on Zenodo and publish it
there. Use `publish = TRUE` to publish the edit directly; the DOI and
version of the record stay the same.
