# Create MR1 Data Package

`createDataPackage()` compiles data from
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
into a standardised MR1 data package for one radar campaign, following
the [Frictionless Data Package](https://datapackage.org) standard. Time
range, pulse type, rotation, echo class, class probability, altitude and
echo validator filters are applied with
[`filterProtocolData()`](https://birdscancommunity.github.io/birdscanR/reference/filterProtocolData.md)
and
[`filterEchoData()`](https://birdscancommunity.github.io/birdscanR/reference/filterEchoData.md).

The package contains five tabular resources: `echo_data`,
`protocol_data`, `blind_times`, `sunrise_sunset` and `site_data`. Each
resource has a Table Schema with field types, descriptions, units, and
primary and foreign keys. `echo_data` contains all echo columns, except
those that are obsolete or redundant: `mtr_factor_old`,
`statistical_classification_old`, `classification_id` and `sphere_dia`
(old classification, outdated since BirdScan v1.6), `feature15.polRatio`
and `feature16.absPolarisation` (outdated since BirdScan v1.6),
`feature3.speed` (superseded by `feature37.speed`), `time_string` (same
as `time_stamp_originTZ`), and `time_bin` and `altitude_bin` (refer to
radar database tables that are not part of the package). The
package-level metadata (title, description, owner, licence, embargo,
contributors with ORCID and DataCite roles, related identifiers,
suggested citation) are read from `metadata`. Spatial and temporal
coverage, the applied filters and the provenance (database, birdscanR
and classifier versions) are added automatically.

When `outputDirPath` is provided, the package is written to
`outputDirPath/<campaignID>/` as `datapackage.json` plus one compressed
CSV file per resource, ready for a Zenodo deposit.

## Usage

``` r
createDataPackage(
  dbExtract,
  metadata,
  manualBlindTimes = NULL,
  dbName = NULL,
  pulseTypeSelection = NULL,
  rotationSelection = NULL,
  timeRangeTargetTZ = NULL,
  classSelection = NULL,
  classProbCutOff = NULL,
  altitudeRange = NULL,
  echoValidator = FALSE,
  outputDirPath = NULL,
  saveAsRDS = FALSE
)
```

## Arguments

- dbExtract:

  named list with the database tables extracted by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).

- metadata:

  path to a YAML file, or a named list with the same structure, with the
  package-level metadata. Use
  [`createDataPackageMetadataTemplate()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackageMetadataTemplate.md)
  to get a template.

- manualBlindTimes:

  data frame with the manual blind times created by
  [`loadManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/loadManualBlindTimes.md),
  or `NULL` (default) if there are none. They are merged with the radar
  blind times by
  [`mergeVisibilityAndManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/mergeVisibilityAndManualBlindTimes.md).
  Only blind times that overlap a selected measurement period (protocol)
  are included in the package.

- dbName:

  Name of the database. Stored in the package provenance. Default is
  `NULL`.

- pulseTypeSelection:

  character vector with the pulse types which should be included in the
  subset. Options: `S`, `M`, `L`, i.e. short-, medium-, long-pulse,
  respectively. Default is `NULL`: no filtering applied based on
  pulseType.

- rotationSelection:

  numeric vector to select the operation modes with and/or without
  antenna rotation. Options: 0, 1. (0 = no rotation, 1 = rotation).
  Default is `NULL`: no filtering applied based on rotation mode.

- timeRangeTargetTZ:

  Character vector of length 2, with start and end of time range,
  formatted as `"%Y-%m-%d %H:%M"`, in the target time zone, or a
  `POSIXct` vector of length 2. Echoes outside the time range will be
  excluded. Default is `NULL`: the full time range of the echo data is
  used.

- classSelection:

  character string vector with the classes that should be included.
  Default is `NULL`: all classes are included.

- classProbCutOff:

  numeric cutoff value for class probabilities. Echoes with a lower
  class probability will be excluded. Default is `NULL`: no cutoff
  applied.

- altitudeRange:

  numeric vector of length 2 with start and end of the altitude range in
  metres a.g.l. Echoes outside the altitude range will be excluded.
  Default is `NULL`: no altitude filtering applied.

- echoValidator:

  logical. If set to `FALSE` (default), no additional filter is applied;
  if set to `TRUE`, echoes labelled by the echo validator as
  `"non-bio scatterer"` will be excluded.

- outputDirPath:

  Directory where the data package is written, in a subdirectory named
  after the `campaignID` in `metadata`. If `NULL` (default), no files
  are written.

- saveAsRDS:

  logical. If `TRUE` and `outputDirPath` is provided, the returned data
  package object is also saved as `<campaignID>.rds` in `outputDirPath`.
  Default is `FALSE`.

## Value

A Frictionless Data Package object (class `mr1dp` and `datapackage`),
i.e. a list with the package-level metadata and the resources
`echo_data`, `protocol_data`, `blind_times`, `sunrise_sunset` and
`site_data`. Read a resource as a data frame with
[`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html).

## See also

Other write file functions:
[`compileData()`](https://birdscancommunity.github.io/birdscanR/reference/compileData.md),
[`createDataPackageMetadataTemplate()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackageMetadataTemplate.md),
[`saveMTR()`](https://birdscancommunity.github.io/birdscanR/reference/saveMTR.md),
[`savePlotToFile()`](https://birdscancommunity.github.io/birdscanR/reference/savePlotToFile.md),
[`validateDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/validateDataPackage.md)

## Author

Baptiste Schmid, Fabian Hertner, Birgen Haest

## Examples

``` r
# \donttest{
# Load example data
# ===========================================================================
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))
dbName = "CH_Sempach_2024_SEP24_25"
timeRangeTargetTZ = c("2024-09-24 00:00", "2024-09-25 23:59")
mainOutputDir = tempdir()

# Set manual blind times to NULL (no manual blind times)
# ===========================================================================
cManualBlindTimes = NULL

# Write the package metadata template (fill it in for a real campaign)
# ===========================================================================
metadataFile = file.path(mainOutputDir, "CH_Sempach_2024_metadata.yaml")
createDataPackageMetadataTemplate(filePath = metadataFile, overwrite = TRUE)

# Create MR1 data package
# ===========================================================================
dataPackage = createDataPackage(
  dbExtract          = dbData,
  metadata           = metadataFile,
  manualBlindTimes   = cManualBlindTimes,
  dbName             = dbName,
  timeRangeTargetTZ  = timeRangeTargetTZ,
  pulseTypeSelection = "S",
  outputDirPath      = mainOutputDir
)

# Read a resource of the data package
# ===========================================================================
echoData = frictionless::read_resource(dataPackage, "echo_data")
# }
```
