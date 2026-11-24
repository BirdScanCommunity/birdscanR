# Validate MR1 Data Package

`validateDataPackage()` checks whether an MR1 data package, created by
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md),
is complete and consistent before it is published, e.g. on Zenodo. It
checks:

- the package structure (Frictionless Data Package) and the
  package-level metadata (the same checks as in
  [`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md),
  plus the properties that
  [`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md)
  adds automatically);

- that all five resources (`echo_data`, `protocol_data`, `blind_times`,
  `sunrise_sunset`, `site_data`) are present, can be read, and contain
  the required columns;

- that the column names and types match the Table Schema of each
  resource, and that every column has a description;

- that primary keys are unique and not missing, and that foreign keys
  (e.g. `protocolID` of the echoes) refer to existing rows;

- that the content is plausible: start times before stop times, echoes
  within their measurement period and within the temporal coverage, and
  site coordinates equal to the spatial coverage.

Problems that make the package unusable or inconsistent are reported as
errors; problems that do not, e.g. a column without description, as
warnings. A package is valid when there are no errors.

## Usage

``` r
validateDataPackage(package)
```

## Arguments

- package:

  MR1 data package object returned by
  [`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md)
  or
  [`frictionless::read_package()`](https://docs.ropensci.org/frictionless/reference/read_package.html),
  or the path to a `datapackage.json` file or to the directory that
  contains it.

## Value

An object of class `mr1dpValidation`: a list with `valid` (logical,
`TRUE` if no errors were found) and `issues` (data frame with the
columns `level` (`"error"` or `"warning"`), `resource`, `field` and
`message`). Printing it gives a summary of the issues.

## See also

Other write file functions:
[`compileData()`](https://birdscancommunity.github.io/birdscanR/reference/compileData.md),
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md),
[`createDataPackageMetadataTemplate()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackageMetadataTemplate.md),
[`saveMTR()`](https://birdscancommunity.github.io/birdscanR/reference/saveMTR.md),
[`savePlotToFile()`](https://birdscancommunity.github.io/birdscanR/reference/savePlotToFile.md)

## Author

Birgen Haest

## Examples

``` r
# \donttest{
# Load example data
# ===========================================================================
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))
mainOutputDir = tempdir()

# Write the package metadata template (fill it in for a real campaign)
# ===========================================================================
metadataFile = file.path(mainOutputDir, "CH_Sempach_2024_metadata.yaml")
createDataPackageMetadataTemplate(filePath = metadataFile, overwrite = TRUE)

# Create MR1 data package
# ===========================================================================
dataPackage = createDataPackage(
  dbExtract          = dbData,
  metadata           = metadataFile,
  pulseTypeSelection = "S",
  outputDirPath      = mainOutputDir
)

# Validate the data package object
# ===========================================================================
validateDataPackage(dataPackage)
#> Warning: `resources()` was deprecated in frictionless 1.3.0.
#> ℹ Please use `resource_names()` instead.
#> ℹ The deprecated feature was likely used in the birdscanR package.
#>   Please report the issue at
#>   <https://github.com/BirdScanCommunity/birdscanR/issues>.
#> Warning: `get_schema()` was deprecated in frictionless 1.3.0.
#> ℹ Please use `schema()` instead.
#> ℹ The deprecated feature was likely used in the birdscanR package.
#>   Please report the issue at
#>   <https://github.com/BirdScanCommunity/birdscanR/issues>.
#> The MR1 data package is valid.

# Validate the data package files on disk
# ===========================================================================
validation = validateDataPackage(file.path(mainOutputDir, "CH_Sempach_2024"))
validation$valid
#> [1] TRUE
# }
```
