# Upload MR1 Data Package to Zenodo

`uploadDataPackageToZenodo()` uploads an MR1 data package, written to
disk by
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md),
to [Zenodo](https://zenodo.org) as a **draft** record: either a new
record, or a new version of an existing record. The function never
publishes the record: check the draft on Zenodo and publish it there.

The package is first checked with
[`validateDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/validateDataPackage.md);
an invalid package is not uploaded. The Zenodo metadata are taken from
`datapackage.json`:

- title, description, version, keywords and related identifiers;

- creators: all contributors, with ORCID and affiliation;

- contributors: each contributor with each of their roles, and the owner
  as rights holder;

- licence, and the embargo: if the embargo date is in the future, the
  record is `embargoed` (metadata public, files restricted until that
  date), otherwise `open`;

- location of the site, and the period of the data (`Collected`);

- the suggested citation, and the contact person(s) for data requests
  during the embargo, in the description and notes.

A DOI is reserved for the record. It is written as `id` in the local
`datapackage.json` (and in the suggested citation) before the files are
uploaded, so the published package carries its own DOI.

**New versions.** When the data of a published campaign are corrected,
create the package again with a higher `version`, and with the DOI of
the published record as `id` in the metadata file. The upload then
creates a new version of that record (with a new DOI) instead of a new
record. All versions share the concept DOI of the record. The files of
the previous version are replaced, and the related identifiers of the
previous version that are not in the package (e.g. `IsPartOf` links to
collections, see
[`addZenodoRelatedIdentifiers()`](https://birdscancommunity.github.io/birdscanR/reference/addZenodoRelatedIdentifiers.md))
are kept.

## Usage

``` r
uploadDataPackageToZenodo(
  packageDir,
  token = NULL,
  sandbox = FALSE,
  community = "birdscanmr1community",
  addDoi = TRUE,
  newVersion = NULL
)
```

## Arguments

- packageDir:

  Directory of the data package, i.e. the folder with `datapackage.json`
  written by
  [`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md).

- token:

  Zenodo personal access token with the scopes `deposit:write` and
  `deposit:actions`. Default is the environment variable `ZENODO_TOKEN`
  (or `ZENODO_SANDBOX_TOKEN` if `sandbox = TRUE`). Never write the token
  in a script.

- sandbox:

  logical. If `TRUE`, upload to the Zenodo sandbox
  (<https://sandbox.zenodo.org>) for testing, which needs its own
  account and token. Default is `FALSE`.

- community:

  Identifier of the Zenodo community the record is submitted to when it
  is published. Default is `"birdscanmr1community"`, the BirdScan MR1
  community. Use `NULL` for none, e.g. when testing in the sandbox,
  where this community does not exist.

- addDoi:

  logical. If `TRUE` (default), the reserved DOI is written into the
  local `datapackage.json` before the upload.

- newVersion:

  logical or `NULL`. If `TRUE`, create a new version of the Zenodo
  record given by the `id` (DOI) of the package; if `FALSE`, create a
  new record. Default is `NULL`: a new version if the package has a
  Zenodo DOI as `id`, otherwise a new record.

## Value

A list, invisibly, with `id` (Zenodo record ID of the draft), `doi`
(reserved DOI), `url` (link to the draft on Zenodo), `files` (names of
the uploaded files) and `newVersion` (logical).

## See also

Other write file functions:
[`addZenodoRelatedIdentifiers()`](https://birdscancommunity.github.io/birdscanR/reference/addZenodoRelatedIdentifiers.md),
[`compileData()`](https://birdscancommunity.github.io/birdscanR/reference/compileData.md),
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md),
[`createDataPackageMetadataTemplate()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackageMetadataTemplate.md),
[`saveMTR()`](https://birdscancommunity.github.io/birdscanR/reference/saveMTR.md),
[`savePlotToFile()`](https://birdscancommunity.github.io/birdscanR/reference/savePlotToFile.md),
[`validateDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/validateDataPackage.md)

## Author

Birgen Haest

## Examples

``` r
if (FALSE) { # \dontrun{
# Create the data package (see createDataPackage())
# ===========================================================================
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))
mainOutputDir = file.path(".", "results")
metadataFile = file.path(mainOutputDir, "CH_Sempach_2024_metadata.yaml")
createDataPackageMetadataTemplate(filePath = metadataFile, overwrite = TRUE)
createDataPackage(
  dbExtract     = dbData,
  metadata      = metadataFile,
  outputDirPath = mainOutputDir
)

# Upload the data package as draft to the Zenodo sandbox
# ===========================================================================
# The token is read from the environment variable ZENODO_SANDBOX_TOKEN, e.g.
# set in your .Renviron file
draft = uploadDataPackageToZenodo(
  packageDir = file.path(mainOutputDir, "CH_Sempach_2024"),
  sandbox    = TRUE,
  community  = NULL
)
draft$url
} # }
```
