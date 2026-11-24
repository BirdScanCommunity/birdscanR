# Upload MR1 Data Package to Zenodo

`uploadDataPackageToZenodo()` uploads an MR1 data package, written to
disk by
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md),
to [Zenodo](https://zenodo.org) as a **draft** record. The function
never publishes the record: check the draft on Zenodo and publish it
there.

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
`datapackage.json` (and added to the suggested citation) before the
files are uploaded, so the published package carries its own DOI. Also
add it as `id` to the metadata file of the campaign, so new versions of
the package keep it.

## Usage

``` r
uploadDataPackageToZenodo(
  packageDir,
  token = NULL,
  sandbox = FALSE,
  community = "birdscanmr1community",
  addDoi = TRUE
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

## Value

A list, invisibly, with `id` (Zenodo record ID), `doi` (reserved DOI),
`url` (link to the draft on Zenodo) and `files` (names of the uploaded
files).

## See also

Other write file functions:
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
