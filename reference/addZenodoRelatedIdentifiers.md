# Add Related Identifiers to a Zenodo Record

`addZenodoRelatedIdentifiers()` adds related identifiers to the metadata
of a Zenodo record, e.g. an `IsPartOf` link from a campaign data package
to the owner collection it belongs to, or a `HasPart` link from a
collection to a campaign. Related identifiers that the record already
has are not added twice; existing ones are kept.

Only the metadata are changed: the files, the DOI and the version of the
record stay the same, so no new version is needed. The latest version of
the record is updated, whichever version's DOI (or the concept DOI) is
given.

- For a **published** record, the function opens an edit of the record
  and updates its metadata. With `publish = FALSE` (default), the edit
  stays open: check it on Zenodo and publish it there (or discard it).
  With `publish = TRUE`, the edit is published directly.

- For an **unpublished draft**, the metadata of the draft are updated.
  The draft is never published, also not with `publish = TRUE`.

## Usage

``` r
addZenodoRelatedIdentifiers(
  doi,
  relatedIdentifiers,
  token = NULL,
  sandbox = FALSE,
  publish = FALSE
)
```

## Arguments

- doi:

  DOI (e.g. `"https://doi.org/10.5281/zenodo.1234567"` or
  `"10.5281/zenodo.1234567"`) or record ID of the Zenodo record.

- relatedIdentifiers:

  related identifiers to add: a list of lists, each with `relationType`
  (DataCite relation type, e.g. `"IsPartOf"`, `"HasPart"`,
  `"IsCitedBy"`) and `relatedIdentifier` (e.g. a DOI URL), as in the
  metadata file of a data package; or a data frame with these two
  columns.

- token:

  Zenodo personal access token with the scopes `deposit:write` and
  `deposit:actions`. Default is the environment variable `ZENODO_TOKEN`
  (or `ZENODO_SANDBOX_TOKEN` if `sandbox = TRUE`). Never write the token
  in a script.

- sandbox:

  logical. If `TRUE`, use the Zenodo sandbox
  (<https://sandbox.zenodo.org>). Default is `FALSE`.

- publish:

  logical. If `TRUE`, the edit of a published record is published
  directly. Default is `FALSE`: the edit stays open on Zenodo.

## Value

A list, invisibly, with `id` (Zenodo record ID), `url` (link to the
record on Zenodo), `added` (number of related identifiers added) and
`published` (logical, whether the edit was published).

## See also

Other write file functions:
[`compileData()`](https://birdscancommunity.github.io/birdscanR/reference/compileData.md),
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md),
[`createDataPackageMetadataTemplate()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackageMetadataTemplate.md),
[`saveMTR()`](https://birdscancommunity.github.io/birdscanR/reference/saveMTR.md),
[`savePlotToFile()`](https://birdscancommunity.github.io/birdscanR/reference/savePlotToFile.md),
[`uploadDataPackageToZenodo()`](https://birdscancommunity.github.io/birdscanR/reference/uploadDataPackageToZenodo.md),
[`validateDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/validateDataPackage.md)

## Author

Birgen Haest

## Examples

``` r
if (FALSE) { # \dontrun{
# Link a campaign data package to its owner collection (Zenodo sandbox)
# ===========================================================================
addZenodoRelatedIdentifiers(
  doi = "https://doi.org/10.5281/zenodo.1234567",
  relatedIdentifiers = list(list(
    relationType      = "IsPartOf",
    relatedIdentifier = "https://doi.org/10.5281/zenodo.7654321"
  )),
  sandbox = TRUE
)
} # }
```
