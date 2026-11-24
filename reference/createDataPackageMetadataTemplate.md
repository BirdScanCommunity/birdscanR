# Create MR1 Data Package Metadata Template

`createDataPackageMetadataTemplate()` writes a commented YAML template
with the package-level metadata of an MR1 data package: campaign ID,
title, description, version, owner, licence, embargo, keywords,
contributors (with ORCID, affiliation, DataCite roles and embargo
request type), related identifiers and suggested citation. Fill in the
template and pass its path to the `metadata` argument of
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md).

## Usage

``` r
createDataPackageMetadataTemplate(filePath, overwrite = FALSE)
```

## Arguments

- filePath:

  Path of the YAML file to write.

- overwrite:

  logical. If `TRUE`, an existing file is overwritten. Default is
  `FALSE`.

## Value

The path of the written file, invisibly.

## See also

Other write file functions:
[`compileData()`](https://birdscancommunity.github.io/birdscanR/reference/compileData.md),
[`createDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/createDataPackage.md),
[`saveMTR()`](https://birdscancommunity.github.io/birdscanR/reference/saveMTR.md),
[`savePlotToFile()`](https://birdscancommunity.github.io/birdscanR/reference/savePlotToFile.md),
[`validateDataPackage()`](https://birdscancommunity.github.io/birdscanR/reference/validateDataPackage.md)

## Author

Birgen Haest

## Examples

``` r
# Write the metadata template
# ===========================================================================
metadataFile = file.path(tempdir(), "CH_Sempach_2024_metadata.yaml")
createDataPackageMetadataTemplate(filePath = metadataFile, overwrite = TRUE)
```
