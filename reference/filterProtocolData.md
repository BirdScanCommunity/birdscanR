# filterProtocolData

With `filterProtocolData()` the protocol data can be filtered by the
operation mode (pulse-type and antenna rotation). The function returns
the filtered subset of the protocol data which can later be used to
filter the echoes based on the operation mode/protocol

## Usage

``` r
filterProtocolData(
  protocolData = NULL,
  pulseTypeSelection = NULL,
  rotationSelection = NULL
)
```

## Arguments

- protocolData:

  `data.frame` with the protocol data from the data list created by
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)

- pulseTypeSelection:

  character vector with the pulse types which should be included in the
  subset. Options: “S”, “M”, “L” (short-, medium-, long-pulse). Default
  is NULL: no filtering applied based on pulseType.

- rotationSelection:

  numeric vector to select the operation modes with and/or without
  antenna rotation. Options: 0, 1. (0 = no rotation, 1 = rotation).
  Default is NULL: no filtering applied based on rotation mode.

## Value

returns the filtered protocol data in the same format as provided in the
parameter `protocolData`.

## See also

Other filter functions:
[`filterData()`](https://birdscancommunity.github.io/birdscanR/reference/filterData.md),
[`filterEchoData()`](https://birdscancommunity.github.io/birdscanR/reference/filterEchoData.md)

## Author

Fabian Hertner, Birgen Haest

## Examples

``` r
# \donttest{
# Load example data
# ===========================================================================
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"))

# Set input settings for filtering of the data
# ===========================================================================
pulseLengthSelection = "S"
rotationSelection = 1

# Filter the protocol data
# ===========================================================================
filteredProtocolData = filterProtocolData(
  protocolData      = dbData$protocolData,
  pulseTypeSelection = pulseLengthSelection,
  rotationSelection  = rotationSelection
)
# }
```
