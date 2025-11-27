# Create Time Range for Plot

Create a list by segmenting the input time range into regular periods
for plots.

## Usage

``` r
createTimeRangeForPlot(
  startDate = NULL,
  endDate = NULL,
  periodLength = 7,
  returnAsList = TRUE
)
```

## Arguments

- startDate:

  Per default, the first element of the input setting `timeRangeData`

- endDate:

  Per default, the second element of the input setting \`timeRangeData“

- periodLength:

  Duration in days of each period

- returnAsList:

  TRUE per default, otherwise as data.frame.

## Value

A list of time periods

## See also

Other plot functions:
[`plotExploration()`](https://birdscancommunity.github.io/birdscanR/reference/plotExploration.md),
[`plotLongitudinalMTR()`](https://birdscancommunity.github.io/birdscanR/reference/plotLongitudinalMTR.md)

## Author

Baptiste Schmid, Birgen Haest

## Examples

``` r
if (FALSE) { # \dontrun{
# Set server, database, and other input settings
# ===========================================================================
# Example with seven days time window
timeRangeData <- c("2024-01-01", "2024-02-15")
timeRangePlot <- createTimeRangePlot(timeRangeData[1], timeRangeData[2], 7)
print(timeRangePlot)
} # }
```
