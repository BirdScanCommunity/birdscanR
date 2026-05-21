# Get the nautical, civil, and solar dawn and dusk for a given timerange and locations.

Get the time of nautical (sun at 12 degrees below horizon), civil (sun
at 6 degrees below horizon) and solar (sun at 0 degrees below horizon)
dawn and dusk for each day over a given time range.

## Usage

``` r
twilight(timeRange, latLon, crs_datum = "WGS84", timeZone)
```

## Arguments

- timeRange:

  A two-element character vector with elements of the form %Y-%m-%d
  defining the start and end of the timerange for which you want to get
  the twilight information.

- latLon:

  A list of X, Y coordinates

- crs_datum:

  The coordinate reference system and datum of the X, Y coordinates.
  Default = "WGS84.

- timeZone:

  The time zone of the area of interest

## Value

A data frame with the results

## See also

Other manipulation functions:
[`addDayNightInfoPerEcho()`](https://birdscancommunity.github.io/birdscanR/reference/addDayNightInfoPerEcho.md),
[`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md),
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md),
[`convertTimeZone()`](https://birdscancommunity.github.io/birdscanR/reference/convertTimeZone.md),
[`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md),
[`filterSpeedFeature37()`](https://birdscancommunity.github.io/birdscanR/reference/filterSpeedFeature37.md),
[`mergeVisibilityAndManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/mergeVisibilityAndManualBlindTimes.md),
[`reclassToBats()`](https://birdscancommunity.github.io/birdscanR/reference/reclassToBats.md)

## Author

Fabian Hertner, Birgen Haest

## Examples

``` r
sunriseSunset = twilight(
  timeRange = c("2024-09-24 00:00", "2024-09-25 23:59"),
  latLon    = c(47.12764, 8.192569),
  timeZone  = "Etc/GMT0"
)
```
