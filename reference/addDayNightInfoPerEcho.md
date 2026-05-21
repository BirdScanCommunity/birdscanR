# addDayNightInfoPerEcho

`addDayNightInfoPerEcho()` adds three columns ‘dayOrNight’,
”dayOrCrepOrNight' and ‘dateSunset’ to the echo data. This allows the
user to filter echo data easily by “day” and “night”, or "day",
"crepuscular", and "night".

## Usage

``` r
addDayNightInfoPerEcho(
  echoData,
  sunriseSunset,
  sunOrCivil = "civil",
  crepuscule = "nauticalSolar"
)
```

## Arguments

- echoData:

  dataframe with the echo data from the data list created with
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).

- sunriseSunset:

  dataframe with sunrise/sunset and civil twilight times created with
  [`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md)

- sunOrCivil:

  optional character variable, Set to “sun” to use sunrise/sunset times
  or to “civil” to use civil twilight times to group echoes into
  day/night. Default is "civil".

- crepuscule:

  optional character variable, Set to “nauticalSolar” to use the time
  between nautical dusk/dawn and sunrise/sunset times to define the
  crepuscular period, or to "nauticalCivil" to use the time between
  nautical and civil dusk/dawn to define the crepuscular period, or to
  "civilSolar" to use the time between civil dusk/dawn and
  sunrise/sunset times to define the crepuscular period. Default is
  "nauticalSolar".

## Value

data frame with thre columns added, i.e. 'dayOrNight',
'dayOrCrepOrNight', and 'dateSunset'.

## See also

Other manipulation functions:
[`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md),
[`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md),
[`convertTimeZone()`](https://birdscancommunity.github.io/birdscanR/reference/convertTimeZone.md),
[`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md),
[`filterSpeedFeature37()`](https://birdscancommunity.github.io/birdscanR/reference/filterSpeedFeature37.md),
[`mergeVisibilityAndManualBlindTimes()`](https://birdscancommunity.github.io/birdscanR/reference/mergeVisibilityAndManualBlindTimes.md),
[`reclassToBats()`](https://birdscancommunity.github.io/birdscanR/reference/reclassToBats.md),
[`twilight()`](https://birdscancommunity.github.io/birdscanR/reference/twilight.md)

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

# Add day/night info to echo data
# ===========================================================================
echoData = addDayNightInfoPerEcho(
  echoData      = dbData$echoData,
  sunriseSunset = dbData$sunriseSunset,
  sunOrCivil    = "civil"
)
# }
```
