# create and read VPTS

------------------------------------------------------------------------

## Summary

This vignette provides an example of the basic workflow for createVPTS:

1.  Create a VPTS using the BirdScan MR1 example data and create a CSV
    file,

2.  Read and convert the CSV file to VPTS,

3.  Visualise the time series.

------------------------------------------------------------------------

## Before we get started

``` r

library(birdscanR)
library(bioRad)
#> Welcome to bioRad version 0.12.0
outDir <- tempdir()
```

Read Test BirdScanMR1 data

The package ships with a sample data extract from a Birdscan MR1 radar
(Sempach, Switzerland), already in the format returned by
[`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md).
We load it directly from the package’s `extdata` folder.

``` r

dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))
```

------------------------------------------------------------------------

## Step 1: Create VPTS from the BirdScan MR1 example dataset

VPTS CSV files, in line with the [ALOFT data
standard](https://aloftdata.eu/vpts-csv/), can be generated directly
from the echo data with
[`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md).
Note that this function only works on Birdscan MR1 database versions \>=
1.7.0.4, as the variable `feature37.speed` is required for the density
calculation.

``` r

dbName = "CH_Sempach_2024_SEP24_25"
mainOutputDir = file.path(outDir)
targetTimeZone = "Etc/GMT0"
timeRangeData = c("2024-09-24 00:00", "2024-09-25 23:59")

# Set manual blind times to NULL (no manual blind times)
# =============================================================================
cManualBlindTimes = NULL

# Create vpts files
# =============================================================================
vptsDir = createVPTS(
  dbName = dbName,
  outputDir = mainOutputDir,
  echoes = dbData$echoData,
  altitudeRange = c(25, 1025),
  altitudeBinSize = 50,
  timeRange = timeRangeData,
  timeBinDuration_sec = 1800,
  timeZone = targetTimeZone,
  protocolData = dbData$protocolData,
  visibilityData = dbData$visibilityData,
  siteData = dbData$siteData,
  sunriseSunset = dbData$sunriseSunset,
  manualBlindTimes = cManualBlindTimes,
  saveBlindTimes = FALSE,
  blindTimesOutputDir = mainOutputDir,
  blindTimeAsMtrZero = NULL,
  propObsTimeCutoff = 0.2
)
#> Creating altitude bins..
#> Creating time bins..
#> Warning in createTimeBins(timeRange = timeRange, timeBinDuration_sec =
#> timeBinDuration_sec, : 'createTimeBins()' was called with an input time range
#> that ends later than 2 days before the last sunrise/sunset in the sunriseSunset
#> dataset. The end of the timerange was adjusted to the last date in the
#> sunriseSunset dataset - 2 days.
#> Calculating blind times..
#> Subsetting echo data..
#> Computing observation times for each timebin..
#> 35 echoes above the defined altitude range, thus excldued from the VPTS calculation.
#> Computing densities in each time-altitude bin..
#> Computing number of animals (n_dbz,), number of birds with speed and direction values (n)mean flux directon (dd, NOTE: not mean circular direction, but  mean flux direction taking into account individuals' flight speeds), mean flux speeds (ff, NOTE: not mean ground speed of the individual birds but speed flux size taking into account individual's directions), mean u and v components and mtr-weighted average rcs (rcs) of animal movements in each time-altitude bin..
#> Writing daily VPTS csv files to /tmp/RtmpoVGpFL/vpts..
```

------------------------------------------------------------------------

## Step 2: Read the CSV-file and convert to VPTS with BioRad::as.vpts()

``` r

vpts = read.csv(file.path(vptsDir, "200_vpts_20240924.csv"))
vpts = as.vpts(vpts) 
#> Warning in validate_vpts(data): Validation issues found: Type validation failed
#> for radar; Type validation failed for datetime; Type validation failed for w;
#> Type validation failed for sd_vvp; Type validation failed for eta; Type
#> validation failed for dbz; Type validation failed for DBZH; Type validation
#> failed for n_all; Type validation failed for n_dbz_all; Type validation failed
#> for sd_vvp_threshold; Type validation failed for vcp; Type validation failed
#> for source_file
#> Warning in check_multivalue_attributes(data): multiple rcs values found,
#> storing only first (NA) as the functional attribute.
```

------------------------------------------------------------------------

## Step 3: Visualise

Visualise

``` r

png(file.path(outDir, "plot.png"))
plot(vpts)
dev.off()
#> agg_png 
#>       2
```
