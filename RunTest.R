#devtools::document()   # regenerates NAMESPACE and Rd files from roxygen comments
#devtools::check()      # or: R CMD check, runs full package check incl. examples/tests

devtools::install()

library(birdscanR)

# run test
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))

mtrData = computeMTR(
  dbName = "CH_Sempach_2024",
  echoes = dbData$echoData,
  classSelection = unique(dbData$echoData$class),
  altitudeRange = c(25, 1025),
  altitudeBinSize = 50,
  timeRange = as.POSIXct(c("2024-09-24","2024-09-25")),
  timeBinDuration_sec = 1800,
  timeZone = "UTC",
  sunriseSunset = dbData$sunriseSunset,
  sunOrCivil = "civil",
  crepuscule = "nauticalSolar",
  protocolData = dbData$protocolData,
  visibilityData = dbData$visibilityData,
  manualBlindTimes = NULL,
  saveBlindTimes = FALSE,
  blindTimesOutputDir = getwd(),
  blindTimeAsMtrZero = NULL,
  propObsTimeCutoff = 0,
  computePerDayNight = FALSE,
  computePerDayCrepusculeNight = FALSE,
  computeAltitudeDistribution = FALSE
)

# Add the weighted mean flight speed for the passerine_type class
# ===========================================================================
mtrData = addFeatSummToMTR(
  mtrDensVPTS = mtrData,
  echoData = dbData$echoData,
  class = "passerine_type",
  inputVariable = "feature37.speed",
  nCores = 1
)


densityData = computeDensity(
  dbName = dbName,
  echoes = dbData$echoData,
  classSelection = unique(dbData$echoData$class),
  altitudeRange = c(25, 1025),
  altitudeBinSize = 50,
  timeRange = as.POSIXct(c("2024-09-24","2024-09-25")),
  timeBinDuration_sec = 1800,
  timeZone = "UTC",
  sunriseSunset = dbData$sunriseSunset,
  sunOrCivil = "civil",
  crepuscule = "nauticalSolar",
  protocolData = dbData$protocolData,
  visibilityData = dbData$visibilityData,
  manualBlindTimes = cManualBlindTimes,
  saveBlindTimes = FALSE,
  blindTimesOutputDir = getwd(),
  blindTimeAsMtrZero = NULL,
  propObsTimeCutoff = 0,
  computePerDayNight = FALSE,
  computePerDayCrepusculeNight = FALSE,
  computeAltitudeDistribution = FALSE
)