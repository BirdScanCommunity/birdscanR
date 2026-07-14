#devtools::document()   # regenerates NAMESPACE and Rd files from roxygen comments
#devtools::check()      # or: R CMD check, runs full package check incl. examples/tests

# make sure to install the latest local version
remove.packages("birdscanR")
devtools::install()

library(birdscanR)

# Load example data
# ===========================================================================
dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))
dbName = "CH_Sempach_2024"
classSelection.mtr = unique(dbData$echoData$class)

# ===========================================================================
# Test 1: interplay between addFeatSummary() and computeMTR()
# ===========================================================================

# 1a. computeMTR() with addFeaturesSummary = TRUE -> direction/speed summary
#     columns should be added automatically, for "allClasses" and for every
#     class in classSelection.
# ===========================================================================
mtrData = computeMTR(
  dbName = dbName,
  echoes = dbData$echoData,
  classSelection = classSelection.mtr,
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
  addFeaturesSummary = TRUE,
  computeAltitudeDistribution = FALSE
)

expectedCols = c()
for (cClass in c("allClasses", classSelection.mtr)) {
  expectedCols = c(expectedCols, c(
    paste0("nEchoesDirection.", cClass),
    paste0("directionMean.", cClass),
    paste0("directionRho.", cClass),
    paste0("directionSD.", cClass),
    paste0("nEchoesSpeed.", cClass),
    paste0("speedMean.", cClass),
    paste0("speedSD.", cClass)
  ))
}
missingCols = setdiff(expectedCols, colnames(mtrData))
if (length(missingCols) > 0) {
  stop("Test 1a FAILED. Missing expected columns: ", paste(missingCols, collapse = ", "))
} else {
  message("Test 1a PASSED. All expected direction/speed summary columns are present.")
}
print(mtrData[1, expectedCols])

# For each class (and .allClasses), generate a historgram of the frequency of echo counts ($nEchoesDirection.) in seperate panels.
# Zero counts tend to dominate the bins, so report the proportion of zeros per
# class separately and restrict the histogram to non-zero values.
# ===========================================================================
nEchoesDirectionCols = grep("^nEchoesDirection\\.", colnames(mtrData), value = TRUE)
nEchoesDirectionLong = data.frame(
  class = sub("^nEchoesDirection\\.", "", rep(nEchoesDirectionCols, each = nrow(mtrData))),
  nEchoesDirection = unlist(mtrData[, nEchoesDirectionCols], use.names = FALSE)
)

propZero = stats::aggregate(
  nEchoesDirection ~ class, data = nEchoesDirectionLong,
  FUN = function(x) mean(x == 0)
)
names(propZero)[2] = "propZero"
print(propZero)

nEchoesDirectionNonZero = nEchoesDirectionLong[nEchoesDirectionLong$nEchoesDirection > 0, ]

echoCountHistPlot = ggplot2::ggplot(nEchoesDirectionNonZero, ggplot2::aes(x = nEchoesDirection)) +
  ggplot2::geom_histogram(binwidth = 1) +
  ggplot2::facet_wrap(~ class, scales = "free") +
  ggplot2::labs(x = "nEchoesDirection", y = "Frequency",
                title = "Frequency of non-zero echo counts per class") +
  ggplot2::theme_bw()
print(echoCountHistPlot)

# 1b. computeMTR() with addFeaturesSummary = FALSE -> none of the
#     direction/speed summary columns should be added.
# ===========================================================================
mtrDataNoSummary = computeMTR(
  dbName = dbName,
  echoes = dbData$echoData,
  classSelection = classSelection.mtr,
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
  addFeaturesSummary = FALSE,
  computeAltitudeDistribution = FALSE
)

presentCols = intersect(expectedCols, colnames(mtrDataNoSummary))
if (length(presentCols) > 0) {
  stop("Test 1b FAILED. Unexpected summary columns found: ", paste(presentCols, collapse = ", "))
} else {
  message("Test 1b PASSED. No direction/speed summary columns were added when addFeaturesSummary = FALSE.")
}

# 1c. addFeatSummary() called standalone on top of mtrDataNoSummary, mirroring
#     what computeMTR(addFeaturesSummary = TRUE) does internally for the
#     passerine_type class, using the new 'outputLabel' naming.
# ===========================================================================
mtrDataNoSummary = addFeatSummary(
  mtrDensVPTS = mtrDataNoSummary,
  echoData = dbData$echoData,
  class = "passerine_type",
  inputVariable = "feature37.speed",
  outputLabel = "Speed",
  nCores = 1
)
stopifnot("speedMean.passerine_type" %in% colnames(mtrDataNoSummary))
message("Test 1c PASSED. addFeatSummary() with outputLabel = 'Speed' added 'speedMean.passerine_type'.")

# 1d. addFeatSummary() called without 'outputLabel' falls back to the
#     original inputVariable-based naming scheme (backward compatibility).
# ===========================================================================
mtrDataNoSummary = addFeatSummary(
  mtrDensVPTS = mtrDataNoSummary,
  echoData = dbData$echoData,
  class = "passerine_type",
  inputVariable = "feature37.speed",
  nCores = 1
)
stopifnot("feature37.speed_mean.passerine_type" %in% colnames(mtrDataNoSummary))
message("Test 1d PASSED. addFeatSummary() without outputLabel falls back to 'feature37.speed_mean.passerine_type'.")

# ===========================================================================
# Test 2: interplay between addFeatSummary() and computeDensity()
# ===========================================================================

# 2a. computeDensity() with addFeaturesSummary = TRUE -> direction/speed
#     summary columns should be added automatically, for "allClasses" and for
#     every class in classSelection.
# ===========================================================================
densityData = computeDensity(
  dbName = dbName,
  echoes = dbData$echoData,
  classSelection = classSelection.mtr,
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
  addFeaturesSummary = TRUE,
  computeAltitudeDistribution = FALSE
)

missingCols = setdiff(expectedCols, colnames(densityData))
if (length(missingCols) > 0) {
  stop("Test 2a FAILED. Missing expected columns: ", paste(missingCols, collapse = ", "))
} else {
  message("Test 2a PASSED. All expected direction/speed summary columns are present.")
}
print(densityData[1, expectedCols])

# 2b. computeDensity() with addFeaturesSummary = FALSE -> none of the
#     direction/speed summary columns should be added.
# ===========================================================================
densityDataNoSummary = computeDensity(
  dbName = dbName,
  echoes = dbData$echoData,
  classSelection = classSelection.mtr,
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
  addFeaturesSummary = FALSE,
  computeAltitudeDistribution = FALSE
)

presentCols = intersect(expectedCols, colnames(densityDataNoSummary))
if (length(presentCols) > 0) {
  stop("Test 2b FAILED. Unexpected summary columns found: ", paste(presentCols, collapse = ", "))
} else {
  message("Test 2b PASSED. No direction/speed summary columns were added when addFeaturesSummary = FALSE.")
}

# ===========================================================================
# Test 3: createVPTS() populates standard VPTS columns via addFeatSummary()
#   createVPTS() always uses addFeatSummary() internally (allClasses only,
#   no per-class columns) to compute weighted flux statistics that map
#   directly to the ALOFT standard column names.
# ===========================================================================
mainOutputDir = file.path(getwd(), "results")

vptsDir = createVPTS(
  dbName = dbName,
  outputDir = mainOutputDir,
  echoes = dbData$echoData,
  classSelection = classSelection.mtr,
  altitudeRange = c(25, 1025),
  altitudeBinSize = 50,
  timeRange = c("2024-09-24 00:00", "2024-09-25 23:59"),
  timeBinDuration_sec = 1800,
  timeZone = "Etc/GMT0",
  protocolData = dbData$protocolData,
  visibilityData = dbData$visibilityData,
  siteData = dbData$siteData,
  sunriseSunset = dbData$sunriseSunset,
  manualBlindTimes = NULL,
  saveBlindTimes = FALSE,
  blindTimesOutputDir = mainOutputDir,
  blindTimeAsMtrZero = NULL,
  propObsTimeCutoff = 0.2
)

csvFiles = list.files(vptsDir, pattern = "\\.csv$", full.names = TRUE)
vptsCheck = readr::read_csv(csvFiles[1], show_col_types = FALSE)

# 3a. Standard VPTS columns should all be present
# ===========================================================================
stdCols = c("radar", "datetime", "height", "u", "v", "w", "ff", "dd",
            "sd_vvp", "gap", "eta", "dens", "dbz", "dbz_all", "n", "n_dbz",
            "n_all", "n_dbz_all", "rcs", "sd_vvp_threshold", "vcp",
            "radar_latitude", "radar_longitude", "radar_height",
            "radar_wavelength", "source_file")
missingCols = setdiff(stdCols, colnames(vptsCheck))
if (length(missingCols) > 0) {
  stop("Test 3a FAILED. Missing standard VPTS columns: ", paste(missingCols, collapse = ", "))
} else {
  message("Test 3a PASSED. All standard VPTS columns are present.")
}

# 3b. No extra class-specific columns should appear (VPTS has fixed schema)
# ===========================================================================
extraCols = setdiff(colnames(vptsCheck), stdCols)
if (length(extraCols) > 0) {
  stop("Test 3b FAILED. Unexpected extra columns in VPTS CSV: ", paste(extraCols, collapse = ", "))
} else {
  message("Test 3b PASSED. No extra columns beyond the ALOFT standard schema.")
}

# 3c. Bins with non-zero density should have non-NA u, v, ff, dd, n, rcs
# ===========================================================================
nonZeroBins = vptsCheck[!is.na(vptsCheck$dens) & vptsCheck$dens > 0, ]
if (nrow(nonZeroBins) > 0) {
  naCheck = sapply(c("u", "v", "ff", "dd", "n", "rcs"), function(col) {
    sum(is.na(nonZeroBins[[col]]))
  })
  allFilled = all(naCheck == 0)
  if (!allFilled) {
    warning("Test 3c WARNING. Some non-zero density bins have NA in: ",
            paste(names(naCheck)[naCheck > 0], collapse = ", "),
            " (may be expected if echoes lacked speed/direction/RCS values).")
  } else {
    message("Test 3c PASSED. All non-zero density bins have non-NA u, v, ff, dd, n, rcs.")
  }
  print(nonZeroBins[1, c("datetime", "height", "dens", "u", "v", "ff", "dd", "n", "rcs")])
} else {
  message("Test 3c SKIPPED. No non-zero density bins found in the first CSV file.")
}

# ===========================================================================
# Benchmark: vectorised addFeatSummary()
#   The previous foreach-based implementation looped over each time-altitude
#   bin sequentially (~7.4 s for feature2.azimuth, ~7.2 s for feature37.speed
#   at nCores = 1 on this machine). The vectorised implementation assigns bin
#   membership to all echoes at once with findInterval(), then computes all
#   per-bin statistics in a single dplyr::group_by/summarise() call.
# ===========================================================================
benchFeatures = c("feature2.azimuth" = "Direction", "feature37.speed" = "Speed")
benchResults  = data.frame(
  feature     = character(0),
  elapsed_sec = numeric(0)
)

for (cFeature in names(benchFeatures)) {
  cLabel = benchFeatures[[cFeature]]
  message(sprintf("  Benchmarking vectorised addFeatSummary() for '%s' ..", cFeature))

  elapsed = system.time(
    addFeatSummary(
      mtrDensVPTS   = mtrDataNoSummary,
      echoData      = dbData$echoData,
      class         = "allClasses",
      inputVariable = cFeature,
      outputLabel   = cLabel
    )
  )[["elapsed"]]

  benchResults = rbind(benchResults, data.frame(
    feature     = cFeature,
    elapsed_sec = round(elapsed, 2)
  ))
}

# Report
# ===========================================================================
message("\n--- addFeatSummary() vectorised benchmark ---")
print(benchResults, row.names = FALSE)
message("Reference (old per-bin foreach, nCores = 1): ~7.4 s (azimuth), ~7.2 s (speed)")
