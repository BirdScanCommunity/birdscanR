#' @title addFeatSummary
#' @author Birgen Haest, Baptiste Schmid
#' @description [addFeatSummary()] adds weighted summary statistics (n,
#' mean, SD, and, for circular data, the mean resultant length "rho") of a
#' selected feature to a MTR/Density/VPTS table. Each time-altitude bin of
#' `mtrDensVPTS` is summarised from the matching echoes in `echoData`,
#' weighted by their MTR-factor (`mtr_factor_rf`). When `inputVariable` is
#' `"feature2.azimuth"`, circular statistics are used instead of linear
#' statistics.
#'
#' The implementation is fully vectorised: bin membership is assigned to all
#' echoes in a single [findInterval()] pass, then per-bin statistics are
#' computed with a single [dplyr::summarise()] call. This eliminates the
#' per-bin loop and makes execution fast regardless of the number of bins.
#' @param mtrDensVPTS dataframe with the MTR/Density/VPTS table created with
#' [computeMTR()], [computeDensity()], or [createVPTS()].
#' @param echoData dataframe with the echo data from the data list created
#' with [extractDbData()] or [compileData()].
#' @param class character string vector with the class(es) for which the
#' feature summary should be calculated. Default: `"allClasses"`, i.e. all
#' classes pooled together.
#' @param inputVariable Name of the feature, included in `echoData`, to
#' summarize.
#' @param outputLabel optional character string used to name the output
#' columns. When provided, the columns are named `"nEchoes<outputLabel>"`,
#' `"<outputLabel>Mean"`, `"<outputLabel>Rho"` (circular data only), and
#' `"<outputLabel>SD"` (with `outputLabel`'s first letter lower-cased for the
#' mean/rho/SD columns), followed by `.<class>`. E.g., with
#' `outputLabel = "Direction"`, the columns are named `nEchoesDirection`,
#' `directionMean`, `directionRho`, and `directionSD`. When `NULL` (default),
#' `inputVariable` is used to name the columns instead, preserving the
#' original naming scheme.
#' @param nCores Retained for backward compatibility; no longer used. The
#' implementation is vectorised and does not create a parallel cluster.
#'
#' @return `mtrDensVPTS`, with the added weighted summary statistics of
#' `inputVariable` for the requested `class`.
#' @family manipulation functions
#' @export
#'
#' @examples
#' \donttest{
#' # Load example data
#' # ===========================================================================
#' dbData = readRDS(system.file("extdata",
#'   "CH_Sempach_2024_SEP24_25_DataExtract.rds",
#'   package = "birdscanR"
#' ))
#' dbName = "CH_Sempach_2024_SEP24_25"
#' targetTimeZone = "Etc/GMT0"
#' timeRangeData = c("2024-09-24 00:00", "2024-09-25 23:59")
#'
#' # Compute migration traffic rate
#' # ===========================================================================
#' classSelection.mtr = c("passerine_type")
#' mtrData = computeMTR(
#'   dbName = dbName,
#'   echoes = dbData$echoData,
#'   classSelection = classSelection.mtr,
#'   altitudeRange = c(25, 1025),
#'   altitudeBinSize = 50,
#'   timeRange = timeRangeData,
#'   timeZone = targetTimeZone,
#'   sunriseSunset = dbData$sunriseSunset,
#'   sunOrCivil = "civil",
#'   crepuscule = "nauticalSolar",
#'   protocolData = dbData$protocolData,
#'   visibilityData = dbData$visibilityData,
#'   manualBlindTimes = NULL,
#'   saveBlindTimes = FALSE,
#'   blindTimesOutputDir = getwd(),
#'   blindTimeAsMtrZero = NULL,
#'   propObsTimeCutoff = 0,
#'   computePerDayNight = FALSE,
#'   computePerDayCrepusculeNight = FALSE,
#'   computeAltitudeDistribution = FALSE,
#'   addFeaturesSummary = FALSE
#' )
#'
#' # Add the weighted mean flight speed for the passerine_type class
#' # ===========================================================================
#' mtrData = addFeatSummary(
#'   mtrDensVPTS = mtrData,
#'   echoData = dbData$echoData,
#'   class = "passerine_type",
#'   inputVariable = "feature37.speed",
#'   outputLabel = "Speed"
#' )
#' }
#'
# =============================================================================
addFeatSummary = function(mtrDensVPTS,
                          echoData,
                          class = "allClasses",
                          inputVariable,
                          outputLabel = NULL,
                          nCores = 2) {
  # nCores is retained for backward compatibility and is silently ignored:
  # the vectorised implementation does not create a parallel cluster.

  # Determine whether the input variable should be treated as circular data
  # =============================================================================
  isCircular = inputVariable %in% "feature2.azimuth"

  # Restrict the echo data to the requested class(es)
  # =============================================================================
  classLabel = paste(class, collapse = "_")
  if (!("allClasses" %in% class)) {
    echoData = echoData[echoData$class %in% class, ]
  }

  # Convert the azimuth to radians, as required by the circular statistics
  # =============================================================================
  if (isCircular) {
    echoData[, inputVariable] = circular::rad(echoData[[inputVariable]])
  }

  # Set the names of the columns to add to mtrDensVPTS
  #  When 'outputLabel' is provided, columns are named after it (e.g.
  #  "nEchoesDirection.allClasses", "directionMean.allClasses",
  #  "directionRho.allClasses", "directionSD.allClasses"). Otherwise, fall
  #  back on 'inputVariable' for backward compatibility.
  # =============================================================================
  if (!is.null(outputLabel)) {
    labelLower = paste0(tolower(substring(outputLabel, 1, 1)), substring(outputLabel, 2))
    nCol = paste0("nEchoes", outputLabel, ".", classLabel)
    meanCol = paste0(labelLower, "Mean.", classLabel)
    rhoCol = paste0(labelLower, "Rho.", classLabel)
    sdCol = paste0(labelLower, "SD.", classLabel)
  } else {
    nCol = paste0(inputVariable, "_n.", classLabel)
    if (isCircular) {
      meanCol = paste0(inputVariable, "_circMean.", classLabel)
      rhoCol = paste0(inputVariable, "_circRho.", classLabel)
      sdCol = paste0(inputVariable, "_circSD.", classLabel)
    } else {
      meanCol = paste0(inputVariable, "_mean.", classLabel)
      sdCol = paste0(inputVariable, "_sd.", classLabel)
    }
  }

  # Report progress
  # =============================================================================
  message(paste0(
    "Adding the weighted summary statistics of '", inputVariable,
    "' for class '", classLabel, "' to the MTR/Density/VPTS table.."
  ))

  # =============================================================================
  # Vectorised bin assignment via findInterval()
  #  All echoes are assigned to their (timeChunkId, altitudeChunkId) bin in a
  #  single O(n log b) pass. A grouped dplyr::summarise() then computes all
  #  per-bin statistics at once — no per-bin loop, no parallel cluster.
  # =============================================================================

  # Build sorted, deduplicated bin lookup tables
  timeBinStartsNum = sort(unique(as.numeric(mtrDensVPTS$timeChunkBegin)))
  altBinStarts = sort(unique(mtrDensVPTS$altitudeChunkBegin))

  timeRows = mtrDensVPTS[
    match(timeBinStartsNum, as.numeric(mtrDensVPTS$timeChunkBegin)),
    c("timeChunkId", "timeChunkBegin", "timeChunkEnd")
  ]
  timeRows = timeRows[order(as.numeric(timeRows$timeChunkBegin)), ]

  altRows = mtrDensVPTS[
    match(altBinStarts, mtrDensVPTS$altitudeChunkBegin),
    c("altitudeChunkId", "altitudeChunkBegin", "altitudeChunkEnd")
  ]
  altRows = altRows[order(altRows$altitudeChunkBegin), ]

  # Assign each echo to its bin index (1-based; 0 means before first bin)
  tIdx = findInterval(as.numeric(echoData$time_stamp_targetTZ), timeBinStartsNum)
  aIdx = findInterval(echoData$feature1.altitude_AGL, altBinStarts)

  # Validate: echo must fall strictly within [begin, end) for both axes
  nT = nrow(timeRows)
  nA = nrow(altRows)
  inRange = (tIdx >= 1L & tIdx <= nT & aIdx >= 1L & aIdx <= nA)
  inRange[inRange] = (
    as.numeric(echoData$time_stamp_targetTZ[inRange]) <
      as.numeric(timeRows$timeChunkEnd[tIdx[inRange]]) &
      echoData$feature1.altitude_AGL[inRange] <
        altRows$altitudeChunkEnd[aIdx[inRange]]
  )

  # Keep only echoes that are in range and have non-NA value and weight
  values = echoData[[inputVariable]]
  weights = echoData[["mtr_factor_rf"]]
  keep = inRange & !is.na(values) & !is.na(weights)

  # =============================================================================
  # Grouped summarisation
  # =============================================================================
  if (any(keep)) {
    echoGroups = data.frame(
      timeChunkId = timeRows$timeChunkId[tIdx[keep]],
      altitudeChunkId = altRows$altitudeChunkId[aIdx[keep]],
      value = values[keep],
      weight = weights[keep],
      stringsAsFactors = FALSE
    )

    if (isCircular) {
      summaryDF = echoGroups %>%
        dplyr::group_by(timeChunkId, altitudeChunkId) %>%
        dplyr::summarise(
          n = dplyr::n(),
          mean = circular::deg(
            circhelp::weighted_circ_mean(x = value, w = weight)
          ),
          rho = circhelp::weighted_circ_rho(x = value, w = weight),
          sd = suppressWarnings(
            circhelp::weighted_circ_sd(x = value, w = weight)
          ),
          .groups = "drop"
        )
    } else {
      summaryDF = echoGroups %>%
        dplyr::group_by(timeChunkId, altitudeChunkId) %>%
        dplyr::summarise(
          n = dplyr::n(),
          mean = stats::weighted.mean(x = value, w = weight),
          sd = descriptio::weighted.sd(x = value, weights = weight),
          .groups = "drop"
        )
    }
  } else {
    # No echoes match — empty summary; all bins will get n=0 / NA after merge
    summaryDF = data.frame(
      timeChunkId     = integer(0),
      altitudeChunkId = integer(0),
      n               = integer(0),
      mean            = numeric(0),
      sd              = numeric(0)
    )
    if (isCircular) summaryDF$rho = numeric(0)
  }

  # =============================================================================
  # Map summarised results back to mtrDensVPTS row order
  #  Bins absent from summaryDF (no echoes) receive n = 0 and NA for stats.
  # =============================================================================
  binKey = paste(mtrDensVPTS$timeChunkId, mtrDensVPTS$altitudeChunkId)
  sumKey = paste(summaryDF$timeChunkId, summaryDF$altitudeChunkId)
  rowIdx = match(binKey, sumKey)

  mtrDensVPTS[, nCol] = ifelse(is.na(rowIdx), 0L, as.integer(summaryDF$n[rowIdx]))
  mtrDensVPTS[, meanCol] = ifelse(is.na(rowIdx), NA_real_, summaryDF$mean[rowIdx])
  if (isCircular) {
    mtrDensVPTS[, rhoCol] = ifelse(is.na(rowIdx), NA_real_, summaryDF$rho[rowIdx])
  }
  mtrDensVPTS[, sdCol] = ifelse(is.na(rowIdx), NA_real_, summaryDF$sd[rowIdx])

  # Report progress
  # =============================================================================
  message(paste0(
    "Finished adding the weighted summary statistics of '", inputVariable, "'.."
  ))

  return(mtrDensVPTS)
}
