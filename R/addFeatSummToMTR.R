#' @title addFeatSummToMTR
#' @author Birgen Haest, Baptiste Schmid
#' @description [addFeatSummToMTR()] adds weighted summary statistics (n,
#' mean, SD) of a selected feature to a MTR/Density/VPTS table. Each
#' time-altitude bin of `mtrDensVPTS` is summarised from the matching echoes
#' in `echoData`, weighted by their MTR-factor (`mtr_factor_rf`). When
#' `inputVariable` is `"feature2.azimuth"`, circular statistics are used
#' instead of linear statistics.
#' @param mtrDensVPTS dataframe with the MTR/Density/VPTS table created with
#' [computeMTR()], [computeDensity()], or [createVPTS()].
#' @param echoData dataframe with the echo data from the data list created
#' with [extractDbData()] or [compileData()].
#' @param class character string vector with the class(es) for which the
#' feature summary should be calculated. Default: `"allClasses"`, i.e. all
#' classes pooled together.
#' @param inputVariable Name of the feature, included in `echoData`, to
#' summarize.
#' @param nCores maximal number of cores used for parallelisation. Default
#' value is 2.
#'
#' @return `mtrDensVPTS`, with the added weighted summary statistics of
#' `inputVariable` for the requested `class`.
#' @family manipulation functions
#' @export
#' @importFrom foreach %dopar%
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
#'   timeBinDuration_sec = 1800,
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
#'   computeAltitudeDistribution = FALSE
#' )
#'
#' # Add the weighted mean flight speed for the passerine_type class
#' # ===========================================================================
#' mtrData = addFeatSummToMTR(
#'   mtrDensVPTS = mtrData,
#'   echoData = dbData$echoData,
#'   class = "passerine_type",
#'   inputVariable = "feature37.speed",
#'   nCores = 1
#' )
#' }
#'
# =============================================================================
addFeatSummToMTR = function(mtrDensVPTS,
                            echoData,
                            class = "allClasses",
                            inputVariable,
                            nCores = 2) {
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
    echoData[, inputVariable] = circular::rad(echoData[, inputVariable])
  }

  # Set the names of the columns to add to mtrDensVPTS
  # =============================================================================
  nCol = paste0(inputVariable, "_n.", classLabel)
  if (isCircular) {
    meanCol = paste0(inputVariable, "_circMean.", classLabel)
    sdCol = paste0(inputVariable, "_circSD.", classLabel)
  } else {
    meanCol = paste0(inputVariable, "_mean.", classLabel)
    sdCol = paste0(inputVariable, "_sd.", classLabel)
  }

  # Report progress
  # =============================================================================
  message(paste0(
    "Adding the weighted summary statistics of '", inputVariable,
    "' for class '", classLabel, "' to the MTR/Density/VPTS table.."
  ))

  # Register the parallel backend
  #  IF nCores == 1, processing is NOT PARALLELIZED, and a cluster is not
  #  created, since 'makeCluster(1)' + 'registerDoParallel()' would only add
  #  the overhead of a 1-worker PSOCK cluster without any speed benefit.
  # =============================================================================
  if (nCores > 1) {
    cParCluster = parallel::makeCluster(nCores, type = "PSOCK")
    doParallel::registerDoParallel(cParCluster)
  } else {
    foreach::registerDoSEQ()
  }

  # Calculate the weighted summary statistics for each time-altitude bin
  # =============================================================================
  summaryStats = foreach::foreach(
    binId = seq_len(nrow(mtrDensVPTS)),
    .combine = "rbind"
  ) %dopar% {
    echoesInBin = echoData[
      echoData$time_stamp_targetTZ >= mtrDensVPTS$timeChunkBegin[binId] &
        echoData$time_stamp_targetTZ < mtrDensVPTS$timeChunkEnd[binId] &
        echoData$feature1.altitude_AGL >= mtrDensVPTS$altitudeChunkBegin[binId] &
        echoData$feature1.altitude_AGL < mtrDensVPTS$altitudeChunkEnd[binId],
    ]
    values = echoesInBin[, inputVariable]
    weights = echoesInBin[, "mtr_factor_rf"]
    keep = !is.na(values) & !is.na(weights)
    values = values[keep]
    weights = weights[keep]

    if (length(values) == 0) {
      c(n = 0, mean = NA_real_, sd = NA_real_)
    } else if (isCircular) {
      c(
        n = length(values),
        mean = circular::deg(circhelp::weighted_circ_mean(x = values, w = weights)),
        sd = circhelp::weighted_circ_sd(x = values, w = weights)
      )
    } else {
      c(
        n = length(values),
        mean = stats::weighted.mean(x = values, w = weights),
        sd = descriptio::weighted.sd(x = values, weights = weights)
      )
    }
  }

  # Stop the cluster
  # =============================================================================
  if (nCores > 1) {
    parallel::stopCluster(cParCluster)
  }

  # Add the summary statistics to mtrDensVPTS
  # =============================================================================
  mtrDensVPTS[, nCol] = summaryStats[, "n"]
  mtrDensVPTS[, meanCol] = summaryStats[, "mean"]
  mtrDensVPTS[, sdCol] = summaryStats[, "sd"]

  # Report progress
  # =============================================================================
  message(paste0(
    "Finished adding the weighted summary statistics of '", inputVariable, "'.."
  ))

  # Return mtrDensVPTS with the added feature summary
  # =============================================================================
  return(mtrDensVPTS)
}