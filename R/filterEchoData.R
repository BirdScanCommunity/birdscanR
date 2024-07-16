#### filterEchoData -----------------------------------------------------------
#' @title filterEchoData
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com};
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @description With the function \code{filterEchoData} the echo data can be
#' filtered by several parameters. The function returns the filtered echo data.
#'
#' @param echoData dataframe with the echo data from the data list created by
#' the function \code{extractDBData}.
#' @param timeRangeTargetTZ Character vector of length 2, with start and end of
#' time range, formatted as "%Y-%m-%d %H:%M". Echoes outside the time range will
#' be excluded.
#' @param targetTimeZone "Etc/GMT0" String specifying the target time zone.
#' Default is "Etc/GMT0".
#' @param protocolData dataframe with the protocol data from the data list
#' created by the function \code{extractDBData} or a subset of it created by
#' the function \code{filterProtocolData}. Echoes not detected during the listed
#' protocols will be excluded.
#' @param classSelection character string vector with the classes that should
#' be included.
#' @param classProbCutOff numeric cutoff value for class probabilities. Echoes
#' with a lower class probability will be excluded.
#' @param altitudeRange_AGL numeric vector of length 2 with start and end of the
#' altitude range. Echoes outside the altitude range will be excluded.
#' @param manualBlindTimes dataframe with the manual blind times created by the
#' function \code{loadManualBlindTimes}.
#' @param echoValidator logical, if set to TRUE, echoes labelled by the echo
#' validator as “non-bio scatterer” will be excluded. If set to FALSE, all
#' echoes are included.
#'
#' @return returns the filtered echo data in the same format as provided in the
#' parameter \code{echoData}.
#' @export
#' @examples
#' \dontrun{
#' # Set server, database, and other input settings for data extraction
#' # ===========================================================================
#' dbServer <- "MACHINE\\SERVERNAME" # Set the name of your SQL server
#' dbName <- "db_Name" # Set the name of your database
#' dbDriverChar <- "SQL Server" # Set either "SQL Server" or "PostgreSQL"
#' mainOutputDir <- file.path(".", "results")
#' radarTimeZone <- "Etc/GMT0"
#' targetTimeZone <- "Etc/GMT0"
#' listOfRfFeaturesToExtract <- c(167, 168)
#' siteLocation <- c(47.494427, 8.716432)
#' sunOrCivil <- "civil"
#'
#' # Get data
#' # ===========================================================================
#' dbData <- extractDbData(
#'   dbDriverChar = dbDriverChar,
#'   dbServer = dbServer,
#'   dbName = dbName,
#'   saveDbToFile = TRUE,
#'   dbDataDir = mainOutputDir,
#'   radarTimeZone = radarTimeZone,
#'   targetTimeZone = targetTimeZone,
#'   listOfRfFeaturesToExtract = listOfRfFeaturesToExtract,
#'   siteLocation = siteLocation,
#'   sunOrCivil = sunOrCivil
#' )
#'
#' # Set input settings for filtering of the data
#' # ===========================================================================
#' timeRangeData <- c("2021-01-15 00:00", "2021-01-31 00:00")
#' classSelection <- c(
#'   "passerine_type", "wader_type", "swift_type",
#'   "large_bird", "unid_bird", "bird_flock"
#' )
#' classProbCutoff <- NULL
#' altitudeRange <- c(50, 1000)
#' data(manualBlindTimes)
#' cManualBlindTimes <- manualBlindTimes
#' useEchoValidator <- FALSE
#'
#' # Filter the echo data
#' # ===========================================================================
#' filteredEchoData <- filterEchoData(
#'   echoData = dbData$echoData,
#'   timeRangeTargetTZ = timeRangeData,
#'   targetTimeZone = targetTimeZone,
#'   protocolData - dbData$protocolData,
#'   classSelection = classSelection,
#'   classProbCutOff = classProbCutoff,
#'   altitudeRange_AGL = altitudeRange,
#'   manualBlindTimes = cManualBlindTimes,
#'   echoValidator = useEchoValidator
#' )
#' }
#'
filterEchoData <- function(echoData = NULL,
                           timeRangeTargetTZ = NULL,
                           targetTimeZone = "Etc/GMT0",
                           protocolData = NULL,
                           classSelection = NULL,
                           classProbCutOff = NULL,
                           altitudeRange_AGL = NULL,
                           manualBlindTimes = NULL,
                           echoValidator = FALSE) {
  # Check whether any input data is provided
  # =============================================================================
  if (is.null(echoData) || nrow(echoData) == 0) {
    stop("There is no echoData provided.. Please check your input!")
  }

  # Check whether all necessary columns are present in echoData
  # =============================================================================
  requiredEchoDataCols <- c(
    "time_stamp_targetTZ", "protocolID",
    "class", "class_probability", "feature1.altitude_AGL"
  )
  if (!all(requiredEchoDataCols %in% names(echoData))) {
    stop(paste0(
      "Not all necessary columns are present in echoData. Please ",
      "check your input for the following columns: ",
      paste(requiredEchoDataCols, collapse = ", ")
    ))
  }

  # Convert the time range input to a POSIXct object
  # =============================================================================
  timeRangeTargetTZ <- as.POSIXct(timeRangeTargetTZ,
    format = "%Y-%m-%d %H:%M",
    tz     = targetTimeZone
  )

  # Filter by time range
  # =============================================================================
  if ((!is.null(timeRangeTargetTZ)) &&
    (length(timeRangeTargetTZ) == 2) &&
    (methods::is(timeRangeTargetTZ, "POSIXct"))) {
    echoData <- echoData[(echoData$time_stamp_targetTZ > timeRangeTargetTZ[1]) &
      (echoData$time_stamp_targetTZ < timeRangeTargetTZ[2]), ]
  }

  # Filter by protocols
  # =============================================================================
  if ((!is.null(protocolData)) &&
    (length(protocolData) > 0) &&
    (c("protocolID") %in% names(protocolData))) {
    echoData <- echoData[echoData$protocolID %in% protocolData$protocolID, ]
  }

  # Filter by classes
  # =============================================================================
  if ((!is.null(classSelection)) &&
    (is.character(classSelection))) {
    echoData <- echoData[echoData$class %in% classSelection, ]
  }

  # Filter by classprobability
  # =============================================================================
  if ((!is.null(classProbCutOff)) &&
    (is.numeric(classProbCutOff))) {
    echoData <- echoData[echoData$class_probability > classProbCutOff, ]
  }

  # Filter by altitudeRange
  # =============================================================================
  if ((!is.null(altitudeRange_AGL)) &&
    (length(altitudeRange_AGL) == 2) &&
    (is.numeric(altitudeRange_AGL))) {
    echoData <- echoData[(echoData$feature1.altitude_AGL > altitudeRange_AGL[1]) &
      (echoData$feature1.altitude_AGL < altitudeRange_AGL[2]), ]
  }

  # Filter by manualBlindTimes
  # =============================================================================
  if ((!is.null(manualBlindTimes)) &&
    (all(c("start_targetTZ", "stop_targetTZ") %in% names(manualBlindTimes))) &&
    (methods::is(manualBlindTimes$start_targetTZ, "POSIXct")) &&
    (methods::is(manualBlindTimes$stop_targetTZ, "POSIXct"))) {
    echoDataInBlindTime <- rep(FALSE, length(echoData[, 1]))
    for (i in 1:length(manualBlindTimes[, 1])) {
      echoDataInBlindTime <- echoDataInBlindTime |
        ((echoData$time_stamp_targetTZ >= manualBlindTimes$start_targetTZ[i]) &
          (echoData$time_stamp_targetTZ <= manualBlindTimes$stop_targetTZ[i]))
    }
    echoData <- echoData[!echoDataInBlindTime, ]
  }

  # Filter by echovalidator
  # =============================================================================
  if (echoValidator == TRUE) {
    echoData <- echoData[(echoData$echoValidationType == "bio scatterer") |
      (is.na(echoData$echoValidationType)), ]
  }

  # Return echo data
  # =============================================================================
  return(echoData)
}

# filterProtocolData(data$protocolData, pulseTypeSelection, rotationSelection)
# filterEchoData(echoData = data$echoData, timeRangeTargetTZ = timeRangeEchoData, protocolData = protocolDataSubset, classSelection = classSelection, classProbCutOff = classProbCutoff, altitudeRange_AGL = altitudeRange_AGL_25_5000, manualBlindTimes = manualBlindTimes, echoValidator = TRUE)
