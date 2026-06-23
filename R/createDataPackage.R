#' @title Create MR1 Data Package
#' @author Baptiste Schmid, Fabian Hertner, Birgen Haest
#' @description [createDataPackage()] compiles data from [extractDbData()] into
#' a standardized MR1 data package, applying filters on time range, pulse type,
#' rotation, echo class, altitude, and class probability. The output follows the
#' MR1 Data Standard (one package per campaign): `echoData`, `protocolData`,
#' `blindTimesData`, `sunriseSunsetData`, `radarSiteData`, `filterParameters`,
#' and `metaData`. Filtering is applied via [filterEchoData()] and
#' [filterProtocolData()]. When `outputDirPath` is provided, the package is
#' saved by default as standardized CSV and YAML files suitable for Zenodo
#' deposit; saving as RDS requires `saveAsRDS = TRUE`.
#' @param echoData dataframe with the echo data from the data list created with
#' [extractDbData()].
#' @param protocolData dataframe with the protocol data from the data list
#' created with [extractDbData()]. Echoes not detected during the listed
#' protocols will be excluded.
#' @param blindTimesData dataframe with the manual blind times created by
#' [loadManualBlindTimes()]. It includes the automated blind times induced by
#' changes in measurement protocol, and blind times added manually to remove
#' periods of incoherent data collection.
#' @param sunriseSunsetData dataframe with sunrise/sunset, and civil and
#' nautical dawn/dusk. Computed with [twilight()].
#' @param radarSiteData dataframe/vector with the database site table.
#' @param manualBlindTimes dataframe with the manual blind times created by the
#' function [loadManualBlindTimes()].
#' @param dbName Name of the database. Used as metadata and as part of the
#' auto-generated output filename.
#' @param pulseTypeSelection character vector with the pulse types which should
#' be included in the subset. Options: `S`, `M`, `L`, i.e. short-, medium-,
#' long-pulse, respectively. Default is `NULL`: no filtering applied based on
#' pulseType.
#' @param rotationSelection numeric vector to select the operation modes with
#' and/or without antenna rotation. Options: 0, 1. (0 = no rotation,
#' 1 = rotation). Default is `NULL`: no filtering applied based on rotation mode.
#' @param timeRangeTargetTZ Character vector of length 2, with start and end of
#' time range, formatted as `"%Y-%m-%d %H:%M"`. Echoes outside the time range
#' will be excluded.
#' @param targetTimeZone String specifying the target time zone.
#' Default is `"Etc/GMT0"`.
#' @param classSelection character string vector with the classes that should be
#' included. Default is `NULL`: all classes are included.
#' @param classProbCutOff numeric cutoff value for class probabilities. Echoes
#' with a lower class probability will be excluded. Default is `NULL`: no cutoff
#' applied.
#' @param altitudeRange_AGL numeric vector of length 2 with start and end of the
#' altitude range in metres a.g.l. Echoes outside the altitude range will be
#' excluded. Default is `NULL`: no altitude filtering applied.
#' @param echoValidator logical. If set to `FALSE` (default), no additional
#' filter is applied; if set to `TRUE`, echoes labelled by the echo validator as
#' `"non-bio scatterer"` will be excluded.
#' @param outputDirPath Directory path where output files are saved. The output
#' filename is auto-generated from the active filter parameters. If `NULL`
#' (default), no files are written.
#' @param tagOutputFile Vector of two elements `c(prefix, suffix)` inserted
#' around the auto-generated filename. Either element may be `NULL`. Only used
#' when `outputDirPath` is not `NULL`.
#' @param saveCSV logical. If `TRUE` (default), creates a subdirectory named
#' after the auto-generated filename and writes the MR1-standard files:
#' `echoData.csv`, `protocolData.csv`, `blindTimesData.csv`,
#' `sunriseSunsetData.csv`, `radarSiteData.csv`, `filterParameters.yaml`,
#' `metaData.yaml`. Only used when `outputDirPath` is not `NULL`.
#' @param saveAsRDS logical. If `TRUE`, saves the full data list as a single
#' `.rds` file in `outputDirPath`. Useful as an in-session cache but not
#' required for the MR1 data standard. Default is `FALSE`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{echoData}{Filtered echo data.}
#'   \item{protocolData}{Filtered protocol data (columns defined by MR1 standard).}
#'   \item{visibilityData}{Filtered blind times data.}
#'   \item{sunriseSunsetData}{Filtered sunrise/sunset data (columns defined by MR1 standard).}
#'   \item{radarSiteData}{Radar and site metadata, with `targetTimeZone` added.}
#'   \item{filterParameters}{Named list of the filter settings applied.}
#'   \item{metaData}{Named list with per-table column metadata (name, type,
#'   description), plus `database` name and `birdscanR` package version.}
#' }
#' @family write file functions
#' @export
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
#' timeRangeTargetTZ = c("2024-09-24 00:00", "2024-09-25 23:59")
#'
#' # Set manual blind times to NULL (no manual blind times)
#' # ===========================================================================
#' cManualBlindTimes = NULL
#'
#' # Create MR1 data package
#' # ===========================================================================
#' dataPackage = createDataPackage(
#'   echoData           = dbData$echoData,
#'   protocolData       = dbData$protocolData,
#'   visibilityData     = dbData$visibilityData,
#'   sunriseSunsetData  = dbData$sunriseSunset,
#'   radarSiteData      = dbData$siteData,
#'   dbName             = dbName,
#'   timeRangeTargetTZ  = timeRangeTargetTZ,
#'   targetTimeZone     = targetTimeZone,
#'   pulseTypeSelection = "S",
#'   classSelection     = c("passerine_type"),
#'   outputDirPath      = getwd()
#' )
#' }
createDataPackage = function(echoData,
                             protocolData,
                             visibilityData,
                             sunriseSunsetData,
                             radarSiteData,
                             manualBlindTimes = NULL,
                             dbName = NULL,
                             pulseTypeSelection = NULL,
                             rotationSelection = NULL,
                             timeRangeTargetTZ = NULL,
                             targetTimeZone = "Etc/GMT0",
                             classSelection = NULL,
                             classProbCutOff = NULL,
                             altitudeRange_AGL = NULL,
                             echoValidator = FALSE,
                             outputDirPath = NULL,
                             tagOutputFile = c(NULL, NULL),
                             saveCSV = TRUE,
                             saveAsRDS = FALSE) {
  # Input validation
  # ============================================================================
  if (!is.data.frame(echoData))
    stop("'echoData' must be a data frame.")
  if (!is.data.frame(protocolData))
    stop("'protocolData' must be a data frame.")
  if (!is.null(visibilityData) && !is.data.frame(blindTimesData))
    stop("'visibilityData' must be a data frame or NULL.")
  if (!is.data.frame(sunriseSunsetData))
    stop("'sunriseSunsetData' must be a data frame.")
  if (!is.data.frame(radarSiteData))
    stop("'radarSiteData' must be a data frame.")
  if (!is.null(manualBlindTimes) && !is.data.frame(manualBlindTimes))
    stop("'manualBlindTimes' must be a data frame or NULL.")
  if (!is.null(dbName) && (!is.character(dbName) || length(dbName) != 1))
    stop("'dbName' must be a single character string or NULL.")
  if (!is.null(pulseTypeSelection)) {
    if (!is.character(pulseTypeSelection) || !all(pulseTypeSelection %in% c("S", "M", "L")))
      stop("'pulseTypeSelection' must be a character vector with values in c('S', 'M', 'L'), or NULL.")
  }
  if (!is.null(rotationSelection)) {
    if (!is.numeric(rotationSelection) || !all(rotationSelection %in% c(0, 1)))
      stop("'rotationSelection' must be a numeric vector with values in c(0, 1), or NULL.")
  }
  if (is.null(timeRangeTargetTZ))
    stop("'timeRangeTargetTZ' must be provided (character vector of length 2, or POSIXct/Date).")
  if (!inherits(timeRangeTargetTZ, c("Date", "POSIXt")) &&
      (!is.character(timeRangeTargetTZ) || length(timeRangeTargetTZ) != 2))
    stop("'timeRangeTargetTZ' must be a character vector of length 2 (format '%Y-%m-%d %H:%M'), or a POSIXct/Date vector of length 2.")
  if (!is.character(targetTimeZone) || length(targetTimeZone) != 1 ||
      !targetTimeZone %in% OlsonNames())
    stop("'targetTimeZone' must be a single valid time zone string (see OlsonNames()).")
  if (!is.null(classSelection) && !is.character(classSelection))
    stop("'classSelection' must be a character vector or NULL.")
  if (!is.null(classProbCutOff) &&
      (!is.numeric(classProbCutOff) || length(classProbCutOff) != 1 ||
       classProbCutOff < 0 || classProbCutOff > 1))
    stop("'classProbCutOff' must be a single numeric value between 0 and 1, or NULL.")
  if (!is.null(altitudeRange_AGL)) {
    if (!is.numeric(altitudeRange_AGL) || length(altitudeRange_AGL) != 2)
      stop("'altitudeRange_AGL' must be a numeric vector of length 2, or NULL.")
    if (altitudeRange_AGL[1] > altitudeRange_AGL[2])
      stop("'altitudeRange_AGL[1]' (start) must be <= 'altitudeRange_AGL[2]' (end).")
  }
  if (!is.logical(echoValidator) || length(echoValidator) != 1)
    stop("'echoValidator' must be a single logical value (TRUE or FALSE).")
  if (!is.null(outputDirPath)) {
    if (!is.character(outputDirPath) || length(outputDirPath) != 1)
      stop("'outputDirPath' must be a single character string or NULL.")
    if (!dir.exists(outputDirPath))
      stop(paste0("'outputDirPath' does not exist: ", outputDirPath))
  }
  if (length(tagOutputFile) != 2)
    stop("'tagOutputFile' must be a vector of length 2, e.g. c('prefix', 'suffix') or c(NULL, NULL).")
  if (!is.logical(saveCSV) || length(saveCSV) != 1)
    stop("'saveCSV' must be a single logical value (TRUE or FALSE).")
  if (!is.logical(saveAsRDS) || length(saveAsRDS) != 1)
    stop("'saveAsRDS' must be a single logical value (TRUE or FALSE).")

  # set the time window
  # ============================================================================
  if (!inherits(timeRangeTargetTZ, "Date") && !inherits(timeRangeTargetTZ, "POSIXt")) {
    timeRangeTargetTZ = as.POSIXct(timeRangeTargetTZ, tz = targetTimeZone)
  }
  startTime = timeRangeTargetTZ[1]
  stopTime = timeRangeTargetTZ[2]

  # Filter parameters
  # ============================================================================
  ls_filters <- list(
    timeRangeTargetTZ    = timeRangeTargetTZ,
    pulseTypeSelection   = pulseTypeSelection,
    rotationSelection    = rotationSelection,
    classSelection       = classSelection,
    classProbCutOff      = classProbCutOff,
    altitudeRange_AGL    = altitudeRange_AGL,
    echoValidator        = echoValidator
  )

  # meta data
  # ============================================================================
  metaFilters <- data.frame(
    "colname" = c(
      "timeRangeTargetTZ",
      "pulseTypeSelection",
      "rotationSelection",
      "classSelection",
      "classProbCutOff",
      "altitudeRange_AGL",
      "echoValidator"
    ),
    "type" = c(
      "POSIXct",
      "char",
      "integer",
      "char",
      "num",
      "num",
      "logical"
    ),
    "description" = c(
      "Time range (from beginning to end) of the filter period, in the time zone used for analyses.",
      'Pulse type is either "S" for Short-pulse, "M" for Medium pulse, or "L" for Long-pulse. See radar table for pulse duration',
      'Rotation is either "0"when the antenna is static, or "1" if the antena is rotating on its vertical axis. Flight speed and direction available only if the anteanna is rotating',
      "List of class - can be a subset of all avaialble classes",
      'PostHoc filter on classification. if "0", all echoes are used, if e.g. 0.4, only echoes with a class probability >= 0.4 are kept.',
      "Altitude range (agl) from the lowest to the highest.",
      'If set to FALSE (default), no additional filters is applied; if set to TRUE, echoes labelled by the echo validator as "non-bio scatterer" will be excluded.'
    )
  )

  # Filter protocol data
  # ============================================================================
  protocolDataSubset = filterProtocolData(
    protocolData = protocolData,
    pulseTypeSelection = pulseTypeSelection,
    rotationSelection = rotationSelection
  )
  TimesInd = (protocolDataSubset$startTime_targetTZ < stopTime) &
    (protocolDataSubset$stopTime_targetTZ > startTime)
  protocolDataSubset = protocolDataSubset[TimesInd, ]


  # meta data for ProtocolData
  # ============================================================================
  metaProtocol <- data.frame(
    "colname" = c(
      "protocolID", "siteID",
      "startTime_originTZ", "startTime_targetTZ", "stopTime_originTZ", "stopTime_targetTZ",
      "pulseType", "rotate", "stc", "threshold",
      "softwareVersion"
    ),
    "type" = c(
      "int", "int",
      "POSIXct", "POSIXct", "POSIXct", "POSIXct",
      "char", "int", "num", "num",
      "char"
    ),
    "description" = c(
      "Incremental ID of measurement periods - linked to EchoData and visibilityData.",
      "Site ID - linked to the site & radar data.",
      "Timestamp upon the start of the measurement period. TimeZone as given in DB",
      "Timestamp upon the start of the measurement period. TimeZone defined by the user, since 2020 usually UTC",
      "Timestamp upon the end of the measurement period. TimeZone as given in DB",
      "Timestamp upon the end of the measurement period. TimeZone defined by the user, since 2020 usually UTC",
      'Either "S" for Short-pulse, "M" for Medium pulse, "L" for Long-pulse. See radar table for pulse duration',
      '"0" when the antenna is static, "1" if the antenna is rotating on its vertical axis. Flight speed and direction available only if the antenna is rotating.',
      "Sensitivity Time Control in metres. Basically a distance to set the minimal detected object size. Key feature to calculate the MTR-factor of the echo.",
      "Detection threshold in dBm. Key feature to calculate the MTR-factor of the echo.",
      "Software version upon detection. Can differ from the classifier version."
    )
  )

  protocolDataSubset = protocolDataSubset[, metaProtocol$colname]

  # Filter Site & Radar data
  # ============================================================================
  radarSiteData$targetTimeZone = targetTimeZone

  mycols_site <- c(
    "radarID", "siteID", "siteCode", "siteName", "siteDesc",
    "targetTimeZone", # "originTimeZone",
    "projectStart_originTZ", "projectStart_targetTZ", "projectEnd_originTZ", "projectEnd_targetTZ",
    "longitude", "latitude", "altitude",
    "customer"
  )

  # Select according to Pulse Type
  # ============================================================================
  mycols_radar <- c(
    "type", "serialNo", "northOffset", "delta", "tiltAngle",
    "transmitPower", "antennaGainInDBi", "waveGuideAttenuation"
  )

  if (pulseTypeSelection == "S") {
    mycols_radar <- c(
      mycols_radar,
      c("short0V", "shortSatLower", "shortSteepness", "shortSatUpper", "pulseLengthShort")
    )
  }
  if (pulseTypeSelection == "M") {
    mycols_radar <- c(
      mycols_radar,
      c("medium0V", "mediumSatLower", "mediumSteepness", "mediumSatUpper", "pulseLengthMedium")
    )
  }
  if (pulseTypeSelection == "L") {
    mycols_radar <- c(
      mycols_radar,
      c("long0V", "longSatLower", "longSteepness", "longSatUpper", "pulseLengthLong")
    )
  }
  # filter variables
  radarSiteData <- radarSiteData[, c(mycols_site, mycols_radar)]
  # if( is.na(radarSiteData$timeShift) ) warning("The 'timeShift' parameter is missing. Edit the site table!")


  # meta data
  # ============================================================================
  metaRadarSiteData <- data.frame(
    "colname" = c(
      "radarID", "siteID", "siteCode", "siteName", "siteDesc",
      "timeZone_targetTZ", # "timeZone_originTZ",
      "projectStart_originTZ", "projectStart_targetTZ", "projectEnd_originTZ", "projectEnd_targetTZ",
      "longitude", "latitude", "altitude",
      "customer",
      "type", "serialNo", "northOffset", "delta", "tiltAngle",
      "transmitPower", "antennaGainInDBi", "waveGuideAttenuation",
      "xxx0V", "xxxSatLower", "xxxSteepness", "xxxSatUpper", "pulseLengthXxx"
    ),
    "type" = c(
      "int",
      "int",
      "char",
      "char",
      "char",
      "char",
      "POSIXct",
      "POSIXct",
      "POSIXct",
      "POSIXct",
      "num",
      "num",
      "int",
      "char",
      "char",
      "int",
      "num",
      "num",
      "num",
      "num",
      "num",
      "num",
      "num",
      "num",
      "num",
      "num",
      "num"
    ),
    "description" = c(
      "Serial number of radar unit - abbreviated.",
      "Radar location: Site ID (integer) given by radar operator.",
      "Radar location: Site code (three letters) given by radar operator.",
      "Radar location: full name.",
      "Radar location: optional further description",
      "Time Zone used for analyses, usually UTC",
      "Beginning of the data collection, using the time zone set on radar.",
      'Beginning of the data collection, using the time zone set for the analyses - see variable "timeZone_targetTZ".',
      "End of the data collection, using the time zone set on radar.",
      'End of the data collection, using the time zone set for the analyses - see variable "timeZone_targetTZ".',
      "Radar location: Longitude", # toDo: specify format
      "Radar location: Latitude",
      "Radar location: altitude above sea level",
      "Radar operator",
      'Model of radar unit, e.g. "BirdScan MR1" from Swiss Birdradar Solution.',
      "Serial number of radar unit - full",
      "Radar parameter: northOffset",
      "Radar parameter: delta",
      "Radar parameter: tiltAngle - a constant for BirdScan MR1.",
      "Radar parameter: transmitted power [W] - can vary between years because of exchange of the magnetron.",
      "Radar parameter: Antenna gain [dBi] is given by the antenna - a constant for BirdScan MR1.",
      "Radar parameter: Wave Guide attenuation [dB] is given by the antenna - a constant for BirdScan MR1.",
      "Pulse type parameter: xxx0V - Calibration. ",
      "Pulse type  parameter: xxxSatLower - Calibration.",
      "Pulse type  parameter: xxxSteepness - Calibration.",
      "Pulse type  parameter: xxxSatUpper - Calibration.",
      "Pulse type  parameter: pulseLengthXxx - Calibration - duration of the pulse length. This value ultimately define the range resolution."
    )
  )


  # Filter visibilityData data
  # ============================================================================
  # restrict the time range
  if (!any(names(visibilityData) == "type")) warning("The 'type' column is missing in the dataset 'visibilityData'. Use the output of the function 'mergeVisibilityAnd ManualBlindTime'.")
  TimesInd = (visibilityData$start_targetTZ < stopTime) &
    (visibilityData$stop_targetTZ > startTime)
  blindTimesDataSubset = visibilityData[TimesInd, ]

  # meta data
  # ============================================================================
  metaBlindTimes <- data.frame(
    "colname" = c(
      "type",
      "start_targetTZ",
      "stop_targetTZ",
      "protocolID"
    ),
    "type" = c(
      "char",
      "POSIXct",
      "POSIXct",
      "char"
    ),
    "description" = c(
      'Type of BlindTime.
                          Blindtime is used to calculate the effective duration of measurements during a temporal bin of the MTR table.
                          Common denominations are:
                          "protocolChange" that include the blindtime subsequent to the start of a new measurement period (protocolID),
                          "technical" denote periods with technical malfunction of the radar,
                          "rain" denote periods of precipitation.',
      "Beginning of the blind period",
      "End of the blind period",
      "ID of measurement periods - linked to protocol table"
    )
  )

  # need to wait for update of the function mergeVisibilityAndManualBlindTimes
  # blindTimesDataSubset =  blindTimesDataSubset[, metaBlindTimes$colname]


  # Filter twilight data
  # ============================================================================
  # restrict the time range on sunStart and sunStop
  TimesInd = (sunriseSunsetData$sunStart < stopTime) &
    (sunriseSunsetData$sunStop > startTime)
  sunriseSunsetDataSubset = sunriseSunsetData[TimesInd, ]
  # ToDo: use the twilight function if no dataset is included, but the site table include the necessary info on location.

  # meta data
  # ============================================================================
  metaSunriseSunset <- data.frame(
    "colname" = c(
      "is_night", "date",
      "sunStart", "sunStop",
      "civilStart", "civilStop",
      "nauticalStart", "nauticalStop"
    ),
    "type" = c(
      "int", "POSIXct",
      "POSIXct", "POSIXct",
      "POSIXct", "POSIXct",
      "POSIXct", "POSIXct"
    ),
    "description" = c(
      '"0" if daytime, "1" if nighttime,',
      "Date of event (in UTC)",
      "Time of sunrise in UTC - see site table for location",
      "Time of sunset in UTC - see site table for location",
      "Time of dawn (civil-twilight, 6\u00b0 below horizon) in UTC - see site table for location",
      "Time of dusk (civil-twilight, 6\u00b0 below horizon) in UTC - see site table for location",
      "Time of dawn (nautical-twilight, 9\u00b0 below horizon) in UTC - see site table for location",
      "Time of dusk (nautical-twilight, 9\u00b0 below horizon) in UTC - see site table for location"
    )
  )

  sunriseSunsetDataSubset = sunriseSunsetDataSubset[, metaSunriseSunset$colname]


  # Filter echo data
  # ============================================================================
  echoDataSubset = filterEchoData(
    echoData = echoData,
    timeRangeTargetTZ = timeRangeTargetTZ,
    targetTimeZone = targetTimeZone,
    protocolData = protocolDataSubset,
    classSelection = classSelection,
    classProbCutOff = classProbCutOff,
    altitudeRange_AGL = altitudeRange_AGL,
    manualBlindTimes = blindTimesDataSubset, # blindTimesDataSubset[which(blindTimesDataSubset$type != "protocolChange"), ],
    echoValidator = echoValidator
  )
  if (nrow(echoDataSubset) == 0) {
    warning(paste0("No echo remaining in the filtered data. Check 'TimeRange' and 'manualBlindTimes', or other filters such as 'pulse-type', 'classSelection', 'altitudeRange'"))
  }

  # meta data
  # ============================================================================
  metaEcho = NULL # TODO: implement echo column metadata
  # metaEcho <- data.frame(
  #   "colname" = c("dummy"
  #
  #   ),
  #   "type" = c('dummy'
  #
  #   ),
  #   "description" = c('dummy'
  #
  #   )
  # )

  if (!is.null(metaEcho)) {
    # subset to target columns
    echoDataSubset = echoDataSubset[, metaEcho$colname]
  }

  # compile meta data into a list
  # =============================================================================
  ls_metaData <- list(
    echoData          = metaEcho,
    protocolData      = metaProtocol,
    visibilityData    = metaBlindTimes,
    sunriseSunsetData = metaSunriseSunset,
    radarSiteData     = metaRadarSiteData,
    filterParameters  = metaFilters,
    database          = dbName, # at the moment, only keep the name of the database, but additional information could be used: version of BirdscanR-package, name of the person who extracted the data, etc.
    birdscanR         = utils::packageVersion("birdscanR") # classifier version is included in the echo-dataset
  )

  # Return the filtered protocol and echo data
  # =============================================================================
  dataPackage = list(
    echoData           = echoDataSubset,
    protocolData       = protocolDataSubset,
    visibilityData     = blindTimesDataSubset,
    sunriseSunsetData  = sunriseSunsetDataSubset,
    radarSiteData      = radarSiteData,
    filterParameters   = ls_filters,
    metaData           = ls_metaData
  )

  # save output
  if (!is.null(outputDirPath) && length(outputDirPath) == 1) {
    # =============================================================================
    # create filename
    # =========================================================================
    fileName = "dataPackage"

    # Add prefix from tagOutputFile to fileName
    # =========================================================================
    if (!is.null(tagOutputFile[1])) {
      prefix = tagOutputFile[1]
      fileName = paste(prefix, fileName, sep = "_")
    }

    # dbName for fileName
    # =========================================================================
    if (!is.null(dbName) && length(dbName) == 1) {
      fileName = paste(fileName, dbName, sep = "_")
    }

    # time range for fileName
    # =========================================================================
    if (!is.null(timeRangeTargetTZ) && length(timeRangeTargetTZ) == 2) {
      startTime = format(startTime, "%Y%m%d")
      stopTime = format(stopTime, "%Y%m%d")
      time = paste("time", startTime, "to", stopTime, sep = "")
      fileName = paste(fileName, time, sep = "_")
    }

    # altitude range for fileName
    # =========================================================================
    if (!is.null(altitudeRange_AGL) && length(altitudeRange_AGL) == 2) {
      altitudeRangeStart = altitudeRange_AGL[1]
      altitudeRangeStop = paste0(altitudeRange_AGL[2], "m")
      altitude = paste("alt", altitudeRangeStart, "to", altitudeRangeStop, sep = "")
      fileName = paste(fileName, altitude, sep = "_")
    }

    # pulseTypeSelection for fileName
    # =========================================================================
    if (!is.null(pulseTypeSelection) && length(pulseTypeSelection) == 1) {
      pulseTypeSelection_char = paste(sort(pulseTypeSelection), collapse = "")
      pulseTypeSelection_char = paste0("pulse", pulseTypeSelection_char, sep = "")
      fileName = paste(fileName, pulseTypeSelection_char, sep = "_")
    }

    # rotationSelection for fileName
    # =========================================================================
    if (!is.null(rotationSelection) && any(rotationSelection %in% c(1, 0))) {
      rotationSelection_char = paste(sort(rotationSelection), collapse = "")
      rotationSelection_char = paste0("rotation", rotationSelection_char, sep = "")
      fileName = paste(fileName, rotationSelection_char, sep = "_")
    }

    # classSelection for fileName
    # =========================================================================
    if (!is.null(classSelection)) {
      classAbbreviations$class <- trimws(classAbbreviations$class, which = "right")
      classAbbreviations$abbr <- trimws(classAbbreviations$abbr, which = "right")
      classes = paste(
        classAbbreviations$abbr[which(classAbbreviations$class %in%
          classSelection)],
        collapse = ""
      )
      fileName = paste(fileName, classes, sep = "_")
    } else {
      fileName = paste(fileName, "allClasses", sep = "_")
    }

    # classProbCutOff for fileName
    # =========================================================================
    if (!is.null(classProbCutOff) && length(classProbCutOff) == 1) {
      classProbCutOff_char <- substr(classProbCutOff, 3, 4)
      classProbCutOff_char = paste0("classProbCutOff.", classProbCutOff_char, sep = "")
      fileName = paste(fileName, classProbCutOff_char, sep = "_")
    }

    # echoValidator for fileName
    # =========================================================================
    if (echoValidator && length(echoValidator) == 1) {
      echoValidator_char = paste0("echoValidator", echoValidator, sep = "")
      fileName = paste(fileName, echoValidator_char, sep = "_")
    }

    # Add suffix from tagOutputFile to fileName
    # =========================================================================
    if (!is.null(tagOutputFile[2])) {
      suffix = tagOutputFile[2]
      fileName = paste(fileName, suffix, sep = "_")
    }

    # Save CSV (default)
    # =========================================================================
    if (saveCSV) {
      csvDirPath = file.path(outputDirPath, fileName)

      # Create a directory to store the CSV files
      dir.create(csvDirPath, showWarnings = FALSE)

      # Loop through each element in the list
      for (name in names(dataPackage)) {
        if (name %in% c("filterParameters", "metaData")) {
          # Save as YAML for list elements
          file_path <- file.path(csvDirPath, paste0(name, ".yaml"))
          yaml::write_yaml(dataPackage[[name]], file = file_path)
        } else {
          # Save as CSV for table elements
          file_path <- file.path(csvDirPath, paste0(name, ".csv"))
          utils::write.csv(dataPackage[[name]], file = file_path, row.names = FALSE)
        }
      }
    }

    # Save RDS (optional)
    # =========================================================================
    if (saveAsRDS) {
      rdsFileName = paste0(fileName, ".rds")
      rdsFilePathName <- file.path(outputDirPath, rdsFileName)
      base::saveRDS(dataPackage, file = rdsFilePathName)
    }
  } # end of if (!is.null(outputDirPath) && length(outputDirPath) == 1)

  return(dataPackage)
}
