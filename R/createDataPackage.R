#' @title Create MR1 Data Package
#' @author Baptiste Schmid, Fabian Hertner, Birgen Haest
#' @description [createDataPackage()] compiles data from [extractDbData()] into
#' a standardized MR1 data package, applying filters on time range, pulse type,
#' rotation, echo class, altitude, and class probability. The output follows the
#' MR1 Data Standard (one package per campaign): `echoData`, `protocolData`,
#' `blindTimesData`, `sunriseSunset`, `siteData`, `filterParameters`,
#' and `metaData`. Filtering is applied via [filterEchoData()] and
#' [filterProtocolData()]. When `outputDirPath` is provided, the package is
#' saved by default as standardized CSV and YAML files suitable for Zenodo
#' deposit; saving as RDS requires `saveAsRDS = TRUE`.
#' @param dbExtract named list with the database tables extracted by [extractDbData()].
#' @param siteData dataframe/vector with the database site table.
#' @param manualBlindTimesFile path to the csv file containing manual blind times.
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
#' @param classSelection character string vector with the classes that should be
#' included. Default is `NULL`: all classes are included.
#' @param classProbCutOff numeric cutoff value for class probabilities. Echoes
#' with a lower class probability will be excluded. Default is `NULL`: no cutoff
#' applied.
#' @param altitudeRange numeric vector of length 2 with start and end of the
#' altitude range in metres a.g.l. Echoes outside the altitude range will be
#' excluded. Default is `NULL`: no altitude filtering applied.
#' @param echoValidator logical. If set to `FALSE` (default), no additional
#' filter is applied; if set to `TRUE`, echoes labelled by the echo validator as
#' `"non-bio scatterer"` will be excluded.
#' @param outputDirPath Directory path where output files are saved. The output
#' filename is auto-generated from the active filter parameters. If `NULL`
#' (default), no files are written.
# #' @param tagOutputFile Vector of two elements `c(prefix, suffix)` inserted
# #' around the auto-generated filename. Either element may be `NULL`. Only used
# #' when `outputDirPath` is not `NULL`.
#' @param saveAsRDS logical. If `TRUE`, saves the full data list as a single
#' `.rds` file in `outputDirPath`. Useful as an in-session cache but not
#' required for the MR1 data standard. Default is `FALSE`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{echoData}{Filtered echo data.}
#'   \item{protocolData}{Filtered protocol data (columns defined by MR1 standard).}
#'   \item{visibilityData}{Filtered blind times data.}
#'   \item{sunriseSunset}{Filtered sunrise/sunset data (columns defined by MR1 standard).}
#'   \item{siteData}{Radar and site metadata, with `targetTimeZone` added.}
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
#' timeRangeTargetTZ = c("2024-09-24 00:00", "2024-09-25 23:59")
#'
#' # Set manual blind times to NULL (no manual blind times)
#' # ===========================================================================
#' cManualBlindTimes = NULL
#'
#' # Create MR1 data package
#' # ===========================================================================
#' dataPackage = createDataPackage(
#'   dbExtract            = dbData
#'   manualBlindTimesFile = cManualBlindTimes,
#'   dbName               = dbName,
#'   timeRangeTargetTZ    = timeRangeTargetTZ,
#'   targetTimeZone       = targetTimeZone,
#'   pulseTypeSelection   = "S",
#'   classSelection       = c("passerine_type"),
#'   outputDirPath        = getwd()
#' )
#' }
createDataPackage = function(dbExtract,
                             manualBlindTimes = NULL,
                             dbName = NULL,
                             pulseTypeSelection = NULL,
                             rotationSelection = NULL,
                             timeRangeTargetTZ = NULL,
                             classSelection = NULL,
                             classProbCutOff = NULL,
                             altitudeRange = NULL,
                             echoValidator = FALSE,
                             outputDirPath = NULL,
                             saveAsRDS = FALSE) {

# Check whether the required data frames are present in the input list
# ==============================================================================
  requiredDataFrames <- c(
    "echoData", "protocolData", "visibilityData",
    "sunriseSunset", "siteData", "TimeZone"
  )
  missingDataFrames <- setdiff(requiredDataFrames, names(dbExtract))
  if (length(missingDataFrames) > 0) {
    stop(paste("The following required data frames are missing in 'dbExtract':",
      paste(missingDataFrames, collapse = ", ")
    ))
  }

# Extract the required data frames from the input list
# ==============================================================================
  echoData <- dbExtract$echoData
  protocolData <- dbExtract$protocolData
  visibilityData <- dbExtract$visibilityData
  sunriseSunset <- dbExtract$sunriseSunset
  siteData <- dbExtract$siteData
  timeZone <- dbExtract$TimeZone

# Input validation
# ==============================================================================
  if (!is.data.frame(echoData))
    stop("'echoData' must be a data frame.")
  }
  if (!is.data.frame(protocolData)) {
    stop("'protocolData' must be a data frame.")
  }
  if (!is.null(visibilityData) && !is.data.frame(visibilityData)) {
    stop("'visibilityData' must be a data frame or NULL.")
  }
  if (!is.data.frame(sunriseSunset)) {
    stop("'sunriseSunset' must be a data frame.")
  if (!is.data.frame(siteData))
    stop("'siteData' must be a data frame.")
  if (!is.data.frame(timeZone))
    stop("'timeZone' must be a data frame.")
  if (!is.null(manualBlindTimesFile) && !file.exists(manualBlindTimesFile))
    stop("'manualBlindTimesFile' must be a data frame or NULL.")
  }
  if (!is.null(dbName) && (!is.character(dbName) || length(dbName) != 1)) {
    stop("'dbName' must be a single character string or NULL.")
  }
  if (!is.null(pulseTypeSelection)) {
    if (!is.character(pulseTypeSelection) || !all(pulseTypeSelection %in% c("S", "M", "L"))) {
      stop("'pulseTypeSelection' must be a character vector with values in c('S', 'M', 'L'), or NULL.")
    }
  }
  if (!is.null(rotationSelection)) {
    if (!is.numeric(rotationSelection) || !all(rotationSelection %in% c(0, 1))) {
      stop("'rotationSelection' must be a numeric vector with values in c(0, 1), or NULL.")
    }
  }
  # if (!inherits(timeRangeTargetTZ, c("Date", "POSIXt")) &&
  #   (!is.character(timeRangeTargetTZ) || length(timeRangeTargetTZ) != 2)) {
  #   stop("'timeRangeTargetTZ' must be a character vector of length 2 (format '%Y-%m-%d %H:%M'), or a POSIXct/Date vector of length 2.")
  # }
  # if (!is.character(targetTimeZone) || length(targetTimeZone) != 1 ||
  #   !targetTimeZone %in% OlsonNames()) {
  #   stop("'targetTimeZone' must be a single valid time zone string (see OlsonNames()).")
  # }
  if (!is.null(classSelection) && !is.character(classSelection)) {
    stop("'classSelection' must be a character vector or NULL.")
  }
  if (!is.null(classProbCutOff) &&
    (!is.numeric(classProbCutOff) || length(classProbCutOff) != 1 ||
      classProbCutOff < 0 || classProbCutOff > 1)) {
    stop("'classProbCutOff' must be a single numeric value between 0 and 1, or NULL.")
  }
  if (!is.null(altitudeRange)) {
    if (!is.numeric(altitudeRange) || length(altitudeRange) != 2) {
      stop("'altitudeRange' must be a numeric vector of length 2, or NULL.")
    }
    if (altitudeRange[1] > altitudeRange[2]) {
      stop("'altitudeRange[1]' (start) must be <= 'altitudeRange[2]' (end).")
    }
  }
  if (!is.logical(echoValidator) || length(echoValidator) != 1) {
    stop("'echoValidator' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.null(outputDirPath)) {
    if (!is.character(outputDirPath) || length(outputDirPath) != 1) {
      stop("'outputDirPath' must be a single character string or NULL.")
    }
    if (!dir.exists(outputDirPath)) {
      stop(paste0("'outputDirPath' does not exist: ", outputDirPath))
    }
  }
  # if (length(tagOutputFile) != 2)
  #   stop("'tagOutputFile' must be a vector of length 2, e.g. c('prefix', 'suffix') or c(NULL, NULL).")
  if (!is.logical(saveAsRDS) || length(saveAsRDS) != 1) {
    stop("'saveAsRDS' must be a single logical value (TRUE or FALSE).")
  }

# Set the time window to use for the data package
# ==============================================================================
  if (is.null(timeRangeTargetTZ)) {
    startTime = min(echoData$time_stamp_targetTZ, na.rm = TRUE)
    stopTime  = max(echoData$time_stamp_targetTZ, na.rm = TRUE)
  } else {
    if (!inherits(timeRangeTargetTZ, "Date") && !inherits(timeRangeTargetTZ, "POSIXt")) {
      timeRangeTargetTZ = as.POSIXct(timeRangeTargetTZ, tz = timeZone$targetTimeZone)
    }
    startTime = timeRangeTargetTZ[1]
    stopTime  = timeRangeTargetTZ[2]
  }

# Prepare the metadata
# ==============================================================================
  # Metadata for ProtocolData
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

  # Metadata for SiteData
  # ============================================================================
  metasiteData <- data.frame(
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

  # Metadata for visibilityData (blind times)
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

  # Metadata for sunrise and sunset times
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

  # Metadata of the applied filtering to create the data package
  # ============================================================================
  metaDataPackageFilters <- data.frame(
    "colname" = c(
      "timeRangeTargetTZ",
      "pulseTypeSelection",
      "rotationSelection",
      "classSelection",
      "classProbCutOff",
      "altitudeRange",
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

# Metadata for echoData
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

  # Metadata for the applied filters to create the data package
  # ============================================================================
  ls_filters <- list(
    timeRangeTargetTZ    = timeRangeTargetTZ,
    pulseTypeSelection   = pulseTypeSelection,
    rotationSelection    = rotationSelection,
    classSelection       = classSelection,
    classProbCutOff      = classProbCutOff,
    altitudeRange        = altitudeRange,
    echoValidator        = echoValidator
  )
  echoDataSubset = echoDataSubset[, metaEcho$colname]

  # compile metadata into a list
  # =============================================================================
  ls_metaData <- list(
    echoData          = metaEcho,
    protocolData      = metaProtocol,
    visibilityData    = metaBlindTimes,
    sunriseSunset = metaSunriseSunset,
    siteData          = metasiteData,
    filterParameters  = metaDataPackageFilters,
    database          = dbName, # at the moment, only keep the name of the database, but additional information could be used: version of BirdscanR-package, name of the person who extracted the data, etc.
    birdscanR         = utils::packageVersion("birdscanR") # classifier version is included in the echo-dataset
  )


# Prepare the data
# ==============================================================================
  # Filter protocol data
  # ============================================================================
  protocolDataSubset = filterProtocolData(
    protocolData = protocolData,
    pulseTypeSelection = pulseTypeSelection,
    rotationSelection = rotationSelection
  )
  timesInd = (protocolDataSubset$startTime_targetTZ < stopTime) &
    (protocolDataSubset$stopTime_targetTZ > startTime)
  protocolDataSubset = protocolDataSubset[timesInd, ]
  protocolDataSubset = protocolDataSubset[, metaProtocol$colname]

  # Filter siteData
  # ============================================================================
  siteCols <- c(
    "radarID", "siteID", "siteCode", "siteName", "siteDesc",
    "projectStart_originTZ", "projectStart_targetTZ", "projectEnd_originTZ", "projectEnd_targetTZ",
    "longitude", "latitude", "altitude",
    "customer"
  )
  radarCols <- c(
    "type", "serialNo", "northOffset", "delta", "tiltAngle",
    "transmitPower", "antennaGainInDBi", "waveGuideAttenuation"
  )
  pulse_cols <- list(
    S = c("short0V", "shortSatLower", "shortSteepness", "shortSatUpper", "pulseLengthShort"),
    M = c("medium0V", "mediumSatLower", "mediumSteepness", "mediumSatUpper", "pulseLengthMedium"),
    L = c("long0V", "longSatLower", "longSteepness", "longSatUpper", "pulseLengthLong")
  )
  radarCols <- c(radarCols, pulse_cols[[pulseTypeSelection]])
  siteData <- siteData[, c(siteCols, radarCols)]

  # Filter visibilityData data
  # ============================================================================
  if (!any(names(visibilityData) == "type")) warning("The 'type' column is missing in the dataset 'visibilityData'. Use the output of the function 'mergeVisibilityAnd ManualBlindTime'.")
  timesInd = (visibilityData$start_targetTZ < stopTime) &
    (visibilityData$stop_targetTZ > startTime)
  blindTimesDataSubset = visibilityData[timesInd, ]

  # Pending note from Baptiste:
  # Need to wait for update of the function mergeVisibilityAndManualBlindTimes
  # blindTimesDataSubset =  blindTimesDataSubset[, metaBlindTimes$colname]

  # Filter twilight data
  # ============================================================================
  timesInd = (sunriseSunset$sunStart < stopTime) &
    (sunriseSunset$sunStop > startTime)
  sunriseSunsetSubset = sunriseSunset[timesInd, ]
  sunriseSunsetSubset = sunriseSunsetSubset[, metaSunriseSunset$colname]

  # Filter echo data
  # ============================================================================
  echoDataSubset = filterEchoData(
    echoData = echoData,
    timeRangeTargetTZ = timeRangeTargetTZ,
    targetTimeZone = timeZone$targetTimeZone,
    protocolData = protocolDataSubset,
    classSelection = classSelection,
    classProbCutOff = classProbCutOff,
    altitudeRange = altitudeRange,
    echoValidator = echoValidator
  )

  # Create the data package
  # ============================================================================
  dataPackage = list(
    echoData           = echoDataSubset,
    protocolData       = protocolDataSubset,
    visibilityData     = blindTimesDataSubset,
    sunriseSunset  = sunriseSunsetSubset,
    siteData           = siteData,
    filterParameters   = ls_filters,
    metaData           = ls_metaData
  )

# Create the data package
# ==============================================================================
  if (!is.null(outputDirPath) && length(outputDirPath) == 1) {
    # Create package name
    # ==========================================================================
    packageName = paste0("dataPackage_", dbName)

    # Default output data package creation¨
    # ==========================================================================
    if (!saveAsRDS){
      dir.create(file.path(outputDirPath, packageName), showWarnings = FALSE)
      outputPackage = frictionless::create_package()
      for (cResource in names(dataPackage)) {
        outputPackage = frictionless::add_resource(outputPackage,
                                                   data = dataPackage[cResource],
                                                   resource_name = "echodata")
      }
      frictionless::write_package(outputPackage,
                                  directory = file.path(outputDirPath, packageName),
                                  compress  = TRUE)
    }

    # if (name %in% c("filterParameters", "metaData")) {
    #   # Save as YAML for list elements
    #   file_path <- file.path(csvDirPath, paste0(name, ".yaml"))
    #   yaml::write_yaml(dataPackage[[name]], file = file_path)
    # } else {
    #   # Save as CSV for table elements
    #   file_path <- file.path(csvDirPath, paste0(name, ".csv"))
    #   utils::write.csv(dataPackage[[name]], file = file_path, row.names = FALSE)
    # }

    # Save RDS (optional)
    # =========================================================================
    if (saveAsRDS) {
      if (!is.null(dbName) && length(dbName) == 1) {
        fileName = paste0(packageName, ".rds")
        base::saveRDS(dataPackage, file = file.path(outputDirPath, fileName))
      }

    }
  } # end of if (!is.null(outputDirPath) && length(outputDirPath) == 1)

  return(dataPackage)
}
