#' @title Create MR1 Data Package
#' @author Baptiste Schmid, Fabian Hertner, Birgen Haest
#' @description [createDataPackage()] compiles data from [extractDbData()] into
#' a standardised MR1 data package for one radar campaign, following the
#' [Frictionless Data Package](https://datapackage.org) standard. Time range,
#' pulse type, rotation, echo class, class probability, altitude and echo
#' validator filters are applied with [filterProtocolData()] and
#' [filterEchoData()].
#'
#' The package contains five tabular resources: `echo_data`, `protocol_data`,
#' `blind_times`, `sunrise_sunset` and `site_data`. Each resource has a Table
#' Schema with field types, descriptions, units, and primary and foreign keys.
#' `echo_data` contains all echo columns, except those that are obsolete or
#' redundant: `mtr_factor_old`, `statistical_classification_old`,
#' `classification_id` and `sphere_dia` (old classification, outdated since
#' BirdScan v1.6), `feature15.polRatio` and `feature16.absPolarisation`
#' (outdated since BirdScan v1.6), `feature3.speed` (superseded by
#' `feature37.speed`), `time_string` (same as `time_stamp_originTZ`), and
#' `time_bin` and `altitude_bin` (refer to radar database tables that are not
#' part of the package).
#' The package-level metadata (title, description, owner, licence, embargo,
#' contributors with ORCID and DataCite roles, related identifiers, suggested
#' citation) are read from `metadata`. Spatial and temporal coverage, the
#' applied filters and the provenance (database, birdscanR and classifier
#' versions) are added automatically.
#'
#' When `outputDirPath` is provided, the package is written to
#' `outputDirPath/<campaignID>/` as `datapackage.json` plus one compressed CSV
#' file per resource, ready for a Zenodo deposit.
#' @param dbExtract named list with the database tables extracted by
#' [extractDbData()].
#' @param metadata path to a YAML file, or a named list with the same structure,
#' with the package-level metadata. Use [createDataPackageMetadataTemplate()]
#' to get a template.
#' @param manualBlindTimes data frame with the manual blind times created by
#' [loadManualBlindTimes()], or `NULL` (default) if there are none. They are
#' merged with the radar blind times by [mergeVisibilityAndManualBlindTimes()].
#' Only blind times that overlap a selected measurement period (protocol) are
#' included in the package.
#' @param dbName Name of the database. Stored in the package provenance.
#' Default is `NULL`.
#' @param pulseTypeSelection character vector with the pulse types which should
#' be included in the subset. Options: `S`, `M`, `L`, i.e. short-, medium-,
#' long-pulse, respectively. Default is `NULL`: no filtering applied based on
#' pulseType.
#' @param rotationSelection numeric vector to select the operation modes with
#' and/or without antenna rotation. Options: 0, 1. (0 = no rotation,
#' 1 = rotation). Default is `NULL`: no filtering applied based on rotation mode.
#' @param timeRangeTargetTZ Character vector of length 2, with start and end of
#' time range, formatted as `"%Y-%m-%d %H:%M"`, in the target time zone, or a
#' `POSIXct` vector of length 2. Echoes outside the time range will be
#' excluded. Default is `NULL`: the full time range of the echo data is used.
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
#' @param outputDirPath Directory where the data package is written, in a
#' subdirectory named after the `campaignID` in `metadata`. If `NULL`
#' (default), no files are written.
#' @param saveAsRDS logical. If `TRUE` and `outputDirPath` is provided, the
#' returned data package object is also saved as `<campaignID>.rds` in
#' `outputDirPath`. Default is `FALSE`.
#'
#' @return A Frictionless Data Package object (class `mr1dp` and
#' `datapackage`), i.e. a list with the package-level metadata and the
#' resources `echo_data`, `protocol_data`, `blind_times`, `sunrise_sunset` and
#' `site_data`. Read a resource as a data frame with
#' [frictionless::read_resource()].
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
#' mainOutputDir = tempdir()
#'
#' # Set manual blind times to NULL (no manual blind times)
#' # ===========================================================================
#' cManualBlindTimes = NULL
#'
#' # Write the package metadata template (fill it in for a real campaign)
#' # ===========================================================================
#' metadataFile = file.path(mainOutputDir, "CH_Sempach_2024_metadata.yaml")
#' createDataPackageMetadataTemplate(filePath = metadataFile, overwrite = TRUE)
#'
#' # Create MR1 data package
#' # ===========================================================================
#' dataPackage = createDataPackage(
#'   dbExtract          = dbData,
#'   metadata           = metadataFile,
#'   manualBlindTimes   = cManualBlindTimes,
#'   dbName             = dbName,
#'   timeRangeTargetTZ  = timeRangeTargetTZ,
#'   pulseTypeSelection = "S",
#'   outputDirPath      = mainOutputDir
#' )
#'
#' # Read a resource of the data package
#' # ===========================================================================
#' echoData = frictionless::read_resource(dataPackage, "echo_data")
#' }
createDataPackage = function(dbExtract,
                             metadata,
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
  requiredDataFrames = c(
    "echoData", "protocolData", "visibilityData",
    "sunriseSunset", "siteData", "TimeZone"
  )
  missingDataFrames = setdiff(requiredDataFrames, names(dbExtract))
  if (length(missingDataFrames) > 0) {
    stop(paste(
      "The following required data frames are missing in 'dbExtract':",
      paste(missingDataFrames, collapse = ", ")
    ))
  }

  # Extract the required data frames from the input list
  # ==============================================================================
  echoData = dbExtract$echoData
  protocolData = dbExtract$protocolData
  visibilityData = dbExtract$visibilityData
  sunriseSunset = dbExtract$sunriseSunset
  siteData = dbExtract$siteData
  timeZone = dbExtract$TimeZone

  # Input validation
  # ==============================================================================
  for (cName in c("echoData", "protocolData", "visibilityData", "sunriseSunset", "siteData")) {
    if (!is.data.frame(dbExtract[[cName]])) {
      stop(paste0("'dbExtract$", cName, "' must be a data frame."))
    }
  }
  if (!is.data.frame(timeZone) || !all(c("radarTimeZone", "targetTimeZone") %in% names(timeZone))) {
    stop("'dbExtract$TimeZone' must be a data frame with the columns 'radarTimeZone' and 'targetTimeZone'.")
  }
  if (!is.null(manualBlindTimes) &&
    (!is.data.frame(manualBlindTimes) ||
      !all(c("start_targetTZ", "stop_targetTZ", "type") %in% names(manualBlindTimes)))) {
    stop("'manualBlindTimes' must be NULL or a data frame created by loadManualBlindTimes().")
  }
  if (!is.null(dbName) && (!is.character(dbName) || length(dbName) != 1)) {
    stop("'dbName' must be a single character string or NULL.")
  }
  if (!is.null(pulseTypeSelection) &&
    (!is.character(pulseTypeSelection) || !all(pulseTypeSelection %in% c("S", "M", "L")))) {
    stop("'pulseTypeSelection' must be a character vector with values in c('S', 'M', 'L'), or NULL.")
  }
  if (!is.null(rotationSelection) &&
    (!is.numeric(rotationSelection) || !all(rotationSelection %in% c(0, 1)))) {
    stop("'rotationSelection' must be a numeric vector with values in c(0, 1), or NULL.")
  }
  if (!is.null(timeRangeTargetTZ) && length(timeRangeTargetTZ) != 2) {
    stop("'timeRangeTargetTZ' must be a vector of length 2 (format '%Y-%m-%d %H:%M'), or NULL.")
  }
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
  if (!is.logical(saveAsRDS) || length(saveAsRDS) != 1) {
    stop("'saveAsRDS' must be a single logical value (TRUE or FALSE).")
  }

  # Read and check the package metadata
  # ==============================================================================
  packageMetadata = .readPackageMetadata(metadata)
  metadataProblems = .checkPackageMetadata(packageMetadata)
  if (length(metadataProblems) > 0) {
    stop(paste0(
      "The package metadata are not valid:\n",
      paste0("- ", metadataProblems, collapse = "\n")
    ))
  }

  # Set the time window to use for the data package
  # ==============================================================================
  targetTZ = timeZone$targetTimeZone
  if (is.null(timeRangeTargetTZ)) {
    startTime = min(echoData$time_stamp_targetTZ, na.rm = TRUE)
    stopTime = max(echoData$time_stamp_targetTZ, na.rm = TRUE)
    # filterEchoData() uses strict inequalities at minute resolution, so widen
    # the window by one minute to keep the first and last echo
    echoTimeRange = format(c(startTime - 60, stopTime + 60), "%Y-%m-%d %H:%M", tz = targetTZ)
  } else {
    if (inherits(timeRangeTargetTZ, "POSIXt")) {
      startTime = timeRangeTargetTZ[1]
      stopTime = timeRangeTargetTZ[2]
    } else {
      timeRangePosix = as.POSIXct(as.character(timeRangeTargetTZ),
        format = "%Y-%m-%d %H:%M", tz = targetTZ
      )
      if (any(is.na(timeRangePosix))) {
        stop("'timeRangeTargetTZ' could not be parsed; use the format '%Y-%m-%d %H:%M'.")
      }
      startTime = timeRangePosix[1]
      stopTime = timeRangePosix[2]
    }
    echoTimeRange = format(c(startTime, stopTime), "%Y-%m-%d %H:%M", tz = targetTZ)
  }
  if (startTime >= stopTime) {
    stop("The start of 'timeRangeTargetTZ' must be before its end.")
  }

  # Prepare the data
  # ==============================================================================
  # Filter protocol data
  # ============================================================================
  protocolDataSubset = filterProtocolData(
    protocolData       = protocolData,
    pulseTypeSelection = pulseTypeSelection,
    rotationSelection  = rotationSelection
  )
  timesInd = (protocolDataSubset$startTime_targetTZ < stopTime) &
    (protocolDataSubset$stopTime_targetTZ > startTime)
  protocolDataSubset = protocolDataSubset[timesInd, ]
  if (nrow(protocolDataSubset) == 0) {
    stop("No measurement periods (protocols) remain after filtering by time range, pulse type and rotation.")
  }
  protocolCols = c(
    "protocolID", "siteID",
    "startTime_originTZ", "startTime_targetTZ", "stopTime_originTZ", "stopTime_targetTZ",
    "pulseType", "rotate", "stc", "threshold", "blockTime", "softwareVersion"
  )
  protocolDataOut = protocolDataSubset[, intersect(protocolCols, names(protocolDataSubset))]

  # Filter echo data
  # ============================================================================
  echoDataSubset = filterEchoData(
    echoData          = echoData,
    timeRangeTargetTZ = echoTimeRange,
    targetTimeZone    = targetTZ,
    protocolData      = protocolDataSubset,
    classSelection    = classSelection,
    classProbCutOff   = classProbCutOff,
    altitudeRange_AGL = altitudeRange,
    echoValidator     = echoValidator
  )

  # Remove echo columns that are obsolete (old classification, outdated since
  # BirdScan v1.6, superseded), redundant, or refer to radar database tables
  # that are not part of the data package
  # ============================================================================
  excludedEchoCols = c(
    "mtr_factor_old", "statistical_classification_old",
    "classification_id", "sphere_dia",
    "feature15.polRatio", "feature16.absPolarisation", "feature3.speed",
    "time_string", "time_bin", "altitude_bin"
  )
  echoDataSubset = echoDataSubset[, setdiff(names(echoDataSubset), excludedEchoCols)]

  # Merge radar and manual blind times, and link them to the protocols
  # ============================================================================
  blindTimes = mergeVisibilityAndManualBlindTimes(
    visibilityData   = visibilityData,
    manualBlindTimes = manualBlindTimes,
    protocolData     = protocolDataSubset
  )
  timesInd = (blindTimes$start_targetTZ < stopTime) &
    (blindTimes$stop_targetTZ > startTime)
  blindTimes = blindTimes[timesInd, c("type", "start_targetTZ", "stop_targetTZ")]
  blindTimes$type = as.character(blindTimes$type)
  blindTimes$protocolID = vapply(seq_len(nrow(blindTimes)), function(i) {
    overlap = which((protocolDataSubset$startTime_targetTZ < blindTimes$stop_targetTZ[i]) &
      (protocolDataSubset$stopTime_targetTZ > blindTimes$start_targetTZ[i]))
    if (length(overlap) == 0) NA_integer_ else as.integer(protocolDataSubset$protocolID[overlap[1]])
  }, integer(1))
  # Drop blind times outside the selected protocols (e.g. those of pulse types
  # that were filtered out); they do not affect the observation time
  blindTimes = blindTimes[!is.na(blindTimes$protocolID), ]

  # Filter twilight data
  # ============================================================================
  timesInd = (sunriseSunset$sunStart < stopTime) &
    (sunriseSunset$sunStop > startTime)
  sunriseSunsetCols = c(
    "is_night", "date", "sunStart", "sunStop",
    "civilStart", "civilStop", "nauticalStart", "nauticalStop"
  )
  sunriseSunsetSubset = sunriseSunset[timesInd, intersect(sunriseSunsetCols, names(sunriseSunset))]

  # Select site and radar data, incl. the calibration of the used pulse types
  # ============================================================================
  siteCols = c(
    "radarID", "siteID", "siteCode", "siteName", "siteDesc",
    "timeZone_originTZ", "timeZone_targetTZ",
    "projectStart_originTZ", "projectStart_targetTZ",
    "projectEnd_originTZ", "projectEnd_targetTZ",
    "longitude", "latitude", "altitude", "radarOrientation", "customer"
  )
  radarCols = c(
    "type", "serialNo", "northOffset", "delta", "tiltAngle",
    "transmitPower", "antennaGainInDBi", "waveGuideAttenuation"
  )
  pulseCols = list(
    S = c("short0V", "shortSatLower", "shortSteepness", "shortSatUpper", "pulseLengthShort"),
    M = c("medium0V", "mediumSatLower", "mediumSteepness", "mediumSatUpper", "pulseLengthMedium"),
    L = c("long0V", "longSatLower", "longSteepness", "longSatUpper", "pulseLengthLong")
  )
  usedPulseTypes = intersect(names(pulseCols), unique(protocolDataSubset$pulseType))
  siteData$timeZone_originTZ = timeZone$radarTimeZone
  siteData$timeZone_targetTZ = targetTZ
  siteDataOut = siteData[, intersect(
    c(siteCols, radarCols, unlist(pulseCols[usedPulseTypes], use.names = FALSE)),
    names(siteData)
  ), drop = FALSE]

  # Write originTZ times as local clock time of the radar; CSV writing would
  # otherwise convert them to UTC
  # ============================================================================
  originTzToString = function(df) {
    for (col in grep("_originTZ$", names(df), value = TRUE)) {
      if (inherits(df[[col]], "POSIXt")) {
        df[[col]] = format(df[[col]], "%Y-%m-%d %H:%M:%S")
      }
    }
    return(df)
  }
  resources = list(
    site_data      = originTzToString(siteDataOut),
    protocol_data  = originTzToString(protocolDataOut),
    echo_data      = originTzToString(echoDataSubset),
    blind_times    = blindTimes,
    sunrise_sunset = sunriseSunsetSubset
  )
  resources = lapply(resources, function(df) {
    rownames(df) = NULL
    df
  })
  resourceInfo = list(
    site_data      = c("Site and radar", "Location of the radar and radar/calibration parameters."),
    protocol_data  = c("Measurement periods", "Measurement periods (protocols) with their radar settings."),
    echo_data      = c("Echoes", "Echoes detected by the radar, with their features, classification and MTR factors."),
    blind_times    = c("Blind times", "Periods during which the radar could not detect echoes (protocol changes, radar-registered and manual blind times)."),
    sunrise_sunset = c("Sunrise and sunset", "Sunrise, sunset and twilight times at the site.")
  )

  # Applied filters
  # ==============================================================================
  filterParameters = list(
    timeRangeTargetTZ  = .jsonArray(format(c(startTime, stopTime), "%Y-%m-%dT%H:%M:%S", tz = targetTZ)),
    pulseTypeSelection = .jsonArray(pulseTypeSelection),
    rotationSelection  = .jsonArray(rotationSelection),
    classSelection     = .jsonArray(classSelection),
    classProbCutOff    = classProbCutOff,
    altitudeRange      = .jsonArray(altitudeRange),
    echoValidator      = echoValidator,
    manualBlindTimes   = !is.null(manualBlindTimes)
  )

  # Build the frictionless data package
  # ==============================================================================
  packageProperties = .completePackageMetadata(
    md               = packageMetadata,
    siteData         = siteDataOut,
    echoData         = echoDataSubset,
    startTime        = startTime,
    stopTime         = stopTime,
    timeZone         = timeZone,
    filterParameters = filterParameters,
    dbName           = dbName
  )
  dataPackage = frictionless::create_package(packageProperties)
  fieldDefs = .readFieldDefinitions()
  for (cResource in names(resources)) {
    dataPackage = frictionless::add_resource(
      package       = dataPackage,
      resource_name = cResource,
      data          = resources[[cResource]],
      schema        = .buildResourceSchema(resources[[cResource]], cResource, fieldDefs),
      title         = resourceInfo[[cResource]][1],
      description   = resourceInfo[[cResource]][2]
    )
  }
  class(dataPackage) = c("mr1dp", class(dataPackage))

  # Write the data package
  # ==============================================================================
  if (!is.null(outputDirPath)) {
    packageDir = file.path(outputDirPath, packageMetadata$campaignID)
    writtenPackage = frictionless::write_package(dataPackage, directory = packageDir, compress = TRUE)

    # Rewrite datapackage.json without rounding numbers (write_package() keeps
    # only 4 decimals, e.g. for the coordinates)
    # ==========================================================================
    writtenPackage$directory = NULL
    packageJson = jsonlite::toJSON(unclass(writtenPackage),
      pretty = TRUE, null = "null", na = "null", auto_unbox = TRUE, digits = NA
    )
    write(packageJson, file.path(packageDir, "datapackage.json"))

    if (saveAsRDS) {
      saveRDS(dataPackage, file = file.path(outputDirPath, paste0(packageMetadata$campaignID, ".rds")))
    }
  }

  return(dataPackage)
}
