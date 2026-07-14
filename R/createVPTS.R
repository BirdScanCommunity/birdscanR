#' @title createVPTS
#' @author Birgen Haest, Baptiste Schmid
#' @description This function creates VPTS CSV output files in line with the
#' ALOFT data standard, described [here](https://aloftdata.eu/vpts-csv/).
#' Note that this function only works on Birdscan MR1 database
#' versions >= 1.7.0.4 as the variable feature37.speed is required for the
#' density calculation.
#' @inheritParams computeDensity
#'
#' @param outputDir Character variable indicating where you want the VPTS
#' files to be stored. The function will create a subdirectory called "vpts"
#' within the specified `outputDir`.
#' @param siteData A data frame holding the site table, as extracted with
#' [extractDbData()] or [getSiteTable()].
#'
#' @return File path to the created VPTS CSV file.
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
#' mainOutputDir = file.path(".", "results")
#' targetTimeZone = "Etc/GMT0"
#' timeRangeData = c("2024-09-24 00:00", "2024-09-25 23:59")
#'
#' # Set manual blind times to NULL (no manual blind times)
#' # ===========================================================================
#' cManualBlindTimes = NULL
#'
#' # Create vpts files
#' # ===========================================================================
#' vptsDir = createVPTS(
#'   dbName = dbName,
#'   outputDir = mainOutputDir,
#'   echoes = dbData$echoData,
#'   altitudeRange = c(25, 1025),
#'   altitudeBinSize = 50,
#'   timeRange = timeRangeData,
#'   timeBinDuration_sec = 1800,
#'   timeZone = targetTimeZone,
#'   protocolData = dbData$protocolData,
#'   visibilityData = dbData$visibilityData,
#'   siteData = dbData$siteData,
#'   sunriseSunset = dbData$sunriseSunset,
#'   manualBlindTimes = cManualBlindTimes,
#'   saveBlindTimes = FALSE,
#'   blindTimesOutputDir = mainOutputDir,
#'   blindTimeAsMtrZero = NULL,
#'   propObsTimeCutoff = 0.2
#' )
#' }
# =============================================================================
createVPTS = function(dbName,
                      outputDir,
                      echoes,
                      classSelection = c(
                        "passerine_type", "wader_type",
                        "swift_type", "large_bird",
                        "unid_bird", "bird_flock"
                      ),
                      altitudeRange = c(50, 1500),
                      altitudeBinSize = 50,
                      timeRange,
                      timeBinDuration_sec = 900,
                      timeZone = "Etc/GMT0",
                      protocolData,
                      visibilityData,
                      siteData,
                      sunriseSunset,
                      manualBlindTimes = NULL,
                      saveBlindTimes = FALSE,
                      blindTimesOutputDir = getwd(),
                      blindTimeAsMtrZero = NULL,
                      propObsTimeCutoff = 0.2) {
  # Check if there is at least one echo in the echo dataset
  # =============================================================================
  if (nrow(echoes) == 0) {
    stop(paste0(
      "There are no echoes to calculate the vpts. Please check your ",
      "input dataset."
    ))
  }

  # Create altitudeBins
  # =============================================================================
  message("Creating altitude bins..")
  sequence = seq(altitudeRange[1], altitudeRange[2], altitudeBinSize)
  altitudeBins = data.frame(
    id = seq(1, (length(sequence) - 1), by = 1),
    begin = sequence[1:(length(sequence) - 1)],
    end = sequence[2:length(sequence)],
    size = NA_real_,
    avgAltitude = NA_real_
  )
  altitudeBins$size = altitudeBins$end - altitudeBins$begin
  altitudeBins$avgAltitude = ((altitudeBins$begin + altitudeBins$end)) / 2

  # Convert the timebin time range input to a POSIXct object
  # =============================================================================
  timeRange = as.POSIXct(timeRange,
    format = "%Y-%m-%d %H:%M",
    tz     = timeZone
  )

  # Create Timebins
  # =============================================================================
  message("Creating time bins..")
  timeBins = createTimeBins(
    timeRange = timeRange,
    timeBinDuration_sec = timeBinDuration_sec,
    timeZone = timeZone,
    sunriseSunset = sunriseSunset
  )

  # compute blindtimes
  # =============================================================================
  message("Calculating blind times..")
  blindTimes = mergeVisibilityAndManualBlindTimes(
    visibilityData = visibilityData,
    manualBlindTimes = manualBlindTimes,
    protocolData = protocolData
  )

  # Save blind times to file, if requested
  # =============================================================================
  if (saveBlindTimes) {
    saveRDS(blindTimes, file = file.path(
      blindTimesOutputDir,
      paste0(dbName, "_overallBlindTimes.rds")
    ))
  }

  # Subset echoes to target class(es)
  # =============================================================================
  message("Subsetting echo data..")
  echoes = echoes[echoes$class %in% classSelection, ]

  # Compute observation time for each timebin
  # =============================================================================
  message("Computing observation times for each timebin..")
  timeBins = computeObservationTime(
    timeBins = timeBins,
    protocolData = protocolData,
    blindTimes = blindTimes,
    blindTimeAsMtrZero = blindTimeAsMtrZero
  )

  # Remove echoes with NA in 'mtr_factor'
  # =============================================================================
  if (any(is.na(echoes$mtr_factor_rf))) {
    n = length(is.na(echoes$mtr_factor_rf))
    echoes = echoes[!is.na(echoes$mtr_factor_rf), ]
    message(paste0("Missing MTR-factors for ", n, " echoes, thus excluded from the VPTS calculation."))
  }

  # Remove echoes outside the heigth range
  # =============================================================================
  if (any(echoes$feature1.altitude_AGL > max(altitudeBins$end))) {
    index = which(echoes$feature1.altitude_AGL > max(altitudeBins$end))
    n = length(index)
    echoes = echoes[-index, ]
    message(paste0(n, " echoes above the defined altitude range, thus excldued from the VPTS calculation."))
  }

  # Abort if no echoes present
  # =============================================================================
  if (length(echoes[, ]) == 0) {
    stop("There are no echoes to compute the vpts on. Check your input settings.")
  }

  # Check if feature37.speed exists, stop processing if not
  # =============================================================================
  if (!"feature37.speed" %in% colnames(echoes)) {
    stop(paste0(
      "Couldn't find feature37.speed in the echo table. Are you ",
      "using a database version < 1.7.0.4? If so, please update",
      "your database."
    ))
  }

  # Combine time bins split by day/night
  # =============================================================================
  for (i in 2:nrow(timeBins)) {
    if (timeBins$id[i] == -1) {
      timeBins$stop[i - 1] = timeBins$stop[i]

      if (timeBins$duration_sec[i] >= timeBins$duration_sec[i - 1]) {
        timeBins$dayOrNight[i - 1] = timeBins$dayOrNight[i]
        timeBins$dateSunset[i - 1] = timeBins$dateSunset[i]
      }
      timeBins$duration_sec[i - 1] = timeBins$duration_sec[i - 1] + timeBins$duration_sec[i]
      timeBins$operationTime_sec[i - 1] = timeBins$operationTime_sec[i - 1] + timeBins$operationTime_sec[i]
      timeBins$blindTime_sec[i - 1] = timeBins$blindTime_sec[i - 1] + timeBins$blindTime_sec[i]
      timeBins$observationTime_h[i - 1] = timeBins$observationTime_h[i - 1] + timeBins$observationTime_h[i]
      timeBins$observationTime_sec[i - 1] = timeBins$observationTime_sec[i - 1] + timeBins$observationTime_sec[i]
      timeBins$proportionalTimeObserved[i - 1] = ifelse(timeBins$duration_sec[i - 1] > 0,
        (timeBins$observationTime_sec[i - 1] / timeBins$duration_sec[i - 1]),
        0
      )
    } else if ((timeBins$id[i - 1] != -1) &&
      difftime(timeBins$stop[i], timeBins$start[i]) != difftime(timeBins$stop[i - 1], timeBins$start[i - 1]) &&
      (i != length(timeBins[, 1]))) {
      timeBins$id[i + 1] = -1
    }
  }

  # Exclude combined time bins and reset time bins id
  # =========================================================================
  timeBins = timeBins[timeBins$id >= 0, ]
  timeBins = timeBins[order(timeBins$start), ]
  timeBins$id = seq(1, length(timeBins[, 1]))

  # Set timeChunk and altitudeChunk
  # =============================================================================
  timeAndAltitudeCombinations = expand.grid(
    timeChunkId = timeBins$id,
    altitudeChunkId = altitudeBins$id,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  # Report progress
  # =============================================================================
  message("Computing densities in each time-altitude bin..")

  # Add features to time and altitude chunk IDs
  # =============================================================================
  density = merge(timeAndAltitudeCombinations,
    data.frame(
      timeChunkId = timeBins$id,
      timeChunkDate = timeBins$date,
      timeChunkBegin = timeBins$start,
      timeChunkEnd = timeBins$stop,
      timeChunkDateSunset = timeBins$dateSunset,
      timeChunkDuration_sec = timeBins$duration_sec,
      observationTime_sec = timeBins$observationTime_sec,
      observationTime_h = timeBins$observationTime_h,
      operationTime_sec = timeBins$operationTime_sec,
      blindTime_sec = timeBins$blindTime_sec,
      proportionalTimeObserved = timeBins$proportionalTimeObserved,
      dayOrNight = as.character(timeBins$dayOrNight)
    ),
    by = "timeChunkId"
  )
  levels(density$dayOrNight) = names(table(timeBins$dayOrNight))
  density = merge(density,
    data.frame(
      altitudeChunkId = altitudeBins$id,
      altitudeChunkBegin = altitudeBins$begin,
      altitudeChunkEnd = altitudeBins$end,
      altitudeChunkSize = altitudeBins$size,
      altitudeChunkAvgAltitude = altitudeBins$avgAltitude
    ),
    by = "altitudeChunkId"
  )
  density = density[order(density$timeChunkId, density$altitudeChunkId), ]

  # Reorder columns as originally
  # =============================================================================
  density = density[, c(
    "timeChunkId", "timeChunkDate", "timeChunkBegin",
    "timeChunkEnd", "timeChunkDateSunset", "timeChunkDuration_sec",
    "observationTime_sec", "observationTime_h", "operationTime_sec",
    "blindTime_sec", "proportionalTimeObserved", "dayOrNight",
    "altitudeChunkId", "altitudeChunkBegin", "altitudeChunkEnd",
    "altitudeChunkSize", "altitudeChunkAvgAltitude"
  )]

  # ----------------------- DENSITY ---------------------------#
  # =============================================================================
  echoes$altitudeChunkId = as.integer(as.character(cut(echoes[, "feature1.altitude_AGL"],
    breaks = c(
      altitudeBins$begin,
      altitudeBins$end[nrow(altitudeBins)]
    ),
    label = altitudeBins$id,
    right = FALSE
  )))
  echoes$timeChunkId = as.integer(as.character(cut(echoes[, "time_stamp_targetTZ"],
    breaks = c(
      timeBins$start,
      timeBins$stop[nrow(timeBins)]
    ),
    label = timeBins$id,
    right = FALSE
  )))
  all_density = echoes %>%
    # Add information on effective observation time
    dplyr::left_join(
      x = .,
      y = density %>% dplyr::distinct(timeChunkId, observationTime_h),
      by = "timeChunkId"
    ) %>%
    # Calculate the density for each echo - will be summed up in a later step
    dplyr::mutate("density_echo" = mtr_factor_rf / observationTime_h / (3.6 * (.data$feature37.speed)) / (altitudeBinSize / 1000)) %>%
    # Group the data with time and height intervals
    dplyr::group_by(timeChunkId, altitudeChunkId) %>%
    dplyr::summarise(
      # count the number of echoes per time-height interval
      "nEchoes" = length(mtr_factor_rf),
      # sum the MTR-factors of all echoes per timeXheight interval
      # "sumOfMTRFactors" = sum(mtr_factor_rf, na.rm = TRUE),
      # sum the density of all echoes per timeXheight interval
      "density" = sum((.data$density_echo), na.rm = TRUE)
    ) %>%
    # Select and reorder the columns of interest
    # dplyr::select(timeChunkId, altitudeChunkId, nEchoes, sumOfMTRFactors, density)
    dplyr::select(timeChunkId, altitudeChunkId, nEchoes, density)

  density = dplyr::left_join(density, all_density,
    by = c("timeChunkId", "altitudeChunkId")
  )

  # Replace NA as ZERO for nEchoes, sumMTRfactors, density, if "proportionalTimeObserved"] != 0
  # =============================================================================
  i_index = which((density[, "nEchoes"] %in% NA) &
    (density[, "proportionalTimeObserved"] != 0))
  density[i_index, "nEchoes"] = 0
  # density[i_index , "sumOfMTRFactors"] = 0
  density[i_index, "density"] = 0

  # Density set back to NA if the observation time is less than the cutoff time
  # threshold
  # =============================================================================
  if (propObsTimeCutoff > 0) {
    i_index = which(density[, "proportionalTimeObserved"] < propObsTimeCutoff)
    density[i_index, "density"] = NA
  }

  # Coerce density to a plain data.frame so addFeatSummary()'s [<- column
  #  assignment works reliably (dplyr tibbles can silently drop new columns
  #  when assigned via [<-).
  # =============================================================================
  density = as.data.frame(density)

  # Compute per-echo u/v flux components and RCS in cm² so addFeatSummary()
  #  can derive weighted bin-level statistics vectorised over all bins at once.
  #  NAs propagate automatically: u/v are NA whenever speed or direction is NA;
  #  rcs_cm2 is NA when RCS2_RCS_max_lowpassed is NA.
  # =============================================================================
  echoes$u_component = echoes$feature37.speed * sin(echoes$feature2.azimuth * pi / 180)
  echoes$v_component = echoes$feature37.speed * cos(echoes$feature2.azimuth * pi / 180)
  echoes$rcs_cm2 = (10^(2 * echoes$RCS2_RCS_max_lowpassed)) * 10000

  # Compute weighted mean u, v, and RCS for each time-altitude bin using
  #  addFeatSummary() (allClasses only — VPTS has fixed standard columns,
  #  no per-class breakdown)
  # =============================================================================
  message("Computing weighted flux components and RCS per time-altitude bin..")
  for (cFeature in c("u_component", "v_component", "rcs_cm2")) {
    cLabel = switch(cFeature,
      u_component = "FluxU",
      v_component = "FluxV",
      rcs_cm2     = "WeightedRCS"
    )
    density = addFeatSummary(
      mtrDensVPTS = density,
      echoData = echoes,
      class = "allClasses",
      inputVariable = cFeature,
      outputLabel = cLabel
    )
  }

  # Create vpts along ALOFT standard
  #  u, v  : mtr-factor weighted mean flux components (m/s)
  #  ff    : flux speed = sqrt(u^2 + v^2) (NOTE: this is the magnitude of the
  #           mean flux vector, not the mean ground speed of individual birds)
  #  dd    : flux direction = atan2(u, v) in degrees (0–360, N=0, clockwise)
  #           (NOTE: same rationale as ff — flux direction, not mean bird direction)
  #  n     : number of echoes with valid speed AND direction per bin
  #  rcs   : mtr-factor weighted mean RCS in cm²
  # =============================================================================
  vpts = data.frame(
    radar            = siteData$radarID,
    datetime         = format(density$timeChunkBegin, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    height           = density$altitudeChunkBegin,
    u                = density$fluxUMean.allClasses,
    v                = density$fluxVMean.allClasses,
    w                = NA_real_,
    ff               = sqrt(density$fluxUMean.allClasses^2 + density$fluxVMean.allClasses^2),
    dd               = (atan2(density$fluxUMean.allClasses, density$fluxVMean.allClasses) * 180 / pi) %% 360,
    sd_vvp           = NA_real_,
    gap              = FALSE,
    eta              = NA_real_,
    dens             = density$density,
    dbz              = NA_real_,
    dbz_all          = NA_real_,
    n                = density$nEchoesFluxU.allClasses,
    n_dbz            = density$nEchoes,
    n_all            = NA_integer_,
    n_dbz_all        = NA_integer_,
    rcs              = density$weightedRCSMean.allClasses,
    sd_vvp_threshold = NA_real_,
    vcp              = NA_integer_,
    radar_latitude   = siteData$latitude,
    radar_longitude  = siteData$longitude,
    radar_height     = siteData$altitude,
    radar_wavelength = 3.19,
    source_file      = NA_character_
  )

  # Create the output directory
  # =============================================================================
  outputDirVPTS = file.path(outputDir, "vpts")
  dir.create(outputDirVPTS, showWarnings = F, recursive = T)
  message(paste0("Writing daily VPTS csv files to ", outputDirVPTS), "..")

  # Save the vpts to files, one file per day
  # =============================================================================
  allDays = unique(as.Date(vpts$datetime, format = "%Y-%m-%dT%H:%M:%SZ"))
  for (i in 1:length(allDays)) {
    cDay = allDays[i]
    vptsDay = vpts[(vpts$datetime >= cDay) & (vpts$datetime < cDay + 1), ]
    readr::write_csv(
      x = vptsDay,
      file = file.path(
        outputDirVPTS,
        paste0(
          siteData$radarID, "_vpts_",
          format(cDay, format = "%Y%m%d"),
          ".csv"
        )
      ),
      na = ""
    )
  }

  # Return directory where vpts were stored
  # =============================================================================
  return(outputDirVPTS)
}
