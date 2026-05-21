#' @title createVPTS
#' @author Birgen Haest
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


  # Create vpts along ALOFT standard
  # =============================================================================
  vpts = data.frame(
    radar            = siteData$radarID,
    datetime         = format(density$timeChunkBegin, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    height           = density$altitudeChunkBegin,
    u                = NA_real_,
    v                = NA_real_,
    w                = NA_real_,
    ff               = NA_real_,
    dd               = NA_real_,
    sd_vvp           = NA_real_,
    gap              = FALSE,
    eta              = NA_real_,
    dens             = density$density,
    dbz              = NA_real_,
    dbz_all          = NA_real_,
    n                = NA_integer_,
    n_dbz            = density$nEchoes,
    n_all            = NA_integer_,
    n_dbz_all        = NA_integer_,
    rcs              = NA_real_,
    sd_vvp_threshold = NA_real_,
    vcp              = NA_integer_,
    radar_latitude   = siteData$latitude,
    radar_longitude  = siteData$longitude,
    radar_height     = siteData$altitude,
    radar_wavelength = 3.19,
    source_file      = NA_character_ # This value could later on be filled with the url to the Zenodo repository, if that information is available
  )

  # Get the values for each time altitude bin
  # =============================================================================
  message(paste0(
    "Computing number of animals (n_dbz,), ",
    "number of birds with speed and direction values (n)",
    "mean flux directon (dd, NOTE: not mean circular direction, ",
    "but  mean flux direction taking into account ",
    "individuals' flight speeds), ",
    "mean flux speeds (ff, NOTE: not mean ground speed of the ",
    "individual birds but speed flux size taking into account ",
    "individual's directions), ",
    "mean u and v components",
    " and mtr-weighted average rcs (rcs)",
    " of animal movements in each time-altitude bin.."
  ))
  for (cRow in 1:nrow(vpts)) {
    # Check whether there are any density values before getting any
    # values from the echo table
    # =========================================================================
    if ((vpts[cRow, "dens"] != 0) && !(is.na((vpts[cRow, "dens"])))) {
      # Get the id of all samples that fall withing the timeframe and
      #  and altitudinal bin
      # =====================================================================
      cDay = as.Date(vpts$datetime[cRow], format = "%Y-%m-%dT%H:%M:%SZ")
      cSampleRows = which((as.Date(echoes$time_stamp_targetTZ) == cDay) &
        (echoes$feature1.altitude_AGL >= vpts$height[cRow]) &
        (echoes$feature1.altitude_AGL < (vpts$height[cRow] + altitudeBinSize)))
      speedsToProcess = echoes[cSampleRows, "feature37.speed"]
      directionsToProcess = echoes[cSampleRows, "feature2.azimuth"]
      elementsToKeep = which((!is.na(speedsToProcess)) & !is.na(directionsToProcess))
      speedsToProcess = speedsToProcess[elementsToKeep]
      directionsToProcess = directionsToProcess[elementsToKeep]
      rcsToProcess = echoes[cSampleRows, "RCS2_RCS_max_lowpassed"]
      rcsToKeep = which(!is.na(rcsToProcess))
      rcsToProcess = rcsToProcess[rcsToKeep]
      mtrFactorsToProcess = echoes[cSampleRows, "mtr_factor_rf"]
      mtrFactorsToProcess = mtrFactorsToProcess[rcsToKeep]

      # Fill the number of birds with speed values
      # =====================================================================
      vpts$n[cRow] = length(speedsToProcess)

      # Get the mean value for u, v, dd, and ff if there are any samples
      #  for this time-altitude bin
      # =====================================================================
      if (length(speedsToProcess) >= 1) {
        # Convert speed and direction to u and v components
        # =================================================================
        u = speedsToProcess * sin(directionsToProcess * pi / 180)
        v = speedsToProcess * cos(directionsToProcess * pi / 180)

        # Get the mean value for the u and v components
        # =================================================================
        meanU = mean(u)
        meanV = mean(v)
        vpts[cRow, "u"] = meanU
        vpts[cRow, "v"] = meanV

        # Get the speed and direction for the mean u and v components
        # (NOTE: we use this and not the mean direction and speed of the birds
        # because we want the values to represent the direction and speed of the flux
        # and not the mean speed of the birds - which is behaviour-related
        # but not flux-related)
        # =================================================================
        vpts[cRow, "dd"] = (atan2(meanU, meanV) * 180 / pi) %% 360
        vpts[cRow, "ff"] = sqrt((meanU^2) + (meanV^2))
      }

      # Get the weighted mean rcs for this time-altitude bin
      # =====================================================================
      if (length(rcsToProcess) >= 1) {
        # Convert rcs to cm^2
        # =================================================================
        rcsToProcessInCM2 = (10^(2 * rcsToProcess)) * 10000

        # Get the weighted average rcs
        # =================================================================
        mtrFactorWeights = mtrFactorsToProcess / max(mtrFactorsToProcess)
        vpts[cRow, "rcs"] = stats::weighted.mean(
          x = rcsToProcessInCM2,
          w = mtrFactorWeights
        )
      }
    }
  }

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
