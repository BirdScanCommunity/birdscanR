#### computeObservationTime ---------------------------------------------------
#' @title computeObservationTime
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com};
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @description Compute blind times and observation times during time bins based
#'  on protocol data and blind times
#'
#' @param timeBins dataframe with the time bins created by the function
#' \code{createTimeBins}.
#' @param protocolData dataframe with the protocol data from the data list
#' created by the function \code{extractDBData} or a subset of it created by the
#' function \code{filterProtocolData}.
#' @param blindTimes dataframe containing the blind times created by the
#' function \code{mergeVisibilityAndManualBlindTimes}.
#' @param blindTimeAsMtrZero character string vector with the blind time types
#' which should be treated as observation time with MTR zero.
#'
#' @return returns a dataframe with the time bins completed with the observation
#' times of each time bin.
#'
computeObservationTime <- function(timeBins,
                                   protocolData,
                                   blindTimes,
                                   blindTimeAsMtrZero = NULL) {
  oldOptions <- options()
  on.exit(options(oldOptions))
  options(scipen = 999, digits = 9)

  # Sort protocolData and blindTimes chronological
  # =============================================================================
  protocolData <- protocolData[order(protocolData$startTime_targetTZ), ]
  blindTimes <- blindTimes[order(blindTimes$start_targetTZ), ]

  # Make sure protocol entries are not overlapping (should not happen)
  # =============================================================================
  overlaps <- protocolData$startTime_targetTZ[2:length(protocolData[, 1])] <
    protocolData$stopTime_targetTZ[1:(length(protocolData[, 1]) - 1)]
  protocolData$stopTime_targetTZ[c(overlaps, FALSE)] <-
    protocolData$startTime_targetTZ[c(FALSE, overlaps)]

  # Add 'timeBinId' column to protocolData and blindTimes
  # =============================================================================
  protocolData <- data.frame(protocolData, timeBinId = NA)
  blindTimes <- data.frame(blindTimes, timeBinId = NA)

  # Add 'protocolID' column to blindTimes
  # =============================================================================
  blindTimes <- data.frame(blindTimes, protocolID = NA)

  # Split protocol entries if expanding over one or more timeBin starts
  # =============================================================================
  for (i in 1:length(timeBins[, 1])) {
    # Find protocol entry at start of timeBin
    # =========================================================================
    protAtTimeBinStart <- (protocolData$startTime_targetTZ < timeBins$start[i]) &
      (protocolData$stopTime_targetTZ > timeBins$start[i])

    # Split protocol entry
    # =========================================================================
    if (sum(protAtTimeBinStart == TRUE) == 1) {
      split <- protocolData[protAtTimeBinStart, ]
      split$stopTime_targetTZ <- timeBins$start[i]
      protocolData$startTime_targetTZ[protAtTimeBinStart] <- timeBins$start[i]
      protocolData <- rbind(protocolData, split)
    }

    # Find protocol entry at end of timeBin
    # =========================================================================
    protAtTimeBinEnd <- (protocolData$startTime_targetTZ < timeBins$stop[i]) &
      (protocolData$stopTime_targetTZ > timeBins$stop[i])

    # split protocol entry
    # =========================================================================
    if (sum(protAtTimeBinEnd == TRUE) == 1) {
      split <- protocolData[protAtTimeBinEnd, ]
      split$stopTime_targetTZ <- timeBins$stop[i]
      protocolData$startTime_targetTZ[protAtTimeBinEnd] <- timeBins$stop[i]
      protocolData <- rbind(protocolData, split)
    }

    # Assign protocol entries to timeBin
    # =========================================================================
    protocolData$timeBinId[(protocolData$startTime_targetTZ >= timeBins$start[i]) &
      (protocolData$startTime_targetTZ < timeBins$stop[i])] <- timeBins$id[i]

    # find blindTime at start of timeBin
    # =========================================================================
    blindTimeAtTimeBinStart <- (blindTimes$start_targetTZ < timeBins$start[i]) &
      (blindTimes$stop_targetTZ > timeBins$start[i])

    # split blindTime
    # =========================================================================
    if (sum(blindTimeAtTimeBinStart == TRUE) == 1) {
      split <- blindTimes[blindTimeAtTimeBinStart, ]
      split$stop_targetTZ <- timeBins$start[i]
      blindTimes$start_targetTZ[blindTimeAtTimeBinStart] <- timeBins$start[i]
      blindTimes <- rbind(blindTimes, split)
    }

    # find blindTime at end of timeBin
    # =========================================================================
    blindTimeAtTimeBinEnd <- (blindTimes$start_targetTZ < timeBins$stop[i]) &
      (blindTimes$stop_targetTZ > timeBins$stop[i])

    # split blindTime
    # =========================================================================
    if (sum(blindTimeAtTimeBinEnd == TRUE) == 1) {
      split <- blindTimes[blindTimeAtTimeBinEnd, ]
      split$stop_targetTZ <- timeBins$stop[i]
      blindTimes$start_targetTZ[blindTimeAtTimeBinEnd] <- timeBins$stop[i]
      blindTimes <- rbind(blindTimes, split)
    }

    # assign blindTime to timeBin
    # =========================================================================
    blindTimes$timeBinId[(blindTimes$start_targetTZ >= timeBins$start[i]) &
      (blindTimes$start_targetTZ < timeBins$stop[i])] <- timeBins$id[i]
  }

  # Sort protocolData and blindTimes chronological
  # =============================================================================
  protocolData <- protocolData[order(protocolData$startTime_targetTZ), ]
  blindTimes <- blindTimes[order(blindTimes$start_targetTZ), ]

  # Assign blindTimes to protocols
  # =============================================================================
  # split blindTimes if expanding over start/end of a protocol
  # =============================================================================
  for (i in 1:length(protocolData[, 1])) {
    # find blindTime at start of protocol
    # =========================================================================
    blindTimeAtProtocolStart <- (blindTimes$start_targetTZ < protocolData$startTime_targetTZ[i]) &
      (blindTimes$stop_targetTZ > protocolData$startTime_targetTZ[i])

    # split blindTime
    # =========================================================================
    if (sum(blindTimeAtProtocolStart == TRUE) == 1) {
      split <- blindTimes[blindTimeAtProtocolStart, ]
      split$stop_targetTZ <- protocolData$startTime_targetTZ[i]
      blindTimes$start_targetTZ[blindTimeAtProtocolStart] <- protocolData$startTime_targetTZ[i]
      blindTimes <- rbind(blindTimes, split)
    }

    # find blindTime at end of protocol
    # =========================================================================
    blindTimeAtProtocolEnd <- (blindTimes$start_targetTZ < protocolData$stopTime_targetTZ[i]) &
      (blindTimes$stop_targetTZ > protocolData$stopTime_targetTZ[i])

    # split blindTime
    # =========================================================================
    if (sum(blindTimeAtProtocolEnd == TRUE) == 1) {
      split <- blindTimes[blindTimeAtProtocolEnd, ]
      split$stop_targetTZ <- protocolData$stopTime_targetTZ[i]
      blindTimes$start_targetTZ[blindTimeAtProtocolEnd] <- protocolData$stopTime_targetTZ[i]
      blindTimes <- rbind(blindTimes, split)
    }

    # assign blindTimes to protocolId
    # =========================================================================
    blindTimes$protocolID[(blindTimes$start_targetTZ >= protocolData$startTime_targetTZ[i]) &
      (blindTimes$start_targetTZ < protocolData$stopTime_targetTZ[i])] <- protocolData$protocolID[i]
  }

  # sort blindTimes chronological
  # =============================================================================
  blindTimes <- blindTimes[order(blindTimes$start_targetTZ), ]

  #------------------- compute observation time -------------------#
  # =============================================================================
  # add columns: 'operationTime_sec', 'blindTime_sec', 'observationTime_h',
  #              'observationTime_sec', 'proportionalTimeObserved' to timeBins
  # ===========================================================================
  timeBins <- data.frame(timeBins,
    operationTime_sec = NA, blindTime_sec = NA,
    observationTime_h = NA, observationTime_sec = NA,
    proportionalTimeObserved = NA
  )

  # add 'duration_sec' column to blindTimes, protocolData
  # ===========================================================================
  blindTimes <- data.frame(blindTimes, duration_sec = NA)
  protocolData <- data.frame(protocolData, duration_sec = NA)

  # duration
  # ===========================================================================
  blindTimes$duration_sec <- as.numeric(difftime(blindTimes$stop_targetTZ,
    blindTimes$start_targetTZ,
    units = "secs"
  ))
  protocolData$duration_sec <- as.numeric(difftime(protocolData$stopTime_targetTZ,
    protocolData$startTime_targetTZ,
    units = "secs"
  ))

  protocolData <- protocolData[!is.na(protocolData$timeBinId), ]
  blindTimes <- blindTimes[!is.na(blindTimes$timeBinId) & !is.na(blindTimes$protocolID), ]

  # loop over time bins
  # ===========================================================================
  for (i in 1:length(timeBins[, 1])) {
    # protocol durations in time bins (operationtime)
    # =========================================================================
    timeBins$operationTime_sec[i] <- sum(protocolData[protocolData$timeBinId == timeBins[i, ]$id, ]$duration_sec)

    # blindTime durations in time bins during protocol (blindTime)
    # ===========================================================================
    if (is.null(blindTimeAsMtrZero)) {
      timeBins$blindTime_sec[i] <- sum(blindTimes[blindTimes$timeBinId == timeBins[i, ]$id, ]$duration_sec)
    } else {
      timeBins$blindTime_sec[i] <- sum(blindTimes[(blindTimes$timeBinId == timeBins[i, ]$id) &
        (!(blindTimes$type %in% blindTimeAsMtrZero)), ]$duration_sec)
    }
  }

  # observation time (operationtime - blindtime)
  # ===========================================================================
  timeBins$observationTime_sec <- timeBins$operationTime_sec - timeBins$blindTime_sec
  timeBins$observationTime_h <- timeBins$observationTime_sec / (60 * 60)
  timeBins$proportionalTimeObserved[timeBins$duration_sec > 0] <- timeBins$observationTime_sec[timeBins$duration_sec > 0] /
    timeBins$duration_sec[timeBins$duration_sec > 0]
  timeBins$proportionalTimeObserved[timeBins$duration_sec == 0] <- 0

  # Return time bins
  # =============================================================================
  return(timeBins)
}

# computeObservationTime(protocolData = protocolDataSubset, blindTimes = blindTimes, timeBins = timeBins_1h_DayNight, blindTimeAsMtrZero = c("rain", "tech"))
# computeObservationTime(protocolData = protocolDataSubset, blindTimes = blindTimes, timeBins = timeBins_1h_DayNight)
