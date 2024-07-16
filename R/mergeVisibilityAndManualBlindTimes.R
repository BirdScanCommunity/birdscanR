#### mergeVisibilityAndManualBlindTimes ----------------------------------------
#' @title mergeVisibilityAndManualBlindTimes
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com};
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @description Function to merge manual blind times with blind times from
#' visibility table. For further processing the radar (visibility) and manual
#' blind times have to be merged with the function
#' ‘mergeVisibilityAndManualBlindTimes’. This function will add a blind time
#' type to the radar/visibility blind times. Blind times during the block time
#' (usually 60s) at the beginning of each protocol are given the type
#' 'protocolChange', the rest of the radar blind times are given the type
#' “visibility”. After that the visibility and manual blind times will be
#' merged. In case manual blind times and radar blind times are overlapping,
#' radar blind times with type “visibility” will be overwritten, but not radar
#' blind times with type “protocolChange”.
#' @param visibilityData dataframe with the visibility data from the data list
#' created by the function ‘extractDBData’.
#' @param manualBlindTimes dataframe with the manual blind times created by the
#' function ‘loadManualBlindTimes’.
#' @param protocolData dataframe with the protocol data from the data list
#' created by the function ‘extractDBData’ or a subset of it created by the
#' function ‘filterProtocolData’.
#'
#' @return dataframe with overall blind times
#' @export
#'
#' @examples
#' \dontrun{
#' # Set server and database settings
#' # ===========================================================================
#' dbServer <- "MACHINE\\SERVERNAME" # Set the name of your SQL server
#' dbName <- "db_Name" # Set the name of your database
#' dbDriverChar <- "SQL Server" # Set either "SQL Server" or "PostgreSQL"
#'
#' # Open the connection with the database
#' # ===========================================================================
#' dsn <- paste0(
#'   "driver=", dbDriverChar, ";server=", dbServer,
#'   ";database=", dbName,
#'   ";uid=", rstudioapi::askForPassword("Database user"),
#'   ";pwd=", rstudioapi::askForPassword("Database password")
#' )
#' dbConnection <- RODBC::odbcDriverConnect(dsn)
#'
#' # Get visibility table
#' # ===========================================================================
#' visibilityTable <- getVisibilityTable(dbConnection, dbDriverChar)
#'
#' # Get manual blind times
#' # ===========================================================================
#' data(manualBlindTimes)
#' cManualBlindTimes <- manualBlindTimes
#'
#' # Merge manual and automatic blind times
#' # ===========================================================================
#' blindTimes <- mergeVisibilityAndManualBlindTimes(
#'   visibilityData = visibilityTable,
#'   manualBlindTimes = cManualBlindTimes,
#'   protocolData = protocolData
#' )
#' }
#'
mergeVisibilityAndManualBlindTimes <- function(visibilityData,
                                               manualBlindTimes = NULL,
                                               protocolData) {
  # Check whether the necessary input data was provided
  # =============================================================================
  if (is.null(visibilityData)) {
    stop("There is no visibilityData provided. Check your input!")
  }
  if (is.null(protocolData)) {
    stop("There is no protocolData provided. Check your input!")
  }

  # Subset visibilityData to columns of interest
  # =============================================================================
  colsOfInterest <- c(
    "visibilityLogID", "protocolID",
    "blind_from_targetTZ", "blind_to_targetTZ"
  )
  visibilityData <- visibilityData[, names(visibilityData) %in% colsOfInterest]

  # Sort visibilityData chronological
  # =============================================================================
  visibilityDataSorted <- visibilityData[order(visibilityData$blind_from_targetTZ), ]

  # Remove rows where start >= stop in visibility blind times
  # =============================================================================
  visibilityDataSorted <- visibilityDataSorted[visibilityDataSorted$blind_from_targetTZ < visibilityDataSorted$blind_to_targetTZ, ]

  # Make sure visibility blind times are not overlapping
  # =============================================================================
  if (nrow(visibilityDataSorted) > 1) {
    overlaps <- visibilityDataSorted$blind_from_targetTZ[2:(length(visibilityDataSorted[, 1]))] < visibilityDataSorted$blind_to_targetTZ[1:(length(visibilityDataSorted[, 1]) - 1)]
    visibilityDataSorted$blind_to_targetTZ[c(overlaps, FALSE)] <- visibilityDataSorted$blind_from_targetTZ[c(FALSE, overlaps)]
  }

  # Add column 'type' to visibilityData
  # =============================================================================
  visibilityDataSorted <- data.frame(visibilityDataSorted, type = "visibility")
  levels(visibilityDataSorted$type) <- c("visibility", "protocolChange")

  # If manualBlindTimes are provided, check and prepare manual blind times for merging
  # =============================================================================
  if (!is.null(manualBlindTimes)) {
    # Sort manual blind times chronological
    # =========================================================================
    manualBlindTimesSorted <- manualBlindTimes[order(manualBlindTimes$start_targetTZ), ]

    # Remove rows where start >= stop in manual blind times
    # =========================================================================
    manualBlindTimesSorted <- manualBlindTimesSorted[manualBlindTimesSorted$start_targetTZ < manualBlindTimesSorted$stop_targetTZ, ]

    # Make sure manual blind times are not overlapping
    # =========================================================================
    if (nrow(manualBlindTimesSorted) > 1) {
      overlaps <- manualBlindTimesSorted$start_targetTZ[2:(length(manualBlindTimesSorted[, 1]))] < manualBlindTimesSorted$stop_targetTZ[1:(length(manualBlindTimesSorted[, 1]) - 1)]
      manualBlindTimesSorted$stop_targetTZ[c(overlaps, FALSE)] <- manualBlindTimesSorted$start_targetTZ[c(FALSE, overlaps)]
    }
  }

  # Separate protocol change blind times (60s at begin of each protocol) in
  # visibilitydata
  # =============================================================================
  protocolId <- -1
  nVis <- length(visibilityDataSorted[, 1])
  for (i in 1:nVis) {
    # Find first occurance of each protocolId in visibilityData
    # =========================================================================
    if (protocolId != visibilityDataSorted$protocolID[i]) {
      # Get protocolID of current visibility item
      # =====================================================================
      protocolId <- visibilityDataSorted$protocolID[i]

      # BlockTime of protocol
      # =====================================================================
      if (protocolId %in% protocolData$protocolID) {
        blockTime <- protocolData$blockTime[protocolData$protocolID == protocolId]
      } else {
        blockTime <- 60
      }

      # If blindTime is longer than 60s, split it after 60s
      # =====================================================================
      if (difftime(visibilityDataSorted$blind_to_targetTZ[i],
        visibilityDataSorted$blind_from_targetTZ[i],
        units = "secs"
      ) > blockTime) {
        split <- visibilityDataSorted[i, ]
        visibilityDataSorted$blind_to_targetTZ[i] <- visibilityDataSorted$blind_from_targetTZ[i] +
          protocolData$blockTime[protocolData$protocolID == protocolId]
        split$blind_from_targetTZ <- visibilityDataSorted$blind_to_targetTZ[i]
        visibilityDataSorted <- rbind(visibilityDataSorted, split)
      }

      # Mark first visibilityBlindTime of each protocol with "protocolChange"
      # =====================================================================
      visibilityDataSorted$type[i] <- "protocolChange"
    }
  }

  # Sort visibilityData chronological
  # =============================================================================
  visibilityDataSorted <- visibilityDataSorted[order(visibilityDataSorted$blind_from_targetTZ), ]

  # If manualBlindTimes are provided, add the manual blind times to the overall
  # blind times
  # =============================================================================
  if (!is.null(manualBlindTimes)) {
    # Extract all blind times of "protocolChange" type
    # =========================================================================
    protChangeBT <- visibilityDataSorted[visibilityDataSorted$type == "protocolChange", ]

    # --- Priorise protocolChange blind times over manual blind times --------- =
    # Loop over manual blind times, and adjust start or stop of blind time in
    # case of overlap with protocolChange blind time
    # =========================================================================
    for (i in 1:length(manualBlindTimesSorted[, 1])) {
      # if manual blindtime ends inside protocolChange blindtime, set end of
      # manual blindtime to start of protocolChange blindtime
      # =====================================================================
      protChangeStart <- protChangeBT$blind_from_targetTZ[(protChangeBT$blind_from_targetTZ < manualBlindTimesSorted$stop_targetTZ[i]) &
        (protChangeBT$blind_to_targetTZ >= manualBlindTimesSorted$stop_targetTZ[i])]
      if (length(as.vector(protChangeStart)) == 1) {
        manualBlindTimesSorted$stop_targetTZ[i] <- protChangeStart
      } else if (length(as.vector(protChangeStart)) > 1) {
        warning("overlapping visibilityData, should not happen.")
      }

      # if manual blindtime starts inside protocolChange blindtime, set start
      # of manual blindtime to end of protocolChange blindtime
      # =====================================================================
      protChangeEnd <- protChangeBT$blind_to_targetTZ[protChangeBT$blind_from_targetTZ <= manualBlindTimesSorted$start_targetTZ[i] &
        protChangeBT$blind_to_targetTZ > manualBlindTimesSorted$start_targetTZ[i]]
      if (length(as.vector(protChangeEnd)) == 1) {
        manualBlindTimesSorted$start_targetTZ[i] <- protChangeEnd
      } else if (length(as.vector(protChangeEnd)) > 1) {
        warning("overlapping visibilityData, should not happen.")
      }
    }

    # Remove rows where start >= stop in manual blind times
    # =========================================================================
    manualBlindTimesSorted <- manualBlindTimesSorted[manualBlindTimesSorted$start_targetTZ < manualBlindTimesSorted$stop_targetTZ, ]

    # Split manual blind times if protocolChange blindtime is inside manual
    # blind time. Loop over protocolChange blind times
    # =========================================================================
    for (i in 1:length(protChangeBT[, 1])) {
      manBTWithProtChangeBTInside <- (manualBlindTimesSorted$start_targetTZ < protChangeBT$blind_from_targetTZ[i]) &
        (manualBlindTimesSorted$stop_targetTZ > protChangeBT$blind_to_targetTZ[i])

      # if protocolChange blindtime is within manual blind time, split manual
      # blind time
      # =====================================================================
      if (sum(manBTWithProtChangeBTInside) == 1) {
        split <- manualBlindTimesSorted[manBTWithProtChangeBTInside, ]
        split$stop_targetTZ <- protChangeBT$blind_from_targetTZ[i]
        manualBlindTimesSorted$start_targetTZ[manBTWithProtChangeBTInside] <- protChangeBT$blind_to_targetTZ[i]
        manualBlindTimesSorted <- rbind(manualBlindTimesSorted, split)
      } else if (sum(manBTWithProtChangeBTInside) > 1) {
        warning("overlapping manual blind times, should not happen.")
      }
    }

    # Remove rows where start >= stop in manual blind times
    # =========================================================================
    manualBlindTimesSorted <- manualBlindTimesSorted[manualBlindTimesSorted$start_targetTZ < manualBlindTimesSorted$stop_targetTZ, ]

    # Sort manual blind times chronological
    # =========================================================================
    manualBlindTimesSorted <- manualBlindTimesSorted[order(manualBlindTimesSorted$start_targetTZ), ]

    # -- Priorise manual blind times over visibility blind times --------------
    # Loop over visibility blind times
    # =========================================================================
    for (i in 1:length(visibilityDataSorted[, 1])) {
      if (visibilityDataSorted$type[i] != "protocolChange") {
        # if visibility blindtime ends inside manual blindtime, set end of
        # visibility blindtime to start of manual blindtime
        # ===================================================================
        manualBTstart <- manualBlindTimesSorted$start_targetTZ[(manualBlindTimesSorted$start_targetTZ < visibilityDataSorted$blind_to_targetTZ[i]) &
          (manualBlindTimesSorted$stop_targetTZ >= visibilityDataSorted$blind_to_targetTZ[i])]
        if (length(as.vector(manualBTstart)) == 1) {
          visibilityDataSorted$blind_to_targetTZ[i] <- manualBTstart
        } else if (length(as.vector(manualBTstart)) > 1) {
          warning("overlapping manual blindTimes, should not happen.")
        }

        # if visibility blindtime starts inside manual blindtime, set start
        # of visibility blindtime to end of manual blindtime
        # ===================================================================
        manualBTend <- manualBlindTimesSorted$stop_targetTZ[(manualBlindTimesSorted$start_targetTZ <= visibilityDataSorted$blind_from_targetTZ[i]) &
          (manualBlindTimesSorted$stop_targetTZ > visibilityDataSorted$blind_from_targetTZ[i])]
        if (length(as.vector(manualBTend)) == 1) {
          visibilityDataSorted$blind_from_targetTZ[i] <- manualBTend
        } else if (length(as.vector(manualBTend)) > 1) {
          warning("overlapping manual blindTimes, should not happen.")
        }
      }
    }

    # Remove rows where start >= stop in visibility blind times
    # =========================================================================
    visibilityDataSorted <- visibilityDataSorted[visibilityDataSorted$blind_from_targetTZ < visibilityDataSorted$blind_to_targetTZ, ]

    # Split visibility blind times if manual blindtime is inside visibility
    # blind time. Loop over manual blind times
    # =========================================================================
    for (i in 1:length(manualBlindTimesSorted[, 1])) {
      visBTWithManualBTInside <- (visibilityDataSorted$blind_from_targetTZ < manualBlindTimesSorted$start_targetTZ[i]) &
        (visibilityDataSorted$blind_to_targetTZ > manualBlindTimesSorted$stop_targetTZ[i])

      # If protocolChange blindtime is within manual blind time, split manual
      # blindtime
      # =====================================================================
      if (sum(visBTWithManualBTInside) == 1) {
        split <- visibilityDataSorted[visBTWithManualBTInside, ]
        split$blind_to_targetTZ <- manualBlindTimesSorted$start_targetTZ[i]
        visibilityDataSorted$blind_from_targetTZ[visBTWithManualBTInside] <- manualBlindTimesSorted$stop_targetTZ[i]
        visibilityDataSorted <- rbind(visibilityDataSorted, split)
      } else if (sum(visBTWithManualBTInside) > 1) {
        warning("overlapping visibility blind times, should not happen.")
      }
    }
  }

  # Remove rows where start >= stop in visibility blind times
  # =============================================================================
  visibilityDataSorted <- visibilityDataSorted[visibilityDataSorted$blind_from_targetTZ < visibilityDataSorted$blind_to_targetTZ, ]

  # Sort visibility blind times chronological
  # =============================================================================
  visibilityDataSorted <- visibilityDataSorted[order(visibilityDataSorted$blind_from_targetTZ), ]

  # Check for problems with visibility values
  # =============================================================================
  if (any(visibilityDataSorted$blind_to_targetTZ < visibilityDataSorted$blind_from_targetTZ)) {
    warning("negative visibility blind times, something went wrong...")
  }
  if (any(visibilityDataSorted$blind_from_targetTZ[2:length(visibilityDataSorted[, 1])] <
    visibilityDataSorted$blind_to_targetTZ[1:(length(visibilityDataSorted[, 1]) - 1)])) {
    warning("overlapping visibility blind times, something went wrong...")
  }

  # If manualBlindTimes are provided, merge them with visibility blind times
  # =============================================================================
  if (!is.null(manualBlindTimes)) {
    overallBlindTimes <- rbind(
      manualBlindTimesSorted[, names(manualBlindTimesSorted) %in% c("start_targetTZ", "stop_targetTZ", "type")],
      data.frame(
        start_targetTZ = visibilityDataSorted$blind_from_targetTZ,
        stop_targetTZ = visibilityDataSorted$blind_to_targetTZ,
        type = visibilityDataSorted$type
      )
    )

    # If NO manual blind times, set the output to the visibility data blind times
    # =============================================================================
  } else {
    overallBlindTimes <- data.frame(
      start_targetTZ = visibilityDataSorted$blind_from_targetTZ,
      stop_targetTZ = visibilityDataSorted$blind_to_targetTZ,
      type = visibilityDataSorted$type
    )
  }

  # sort overall blind times chronological
  # =============================================================================
  overallBlindTimes <- overallBlindTimes[order(overallBlindTimes$start_targetTZ), ]

  # Return merged blind times
  # =============================================================================
  return(overallBlindTimes)
}
