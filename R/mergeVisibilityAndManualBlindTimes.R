#### mergeVisibilityAndManualBlindTimes ------------------------------------------------------
#' @title mergeVisibilityAndManualBlindTimes
#' @author Fabian Hertner (SBRS), \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest (SOI), \email{birgen.haest@@vogelwarte.ch}
#' @description Function to merge manual blindtimes with blindtimes from visibility table. 
#' For further processing the radar (visibility) and manual blind times have to be merged 
#' with the function ‘mergeVisibilityAndManualBlindTimes’. This function will add a blind 
#' time type to the radar/visibility blind times. Blind times during the blocktime (usually 60s) 
#' at the beginning of each protocol are given the type “protocolChange”, the rest of the radar 
#' blind times are given the type “visibility”. After that the visibility and manual blind times 
#' will be merged. In case manual blind times and radar blind times are overlapping, radar blind 
#' times with type “visibility” will be overwritten, but not radar blind times with type “protocolChange”. 
#' @param visibilityData dataframe with the visibility data from the data list created by the function ‘extractDBData’.
#' @param manualBlindTimes dataframe with the manual blind times created by the function ‘loadManualBlindTimes’.
#' @param protocolData dataframe with the protocol data from the data list created by the function ‘extractDBData’ or a subset of it created by the function ‘filterProtocolData’.
#'
#' @return dataframe with overall blind times
#' @export
#'
# #' @examples
# #' mergeVisibilityAndManualBlindTimes(visibilityData = data$visibilityData, manualBlindTimes = manualBlindTimes, protocolData = protocolDataSubset)
mergeVisibilityAndManualBlindTimes <- function(visibilityData = NULL, manualBlindTimes = NULL, protocolData = NULL)
{
  if (!is.null(visibilityData) && !is.null(protocolData))
  {
    # remove unsused columns in visibilityData
    visibilityData <- visibilityData[, names(visibilityData) %in% c("visibilityLogID", "protocolID", "blind_from_targetTZ", "blind_to_targetTZ")]
    
    # sort visibilityData chronological
    visibilityDataSorted <- visibilityData[order(visibilityData$blind_from_targetTZ),]
    
    # remove rows where start >= stop in visibility blindtimes
    visibilityDataSorted <- visibilityDataSorted[visibilityDataSorted$blind_from_targetTZ < visibilityDataSorted$blind_to_targetTZ,]
    
    # make sure visibility blindtimes are not overlapping
    overlaps <- visibilityDataSorted$blind_from_targetTZ[2 : length(visibilityDataSorted[, 1])] < visibilityDataSorted$blind_to_targetTZ[1 : length(visibilityDataSorted[, 1]) - 1]
    visibilityDataSorted$blind_to_targetTZ[c(overlaps, FALSE)] <- visibilityDataSorted$blind_from_targetTZ[c(FALSE, overlaps)]
    
    # add column 'type' to visibilityData
    visibilityDataSorted <- data.frame(visibilityDataSorted, type = "visibility")
    levels(visibilityDataSorted$type) <- c("visibility", "protocolChange")
    
    if (!is.null(manualBlindTimes))
    {
      # sort manual blind times chronological
      manualBlindTimesSorted <- manualBlindTimes[order(manualBlindTimes$start_targetTZ),]
      
      # remove rows where start >= stop in manual blindtimes
      manualBlindTimesSorted <- manualBlindTimesSorted[manualBlindTimesSorted$start_targetTZ < manualBlindTimesSorted$stop_targetTZ,]
      
      # make sure manual blindtimes are not overlapping
      overlaps <- manualBlindTimesSorted$start_targetTZ[2 : length(manualBlindTimesSorted[, 1])] < manualBlindTimesSorted$stop_targetTZ[1 : length(manualBlindTimesSorted[, 1]) - 1]
      manualBlindTimesSorted$stop_targetTZ[c(overlaps, FALSE)] <- manualBlindTimesSorted$start_targetTZ[c(FALSE, overlaps)]
    }
    
    # separate protocol change blindtimes (60s at begin of each protocol) in visibilitydata
    protocolId <- -1
    nVis <- length(visibilityDataSorted[, 1])
    for (i in 1 : nVis)
    {
      # find first occurance of each protocolId in visibilityData
      if (protocolId != visibilityDataSorted$protocolID[i])
      {
        protocolId = visibilityDataSorted$protocolID[i]
        
        # blockTime of protocol
        if (protocolId %in% protocolData$protocolID)
        {
          blockTime <- protocolData$blockTime[protocolData$protocolID == protocolId]
        } else
        {
          blockTime <- 60
        }
        
        # if blindTime is longer than 60s, split it after 60s
        if (difftime(visibilityDataSorted$blind_to_targetTZ[i] , visibilityDataSorted$blind_from_targetTZ[i], units = "secs") > blockTime)
        {
          split <- visibilityDataSorted[i,]
          visibilityDataSorted$blind_to_targetTZ[i] <- visibilityDataSorted$blind_from_targetTZ[i] + protocolData$blockTime[protocolData$protocolID == protocolId]
          split$blind_from_targetTZ <- visibilityDataSorted$blind_to_targetTZ[i]
          visibilityDataSorted <- rbind(visibilityDataSorted, split)
        }
        
        # mark first visibilityBlindTime of each protocol with "protocolChange"
        visibilityDataSorted$type[i] <- "protocolChange"
      }
    }
    
    # sort visibilityData chronological
    visibilityDataSorted <- visibilityDataSorted[order(visibilityDataSorted$blind_from_targetTZ),]
    
    if (!is.null(manualBlindTimes))
    {
      # --------------------------- priorise protocolChange blindtimes over manual blindtimes ------------------
      # loop over manual blindtimes
      protChangeBT <- visibilityDataSorted[visibilityDataSorted$type == "protocolChange",]
      for (i in 1 : length(manualBlindTimesSorted[, 1]))
      {
        # if manual blindtime ends inside protocolChange blindtime, set end of manual blindtime to start of protocolChange blindtime
        protChangeStart <- protChangeBT$blind_from_targetTZ[protChangeBT$blind_from_targetTZ < manualBlindTimesSorted$stop_targetTZ[i]
                                                             & protChangeBT$blind_to_targetTZ >= manualBlindTimesSorted$stop_targetTZ[i]]
        if (length(as.vector(protChangeStart)) == 1)
        {
          manualBlindTimesSorted$stop_targetTZ[i] <- protChangeStart
        } else if (length(as.vector(protChangeStart)) > 1)
        {
          warning("overlapping visibilityData, should not happen.")
        }
        
        # if manual blindtime starts inside protocolChange blindtime, set start of manual blindtime to end of protocolChange blindtime
        protChangeEnd <- protChangeBT$blind_to_targetTZ[protChangeBT$blind_from_targetTZ <= manualBlindTimesSorted$start_targetTZ[i]
                                                             & protChangeBT$blind_to_targetTZ > manualBlindTimesSorted$start_targetTZ[i]]
        if (length(as.vector(protChangeEnd)) == 1)
        {
          manualBlindTimesSorted$start_targetTZ[i] <- protChangeEnd
        } else if (length(as.vector(protChangeEnd)) > 1)
        {
          warning("overlapping visibilityData, should not happen.")
        }
      }
      
      # remove rows where start >= stop in manual blindtimes
      manualBlindTimesSorted <- manualBlindTimesSorted[manualBlindTimesSorted$start_targetTZ < manualBlindTimesSorted$stop_targetTZ,]
      
      # split manual blindtimes if protocolChange blindtime is inside manual blind time
      # loop over protocolChange blindtimes
      for (i in 1 : length(protChangeBT[, 1]))
      {
        manBTWithProtChangeBTInside <- manualBlindTimesSorted$start_targetTZ < protChangeBT$blind_from_targetTZ[i] & manualBlindTimesSorted$stop_targetTZ > protChangeBT$blind_to_targetTZ[i]
        
        # if protocolChange blindtime is within manual blind time, split manual blindtime
        if (sum(manBTWithProtChangeBTInside) == 1)
        {
          split <- manualBlindTimesSorted[manBTWithProtChangeBTInside,]
          split$stop_targetTZ <- protChangeBT$blind_from_targetTZ[i]
          manualBlindTimesSorted$start_targetTZ[manBTWithProtChangeBTInside] <- protChangeBT$blind_to_targetTZ[i]
          manualBlindTimesSorted <- rbind(manualBlindTimesSorted, split)
        }else if (sum(manBTWithProtChangeBTInside) > 1)
        {
          warning("overlapping manual blindtimes, should not happen.")
        }
      }
      
      # remove rows where start >= stop in manual blindtimes
      manualBlindTimesSorted <- manualBlindTimesSorted[manualBlindTimesSorted$start_targetTZ < manualBlindTimesSorted$stop_targetTZ,]
     
      # sort manual blind times chronological
      manualBlindTimesSorted <- manualBlindTimesSorted[order(manualBlindTimesSorted$start_targetTZ),]
    
      # --------------------------- priorise manual blindtimes over visibility blindtimes ------------------
      # loop over visibility blindtimes
      for (i in 1 : length(visibilityDataSorted[, 1]))
      {
        if (visibilityDataSorted$type[i] != "protocolChange")
        {
          # if visibility blindtime ends inside manual blindtime, set end of visibility blindtime to start of manual blindtime
          manualBTstart <- manualBlindTimesSorted$start_targetTZ[manualBlindTimesSorted$start_targetTZ < visibilityDataSorted$blind_to_targetTZ[i]
                                                         & manualBlindTimesSorted$stop_targetTZ >= visibilityDataSorted$blind_to_targetTZ[i]]
          if (length(as.vector(manualBTstart)) == 1)
          {
            visibilityDataSorted$blind_to_targetTZ[i] <- manualBTstart
          } else if (length(as.vector(manualBTstart)) > 1)
          {
            warning("overlapping manual blindTimes, should not happen.")
          }
          
          # if visibility blindtime starts inside manual blindtime, set start of visibility blindtime to end of manual blindtime
          manualBTend <- manualBlindTimesSorted$stop_targetTZ[manualBlindTimesSorted$start_targetTZ <= visibilityDataSorted$blind_from_targetTZ[i]
                                                           & manualBlindTimesSorted$stop_targetTZ > visibilityDataSorted$blind_from_targetTZ[i]]
          if (length(as.vector(manualBTend)) == 1)
          {
            visibilityDataSorted$blind_from_targetTZ[i] <- manualBTend
          } else if (length(as.vector(manualBTend)) > 1)
          {
            warning("overlapping manual blindTimes, should not happen.")
          } 
        }
      }
      
      # remove rows where start >= stop in visibility blindtimes
      visibilityDataSorted <- visibilityDataSorted[visibilityDataSorted$blind_from_targetTZ < visibilityDataSorted$blind_to_targetTZ,]
      
      # split visibility blindtimes if manual blindtime is inside visibility blind time
      # loop over manual blindtimes
      for (i in 1 : length(manualBlindTimesSorted[, 1]))
      {
        visBTWithManualBTInside <- visibilityDataSorted$blind_from_targetTZ < manualBlindTimesSorted$start_targetTZ[i] & visibilityDataSorted$blind_to_targetTZ > manualBlindTimesSorted$stop_targetTZ[i]
        
        # if protocolChange blindtime is within manual blind time, split manual blindtime
        if (sum(visBTWithManualBTInside) == 1)
        {
          split <- visibilityDataSorted[visBTWithManualBTInside,]
          split$blind_to_targetTZ <- manualBlindTimesSorted$start_targetTZ[i]
          visibilityDataSorted$blind_from_targetTZ[visBTWithManualBTInside] <- manualBlindTimesSorted$stop_targetTZ[i]
          visibilityDataSorted <- rbind(visibilityDataSorted, split)
        }else if (sum(visBTWithManualBTInside) > 1)
        {
          warning("overlapping visibility blindtimes, should not happen.")
        }
      }
    }
    
    # remove rows where start >= stop in visibility blindtimes
    visibilityDataSorted <- visibilityDataSorted[visibilityDataSorted$blind_from_targetTZ < visibilityDataSorted$blind_to_targetTZ,]
    
    # sort visibility blind times chronological
    visibilityDataSorted <- visibilityDataSorted[order(visibilityDataSorted$blind_from_targetTZ),]
    
    if (sum(visibilityDataSorted$blind_to_targetTZ < visibilityDataSorted$blind_from_targetTZ) > 0)
    {
      warning("negative visibility blindtimes, something went wrong...")
    }
    if (sum(visibilityDataSorted$blind_from_targetTZ[2 : length(visibilityDataSorted[, 1])] < visibilityDataSorted$blind_to_targetTZ[1 : length(visibilityDataSorted[, 1]) - 1]) > 0)
    {
      warning("overlapping visibility blindtimes, something went wrong...")
    }
    
    if (!is.null(manualBlindTimes))
    {
      # merge visibility and manual blindtimes
      overallBlindTimes <- rbind(manualBlindTimesSorted[, names(manualBlindTimesSorted) %in% c("start_targetTZ", "stop_targetTZ", "type")], 
                                  data.frame(start_targetTZ = visibilityDataSorted$blind_from_targetTZ, 
                                              stop_targetTZ = visibilityDataSorted$blind_to_targetTZ, 
                                              type = visibilityDataSorted$type))
    } else
    {
      overallBlindTimes <- data.frame(start_targetTZ = visibilityDataSorted$blind_from_targetTZ, 
                                              stop_targetTZ = visibilityDataSorted$blind_to_targetTZ, 
                                              type = visibilityDataSorted$type)
    }
    
    # sort overall blindtimes chronological
    overallBlindTimes <- overallBlindTimes[order(overallBlindTimes$start_targetTZ),]
    
    return(overallBlindTimes)
  }
  
  return()
}