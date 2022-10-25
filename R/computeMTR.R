#### computeMTR ------------------------------------------------------
#' @title computeMTR
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; with edits by Baptiste Schmid, \email{baptiste.schmid@@vogelwarte.ch}, and Birgen Haest, \email{birgen.haest@@vogelwarte.ch}  
#' @description Make the original function 'computeMTR' faster. 
#' There are two options to compute MTR values. 
#' The options are selected with the parameter ‘computePerDayNight’. 
#' - compute MTR for each time bin: This option computes the MTR for each time bin defined in the time bin dataframe. The time bins that were split due to sunrise/sunset during the time bin will be combined to one bin.
#' - compute MTR per day/night: The time bins of each day and night will be combined and the mean MTR is computed for each day and night. Aside this, the spread (first and third Quartile) for each day and night is computed. The spread is dependent on the chosen time bin duration/amount of time bins. 
#' @param echoes dataframe with the echo data from the data list created by the function ‘extractDBData’ or a subset of it created by the function ‘filterEchoData’. 
#' @param classSelection character string vector with all classes which should be used to calculate the MTR. The MTR and number of Echoes will be calculated for each class as well as for all classes together. 
#' @param altitudeRange numeric vector of length 2 with the start and end of the altitude range in meter a.g.l. 
#' @param altitudeBinSize numeric, size of the altitude bins in meter. 
#' @param timeRange vector of length 2, with start and end of timerange as POSIXct
#' @param timeBinDuration_sec duration of timeBins in seconds (numeric). for values <= 0 a duration of 1 hour will be set
#' @param timeZone timezone in which the timebins should be created as string. e.g. "Etc/GMT0"
#' @param sunriseSunset dataframe with sunrise/sunset, civil dawn/dusk. computed with function 'twilight'
#' @param sunOrCivil sunrise/sunset or civil dawn/dusk used to split day and night. Supported values: "sun" or "civil", default: "civil"
#' @param protocolData dataframe with the protocol data from the data list created by the function \code{extractDBData} or a subset of it created by the function \code{filterProtocolData}.
#' @param blindTimes dataframe containing the blind times created by the function \code{mergeVisibilityAndManualBlindTimes}.
#' @param blindTimeAsMtrZero character string vector with the blind time types which should be treated as observation time with MTR zero.
#' @param propObsTimeCutoff numeric between 0 and 1. If the MTR is computed per day and night, time bins with a proportional observation time smaller than propObsTimeCutoff are ignored when combining the time bins. If the MTR is computed for each time bin, the parameter is ignored.
#' @param computePerDayNight logical, TRUE: MTR is computed per day and night FALSE: MTR is computed for each time bin
#' @param computeAltitudeDistribution logical, TRUE: compute the mean height and altitude distribution of MTR for the pre-defined quantiles 0.05, 0.25, 0.5, 0.75, 0.95
#'
#' @return mtr
#' @importFrom magrittr %>%
#' @export
#'
# #' @examples 
# #' computeMTR(echoes = echoDataSubset, classSelection = classSelection, altitudeBins = altitudeBins_25_1000_oneBin, timeBins = timeBins_1h, propObsTimeCutoff = propObsTimeCutoff, computePerDayNight = FALSE)
# #' computeMTR(echoes = echoDataSubset,  classSelection = classSelection, altitudeBins = altitudeBins_25_1000_oneBin, timeBins = timeBins_1h_DayNight, propObsTimeCutoff = propObsTimeCutoff, computePerDayNight = TRUE)
# #' computeMTR(echoes = echoDataSubset, classSelection = classSelection, altitudeBins = altitudeBins_25_1025_binSize50, timeBins = timeBins_1h_DayNight, propObsTimeCutoff = propObsTimeCutoff, computePerDayNight = TRUE)
# #' computeMTR(echoes = echoDataSubset, classSelection = classSelection, altitudeBins = altitudeBins_25_1000_oneBin, timeBins = timeBins_1h_DayNight, propObsTimeCutoff = propObsTimeCutoff, computePerDayNight = TRUE)
# =============================================================================
computeMTR = function(echoes, 
                      classSelection, 
                      altitudeRange,
                      altitudeBinSize,
                      timeRange, 
                      timeBinDuration_sec,
                      timeZone,
                      sunriseSunset,
                      sunOrCivil                  = "civil",
                      protocolData, 
                      blindTimes, 
                      blindTimeAsMtrZero          = NULL,
                      propObsTimeCutoff           = 0, 
                      computePerDayNight          = FALSE, 
                      computeAltitudeDistribution = TRUE){
# Create altitudeBins
# =============================================================================
  message("Creating altitude bins..")
  sequence     = seq(altitudeRange[1], altitudeRange[2], altitudeBinSize)
  altitudeBins = data.frame(id          = seq(1, (length(sequence)-1), by = 1), 
                            begin       = sequence[1:(length(sequence)-1)], 
                            end         = sequence[2:length(sequence)], 
                            size        = NA_real_, 
                            avgAltitude = NA_real_)
  altitudeBins$size        = altitudeBins$end - altitudeBins$begin
  altitudeBins$avgAltitude = ((altitudeBins$begin + altitudeBins$end)) / 2
  
# Create Timebins
# =============================================================================
  message("Creating time bins..")
  timeBins = createTimeBins(timeRange           = timeRange, 
                            timeBinDuration_sec = timeBinDuration_sec, 
                            timeZone            = timeZone, 
                            sunriseSunset       = sunriseSunset, 
                            sunOrCivil          = sunOrCivil)
  
# Compute observation time for each timebin
# =============================================================================
  message("Computing observation times for each timebin.." )
  timeBins = computeObservationTime(timeBins           = timeBins, 
                                    protocolData       = protocolData, 
                                    blindTimes         = blindTimes, 
                                    blindTimeAsMtrZero = blindTimeAsMtrZero)
  
# Remove echoes with NA in 'mtr_factor'
# =============================================================================
  if (any(is.na(echoes$mtr_factor_rf))){
    n = length(is.na(echoes$mtr_factor_rf))
    echoes = echoes[!is.na(echoes$mtr_factor_rf),]
    message(paste0("Missing MTR-factors for ",  n, " echoes, thus excldued from the MTR calculation."))
  }
  
# Remove echoes outside the heigth range
# =============================================================================
  if (any(echoes$feature1.altitude_AGL > max(altitudeBins$end))){
    index = which(echoes$feature1.altitude_AGL > max(altitudeBins$end))
    n = length(index)
    echoes = echoes[-index ,]
    message(paste0(n, " echoes above the defined altitude range, thus excldued from the MTR calculation."))
  }
  
# abort if no echoes present
# =============================================================================
  if (length(echoes[,]) == 0){
    warning("no echoes to compute MTR (function: computeMTR)")
    return()
  }
  
# combine timebins splitted by day/night
# =============================================================================
  if (computePerDayNight == FALSE){
    for (i in 2 : length(timeBins[, 1])){
      if (timeBins$id[i] == -1){
        timeBins$stop[i-1] = timeBins$stop[i]
        
        if (timeBins$duration_sec[i] >= timeBins$duration_sec[i-1]){
          timeBins$dayOrNight[i-1] = timeBins$dayOrNight[i]
          timeBins$dateSunset[i-1] = timeBins$dateSunset[i]
        }
        timeBins$duration_sec[i-1]        = timeBins$duration_sec[i-1] + timeBins$duration_sec[i]
        timeBins$operationTime_sec[i-1]   = timeBins$operationTime_sec[i-1] + timeBins$operationTime_sec[i]
        timeBins$blindTime_sec[i-1]       = timeBins$blindTime_sec[i-1] + timeBins$blindTime_sec[i]
        timeBins$observationTime_h[i-1]   = timeBins$observationTime_h[i-1] + timeBins$observationTime_h[i]
        timeBins$observationTime_sec[i-1] = timeBins$observationTime_sec[i-1] + timeBins$observationTime_sec[i]
        timeBins$proportionalTimeObserved[i-1] = ifelse(timeBins$duration_sec[i-1] > 0, 
                                                        (timeBins$observationTime_sec[i-1] / timeBins$duration_sec[i-1]), 
                                                        0)
      } else if ((timeBins$id[i-1] != -1) && 
                 difftime(timeBins$stop[i], timeBins$start[i]) != difftime(timeBins$stop[i - 1], timeBins$start[i - 1]) && 
                 (i != length(timeBins[, 1]))){
        timeBins$id[i + 1] = -1
      }
    }
    
    # exclude combined timebins and reset timebins id
    # =========================================================================
      timeBins = timeBins[timeBins$id >= 0,]
      timeBins = timeBins[order(timeBins$start),]
      timeBins$id = seq(1, length(timeBins[, 1]))
  }
  
# set timeChunk and altitudeChunk
# =============================================================================
  timeAndAltitudeCombinations = expand.grid(timeChunkId      = timeBins$id , 
                                            altitudeChunkId  = altitudeBins$id, 
                                            KEEP.OUT.ATTRS   = FALSE, 
                                            stringsAsFactors = FALSE)
  
# add features to time and altitude chunk IDs
# =============================================================================
  mtr = merge(timeAndAltitudeCombinations, 
              data.frame(timeChunkId              = timeBins$id, 
                         timeChunkDate            = timeBins$date, 
                         timeChunkBegin           = timeBins$start, 
                         timeChunkEnd             = timeBins$stop,
                         timeChunkDateSunset      = timeBins$dateSunset,
                         timeChunkDuration_sec    = timeBins$duration_sec,
                         observationTime_sec      = timeBins$observationTime_sec,
                         observationTime_h        = timeBins$observationTime_h,
                         operationTime_sec        = timeBins$operationTime_sec,
                         blindTime_sec            = timeBins$blindTime_sec,
                         proportionalTimeObserved = timeBins$proportionalTimeObserved,
                         dayOrNight               = as.character(timeBins$dayOrNight)),
              by = "timeChunkId")
  
  levels(mtr$dayOrNight) = names(table(timeBins$dayOrNight))
  
  mtr = merge(mtr, 
              data.frame(altitudeChunkId          = altitudeBins$id, 
                         altitudeChunkBegin       = altitudeBins$begin,
                         altitudeChunkEnd         = altitudeBins$end,
                         altitudeChunkSize        = altitudeBins$size,
                         altitudeChunkAvgAltitude = altitudeBins$avgAltitude),
              by = "altitudeChunkId")
  
  mtr = mtr[order(mtr$timeChunkId, mtr$altitudeChunkId),]
  
# Reorder columns as originally
# =============================================================================
  mtr = mtr[, c("timeChunkId" , "timeChunkDate" , "timeChunkBegin" , 
                "timeChunkEnd" , "timeChunkDateSunset" , "timeChunkDuration_sec" ,           
                "observationTime_sec" , "observationTime_h" , "operationTime_sec" , 
                "blindTime_sec" , "proportionalTimeObserved" , "dayOrNight" ,      
                "altitudeChunkId" , "altitudeChunkBegin" , "altitudeChunkEnd" , 
                "altitudeChunkSize" , "altitudeChunkAvgAltitude")]
  
# Start showing progress, if requested
  # progressTotal    = (length(classSelection) + 1) * nrow(mtr) * 2
  # progressCnt      = 1
  # progressPercent  = 0
  # altitudeBinsStep = nrow(mtr) / length(unique(mtr$altitudeChunkId))
  # message('\r', paste0("MTR computation progress: ", progressPercent, "%"), 
  #         appendLF = FALSE)
  
# ----------------------- MTR ---------------------------#
# =============================================================================
  echoes$altitudeChunkId = as.integer(as.character(cut(echoes[,"feature1.altitude_AGL"], 
                                                       breaks = c(altitudeBins$begin, 
                                                                  altitudeBins$end[nrow(altitudeBins)]), 
                                                       label = altitudeBins$id, right = FALSE)))
  echoes$timeChunkId     = as.integer(as.character(cut(echoes[,"time_stamp_targetTZ"], 
                                                       breaks = c(timeBins$start, 
                                                                  timeBins$stop[nrow(timeBins)]), 
                                                       label = timeBins$id, right = FALSE)))
  all_mtr = echoes %>% 
          # add information on effective observation time
            dplyr::left_join(x  = ., 
                             y  = mtr %>% dplyr::distinct(timeChunkId, observationTime_h), 
                             by = "timeChunkId") %>% 
    
          # calculate the MTR for each echo - will be summed up in a later step
            dplyr::mutate("mtr_echo" = mtr_factor_rf / observationTime_h) %>% 
    
          # group the data with time and height intervals
            dplyr::group_by(timeChunkId, altitudeChunkId) %>% 
    
            dplyr::summarise(
              # count the number of echoes per timeXheight interval
                "nEchoes" = length(mtr_factor_rf), 
              # sum the mtr-factors of all echoes per timeXheight interval
                "sumOfMTRFactors" = sum(mtr_factor_rf, na.rm = TRUE), 
              # sum the mtr of all echoes per timeXheight interval - equivalent as "sumOfMTRFactors / observationTime_h"
                "mtr" = sum(mtr_echo, na.rm = TRUE)) %>% 
    
          # add the class denomination to merge with the per-class MTR dataset
            tibble::add_column(class = "allClasses") %>% 
    
          # select and reorder the columns of interest 
            dplyr::select(timeChunkId, altitudeChunkId, class, nEchoes, sumOfMTRFactors, mtr) %>% 
    
          # use wide-format to match @fabian's original format
            tidyr::pivot_wider(names_from =  class, values_from = c(nEchoes, sumOfMTRFactors, mtr), names_sep = ".", values_fill = 0) 
    
  each_mtr = echoes %>% 
           # add information on effective observation time
             dplyr::left_join(x  = ., 
                       y  = mtr %>% dplyr::distinct(timeChunkId, observationTime_h), 
                       by = "timeChunkId") %>% 
             dplyr::mutate("mtr_echo" = mtr_factor_rf/observationTime_h) %>%
             dplyr::group_by(timeChunkId, altitudeChunkId, class) %>% 
             dplyr::summarise("nEchoes" = length(mtr_factor_rf),
                              "sumOfMTRFactors" = sum(mtr_factor_rf, na.rm=TRUE),
                              "mtr" = sum(mtr_echo, na.rm = TRUE)) %>% 
             dplyr::select(timeChunkId, altitudeChunkId, class, nEchoes, sumOfMTRFactors, mtr) %>% 
             tidyr::pivot_wider(names_from  =  class, 
                                values_from = c(nEchoes, sumOfMTRFactors, mtr), 
                                names_sep   = ".", 
                                values_fill = 0)
  
  mtr = dplyr::left_join(mtr, all_mtr, by = c("timeChunkId", "altitudeChunkId")) %>% 
        dplyr::left_join(each_mtr, by = c("timeChunkId", "altitudeChunkId"))
  
# replace NA as ZERO for nEchoes, sumMTRfacotrs, mtr, if "proportionalTimeObserved"] != 0
# =============================================================================
  for (i in 0:length(classSelection)){ # i = 0
    i_class = ifelse(i == 0, "allClasses", classSelection[i])
    # if...
    i_index = which((mtr[, paste("nEchoes", i_class, sep = ".")] %in% NA) &
                    (mtr[, "proportionalTimeObserved"] != 0))
    mtr[i_index , paste("nEchoes", i_class, sep = ".")]         = 0
    mtr[i_index , paste("sumOfMTRFactors", i_class, sep = ".")] = 0
    mtr[i_index , paste("mtr", i_class, sep = ".")]             = 0
  }
 
# Compute MTR per day and night, if requested
# =============================================================================
  if (computePerDayNight == TRUE){
    #  Combine all timebins of one day (grouped by timeChunkDateSunset) to one 
    # =========================================================================
    if ("timeChunkDateSunset" %in% colnames(mtr) && !all(is.na(mtr$timeChunkDateSunset))){
      for (k in 1:length(unique(mtr$altitudeChunkId))){
        # Day MTR ----
        # =====================================================================
          # combine all timebins with the same value in 'timeChunkDateSunset','altitudeChunkId' and 'dayOrNight'
          # ===================================================================
            mtrTmp         = mtr[!is.na(mtr$dayOrNight) & 
                                 mtr$dayOrNight == "day" & 
                                 mtr$altitudeChunkId == k,]
            timeChunkDate  = mtrTmp %>% 
                              dplyr::group_by(timeChunkDateSunset) %>% 
                              dplyr::summarise(x = min(timeChunkBegin))
            timeChunkDate  = timeChunkDate[!is.na(timeChunkDate$x) & 
                                           !is.na(timeChunkDate$timeChunkDateSunset),]
            timeChunkBegin = mtrTmp %>% 
                              dplyr::group_by(timeChunkDateSunset) %>% 
                              dplyr::summarise(x = min(timeChunkBegin))
            timeChunkBegin = timeChunkBegin[!is.na(timeChunkBegin$x) & 
                                            !is.na(timeChunkBegin$timeChunkDateSunset),]
            timeChunkEnd   = mtrTmp %>% 
                              dplyr::group_by(timeChunkDateSunset) %>% 
                              dplyr::summarise(x = max(timeChunkEnd))
            timeChunkEnd          = timeChunkEnd[!is.na(timeChunkEnd$x) & 
                                                 !is.na(timeChunkEnd$timeChunkDateSunset),]
            timeChunkDateSunset   = mtrTmp %>% 
                                      dplyr::group_by(timeChunkDateSunset) %>% 
                                      dplyr::summarise(x = max(timeChunkDateSunset))
            timeChunkDateSunset   = timeChunkDateSunset[!is.na(timeChunkDateSunset$x) & 
                                                        !is.na(timeChunkDateSunset$timeChunkDateSunset),]
            operationTime_sec     = stats::aggregate(mtrTmp$operationTime_sec, 
                                                     list(mtrTmp$timeChunkDateSunset), 
                                                     sum, na.rm = TRUE)
            blindTime_sec         = stats::aggregate(mtrTmp$blindTime_sec, 
                                                     list(mtrTmp$timeChunkDateSunset), 
                                                     sum, na.rm = TRUE)
            observationTime_sec   = stats::aggregate(mtrTmp$observationTime_sec, 
                                                     list(mtrTmp$timeChunkDateSunset), 
                                                     sum, na.rm = TRUE)
            timeChunkDuration_sec = stats::aggregate(mtrTmp$timeChunkDuration_sec, 
                                                     list(mtrTmp$timeChunkDateSunset), 
                                                     sum, na.rm = TRUE)
            
            mtrDay = data.frame(timeChunkDate            = as.Date(timeChunkDate$x), 
                                timeChunkBegin           = timeChunkBegin$x, 
                                timeChunkEnd             = timeChunkEnd$x,
                                timeChunkDateSunset      = timeChunkDateSunset$x,
                                timeChunkDuration_sec    = timeChunkDuration_sec$x,
                                blindTime_sec            = blindTime_sec$x,
                                operationTime_sec        = operationTime_sec$x,
                                observationTime_sec      = observationTime_sec$x,
                                observationTime_h        = observationTime_sec$x / 3600,
                                proportionalTimeObserved = NA,
                                dayOrNight               = "day",
                                altitudeChunkId          = min(mtrTmp$altitudeChunkId),
                                altitudeChunkBegin       = min(mtrTmp$altitudeChunkBegin),
                                altitudeChunkEnd         = min(mtrTmp$altitudeChunkEnd),
                                altitudeChunkSize        = min(mtrTmp$altitudeChunkSize),
                                altitudeChunkAvgAltitude = min(mtrTmp$altitudeChunkAvgAltitude))
          
          # nEchoes
          # =====================================================================
            nEchoes = stats::aggregate(mtrTmp$nEchoes.allClasses, 
                                       list(mtrTmp$timeChunkDateSunset), 
                                       sum, na.rm = TRUE)
            mtrDay = data.frame(mtrDay, nEchoes.allClasses = nEchoes$x)
            for (i in 1:length(classSelection)){
              nEchoes = stats::aggregate(mtrTmp[, paste("nEchoes", classSelection[i], sep = ".")], 
                                         list(mtrTmp$timeChunkDateSunset), 
                                         sum, na.rm = TRUE)
              mtrDay[, paste("nEchoes", classSelection[i], sep = ".")] = nEchoes$x 
            }
          
          # sum of mtr factors
          # =====================================================================
            sumOfMTRFactors = stats::aggregate(mtrTmp$sumOfMTRFactors.allClasses, 
                                               list(mtrTmp$timeChunkDateSunset), 
                                               sum, na.rm = TRUE)
            mtrDay = data.frame(mtrDay, sumOfMTRFactors.allClasses = sumOfMTRFactors$x)
            for (i in 1:length(classSelection)){
              sumOfMTRFactors = stats::aggregate(mtrTmp[, paste("sumOfMTRFactors", 
                                                                classSelection[i], sep = ".")], 
                                                 list(mtrTmp$timeChunkDateSunset), 
                                                 sum, na.rm = TRUE)
              mtrDay[, paste("sumOfMTRFactors", classSelection[i], sep = ".")] = sumOfMTRFactors$x 
            }
          
          # mtr
          # =====================================================================
            mtrDay = data.frame(mtrDay, mtr.allClasses = NA)
            for (i in 1:length(classSelection)){
              mtrDay[, paste("mtr", classSelection[i], sep = ".")] = NA
            }
          
          # first quartiles
          # =====================================================================
            mtrFirstQuartile = suppressWarnings(
                                stats::aggregate(list(mtrFirstQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 stats::quantile(), 
                                                 prob = 0.25))
            mtrDay = merge(mtrDay, mtrFirstQuartile, 
                           by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrFirstQuartile = suppressWarnings(
                                  stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, 
                                                          paste("mtr", classSelection[i], sep = ".")], 
                                                   list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                   stats::quantile(), 
                                                   prob = 0.25))
              names(mtrFirstQuartile)[2] = paste("mtrFirstQuartile", classSelection[i], sep = ".")
              mtrDay  = merge(mtrDay, mtrFirstQuartile, 
                              by = "timeChunkDateSunset", all = TRUE)
            }
          
          # third quartiles
          # =====================================================================
            mtrThirdQuartile = suppressWarnings(
                                stats::aggregate(list(mtrThirdQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 stats::quantile(), 
                                                 prob = 0.75))
            mtrDay = merge(mtrDay, mtrThirdQuartile, 
                           by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrThirdQuartile = suppressWarnings(
                                  stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, 
                                                   paste("mtr", classSelection[i], sep = ".")], 
                                                   list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                   stats::quantile(), 
                                                   prob = 0.75))
              names(mtrThirdQuartile)[2] = paste("mtrThirdQuartile", classSelection[i], sep = ".")
              mtrDay  = merge(mtrDay, mtrThirdQuartile, 
                              by = "timeChunkDateSunset", all = TRUE)
            }
        
        # NIGHT MTR
        # =====================================================================
          # combine all timebins with the same value in 'timeChunkDateSunset','altitudeChunkId' and 'dayOrNight'
          # ===================================================================
            mtrTmp         = mtr[!is.na(mtr$dayOrNight) & 
                                 mtr$dayOrNight == "night" & 
                                 mtr$altitudeChunkId == k,]
            timeChunkDate  = mtrTmp %>% 
                               dplyr::group_by(timeChunkDateSunset) %>% 
                               dplyr::summarise(x = min(timeChunkBegin))
            timeChunkDate  = timeChunkDate[!is.na(timeChunkDate$x) & 
                                           !is.na(timeChunkDate$timeChunkDateSunset),]
            timeChunkBegin = mtrTmp %>% 
                               dplyr::group_by(timeChunkDateSunset) %>% 
                               dplyr::summarise(x = min(timeChunkBegin))
            timeChunkBegin = timeChunkBegin[!is.na(timeChunkBegin$x) & 
                                            !is.na(timeChunkBegin$timeChunkDateSunset),]
            timeChunkEnd   = mtrTmp %>% 
                               dplyr::group_by(timeChunkDateSunset) %>% 
                               dplyr::summarise(x = max(timeChunkEnd))
            timeChunkEnd   = timeChunkEnd[!is.na(timeChunkEnd$x) & 
                                          !is.na(timeChunkEnd$timeChunkDateSunset),]
            timeChunkDateSunset   = mtrTmp %>% 
                                      dplyr::group_by(timeChunkDateSunset) %>% 
                                      dplyr::summarise(x = max(timeChunkDateSunset))
            timeChunkDateSunset   = timeChunkDateSunset[!is.na(timeChunkDateSunset$x) & 
                                                        !is.na(timeChunkDateSunset$timeChunkDateSunset),]
            operationTime_sec     = stats::aggregate(mtrTmp$operationTime_sec, 
                                                     list(mtrTmp$timeChunkDateSunset), 
                                                     sum, na.rm = TRUE)
            blindTime_sec         = stats::aggregate(mtrTmp$blindTime_sec, 
                                                     list(mtrTmp$timeChunkDateSunset), 
                                                     sum, na.rm = TRUE)
            observationTime_sec   = stats::aggregate(mtrTmp$observationTime_sec, 
                                                     list(mtrTmp$timeChunkDateSunset), 
                                                     sum, na.rm = TRUE)
            timeChunkDuration_sec = stats::aggregate(mtrTmp$timeChunkDuration_sec, 
                                                     list(mtrTmp$timeChunkDateSunset), 
                                                     sum, na.rm = TRUE)
            
            mtrNight = data.frame(timeChunkDate            = as.Date(timeChunkDate$x), 
                                  timeChunkBegin           = timeChunkBegin$x, 
                                  timeChunkEnd             = timeChunkEnd$x,
                                  timeChunkDateSunset      = timeChunkDateSunset$x,
                                  timeChunkDuration_sec    = timeChunkDuration_sec$x,
                                  blindTime_sec            = blindTime_sec$x,
                                  operationTime_sec        = operationTime_sec$x,
                                  observationTime_sec      = observationTime_sec$x,
                                  observationTime_h        = observationTime_sec$x / 3600,
                                  proportionalTimeObserved = NA,
                                  dayOrNight               = "night",
                                  altitudeChunkId          = min(mtrTmp$altitudeChunkId),
                                  altitudeChunkBegin       = min(mtrTmp$altitudeChunkBegin),
                                  altitudeChunkEnd         = min(mtrTmp$altitudeChunkEnd),
                                  altitudeChunkSize        = min(mtrTmp$altitudeChunkSize),
                                  altitudeChunkAvgAltitude = min(mtrTmp$altitudeChunkAvgAltitude))
          
          # nEchoes
          # ===================================================================
            nEchoes = stats::aggregate(mtrTmp$nEchoes.allClasses, 
                                       list(mtrTmp$timeChunkDateSunset), 
                                       sum, na.rm = TRUE)
            mtrNight = data.frame(mtrNight, nEchoes.allClasses = nEchoes$x)
            for (i in 1:length(classSelection)){
              nEchoes = stats::aggregate(mtrTmp[, paste("nEchoes", classSelection[i], sep = ".")], 
                                         list(mtrTmp$timeChunkDateSunset), 
                                         sum, na.rm = TRUE)
              mtrNight[, paste("nEchoes", classSelection[i], sep = ".")] = nEchoes$x 
            }
          
          # sum of mtr factors
          # ===================================================================
            sumOfMTRFactors = stats::aggregate(mtrTmp$sumOfMTRFactors.allClasses, 
                                               list(mtrTmp$timeChunkDateSunset), 
                                               sum, na.rm = TRUE)
            mtrNight = data.frame(mtrNight, 
                                  sumOfMTRFactors.allClasses = sumOfMTRFactors$x)
            for (i in 1:length(classSelection)){
              sumOfMTRFactors = stats::aggregate(mtrTmp[, paste("sumOfMTRFactors", classSelection[i], sep = ".")], 
                                                 list(mtrTmp$timeChunkDateSunset), 
                                                 sum, na.rm = TRUE)
              mtrNight[, paste("sumOfMTRFactors", classSelection[i], sep = ".")] = sumOfMTRFactors$x 
            }
          
          # mtr
          # ===================================================================
            mtrNight = data.frame(mtrNight, mtr.allClasses = NA)
            for (i in 1:length(classSelection)){
              mtrNight[, paste("mtr", classSelection[i], sep = ".")] = NA
            }
          
          # first quartiles
          # ===================================================================
            mtrFirstQuartile = suppressWarnings(
                                 stats::aggregate(list(mtrFirstQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  stats::quantile(), 
                                                  prob = 0.25))
            mtrNight = merge(mtrNight, mtrFirstQuartile, 
                             by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrFirstQuartile = suppressWarnings(
                                   stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, paste("mtr", classSelection[i], sep = ".")], 
                                                    list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                    stats::quantile(), 
                                                    prob = 0.25))
              names(mtrFirstQuartile)[2] = paste("mtrFirstQuartile", 
                                                 classSelection[i], 
                                                 sep = ".")
              mtrNight  = merge(mtrNight, mtrFirstQuartile, 
                                by = "timeChunkDateSunset", all = TRUE)
            }
          
          # third quartiles
          # ===================================================================
            mtrThirdQuartile = suppressWarnings(
                                 stats::aggregate(list(mtrThirdQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  stats::quantile(), 
                                                  prob = 0.75))
            mtrNight = merge(mtrNight, mtrThirdQuartile, 
                             by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrThirdQuartile = suppressWarnings(
                                   stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, paste("mtr", classSelection[i], sep = ".")], 
                                                    list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                    stats::quantile(), 
                                                    prob = 0.75))
              names(mtrThirdQuartile)[2] = paste("mtrThirdQuartile", 
                                                 classSelection[i], 
                                                 sep = ".")
              mtrNight  = merge(mtrNight, mtrThirdQuartile, 
                                by = "timeChunkDateSunset", all = TRUE)
            }
        
            
        # Add day night mtrs for current altitude chunk to overview mtr data
        # ===================================================================== 
          if (exists("mtrDayNight")){
            mtrDayNight = rbind(mtrDayNight, mtrDay, mtrNight)
          } else {
            mtrDayNight = rbind(mtrDay, mtrNight)
          }
        
      }
      
      mtr = mtrDayNight
      
      # Proportional observation time
      # =======================================================================
        mtr$proportionalTimeObserved[mtr$operationTime_sec > 0] = mtr$observationTime_sec[mtr$operationTime_sec > 0] / 
                                                                    mtr$timeChunkDuration_sec[mtr$operationTime_sec > 0]
        mtr$proportionalTimeObserved[mtr$operationTime_sec == 0] = 0 
      
      # MTR
      # =======================================================================
        mtr$mtr.allClasses[mtr$observationTime_h > 0] = mtr$sumOfMTRFactors.allClasses[mtr$observationTime_h > 0] / 
                                                          mtr$observationTime_h[mtr$observationTime_h > 0]
        for (i in 1:length(classSelection)){
          mtr[mtr$observationTime_h > 0 , paste("mtr", classSelection[i], sep = ".")] = mtr[mtr$observationTime_h > 0, 
                                                                                            paste("sumOfMTRFactors", classSelection[i], sep = ".")] / 
                                                                                        mtr$observationTime_h[mtr$observationTime_h > 0]
        }
      
      # sort by timeChunkBegin and set timeChunkId
      # =======================================================================
        mtr = mtr[order(mtr$timeChunkBegin),]
        timeChunks = data.frame(timeChunkBegin = unique(mtr$timeChunkBegin), 
                                timeChunkId    = seq(1, length(unique(mtr$timeChunkBegin))))
        timeChunks = merge(timeChunks, 
                           data.frame(timeChunkBegin = mtr$timeChunkBegin), 
                           by = "timeChunkBegin")
        timeChunks = timeChunks[order(timeChunks$timeChunkBegin),]
        mtr        = data.frame(timeChunkId = timeChunks$timeChunkId, mtr)
      
    } else {
      warning(paste0("Compute MTR per day/night not possible. Set parameter ", 
                     "'computePerDayNight' to FALSE when calling the function ", 
                     "'computeMTR' or create the timeBins using the function ", 
                     "'createTimeBins' and set the parameter 'addDayNightSplit' ", 
                     "to TRUE."))
    }
  } 
  
# MTR set back to NA if...
# =============================================================================
  if (propObsTimeCutoff > 0){
    i_index = which(mtr[, "proportionalTimeObserved"] < propObsTimeCutoff)
    mtr[i_index , paste("mtr", i_class, sep = ".")] = NA
  }
  
  # progressStep = (progressTotal - progressCnt) / ((length(classSelection) + 1) * nrow(mtr))
  
# compute altitude distribution
# =============================================================================
  if (computeAltitudeDistribution){
    for (i in 0:length(classSelection)){
      if (i == 0){
        classLabel = "allClasses"
      } else {
        classLabel = classSelection[i]
      }
      
      mtr[, paste("meanAltitude", classLabel, sep = ".")]          = NA
      mtr[, paste("altitudeQuantile_0.05", classLabel, sep = ".")] = 0
      mtr[, paste("altitudeQuantile_0.25", classLabel, sep = ".")] = 0
      mtr[, paste("altitudeQuantile_0.5", classLabel, sep = ".")]  = 0
      mtr[, paste("altitudeQuantile_0.75", classLabel, sep = ".")] = 0
      mtr[, paste("altitudeQuantile_0.95", classLabel, sep = ".")] = 0
      
      for (k in 1:nrow(mtr)){
        if (i == 0){
          echoesInTimeAndAltitudeBin = echoes[echoes$feature1.altitude_AGL >= mtr$altitudeChunkBegin[k] & 
                                                echoes$feature1.altitude_AGL < mtr$altitudeChunkEnd[k] & 
                                                echoes$time_stamp_targetTZ >= mtr$timeChunkBegin[k] & 
                                                echoes$time_stamp_targetTZ < mtr$timeChunkEnd[k], 
                                              names(echoes) %in% c("feature1.altitude_AGL", "mtr_factor_rf")]
        } else {
          echoesInTimeAndAltitudeBin = echoes[echoes$class == classSelection[i] & 
                                                echoes$feature1.altitude_AGL >= mtr$altitudeChunkBegin[k] & 
                                                echoes$feature1.altitude_AGL < mtr$altitudeChunkEnd[k] & 
                                                echoes$time_stamp_targetTZ >= mtr$timeChunkBegin[k] & 
                                                echoes$time_stamp_targetTZ < mtr$timeChunkEnd[k], 
                                              names(echoes) %in% c("feature1.altitude_AGL", "mtr_factor_rf")]
        }
        
        if (mtr$observationTime_h[k] <= 0){
          mtr[k, paste("meanAltitude", classLabel, sep = ".")] = NA
        } else if (mtr[k, paste("sumOfMTRFactors", classLabel, sep = ".")] <= 0.000001){
          mtr[k, paste("meanAltitude", classLabel, sep = ".")] = 0
        } else {
          mtr[k, paste("meanAltitude", classLabel, sep = ".")] = stats::weighted.mean(x = echoesInTimeAndAltitudeBin$feature1.altitude_AGL, 
                                                                                      w = echoesInTimeAndAltitudeBin$mtr_factor_rf, 
                                                                                      na.rm = TRUE)
          if (nrow(echoesInTimeAndAltitudeBin) > 1){
            mtr[k, paste("altitudeQuantile_0.05", classLabel, sep = ".")] = suppressWarnings(
                                                                              modi::weighted.quantile(x = echoesInTimeAndAltitudeBin$feature1.altitude_AGL, 
                                                                                                      w = echoesInTimeAndAltitudeBin$mtr_factor_rf, 
                                                                                                      prob = 0.05))
            mtr[k, paste("altitudeQuantile_0.25", classLabel, sep = ".")] = suppressWarnings(
                                                                              modi::weighted.quantile(x = echoesInTimeAndAltitudeBin$feature1.altitude_AGL, 
                                                                                                      w = echoesInTimeAndAltitudeBin$mtr_factor_rf, 
                                                                                                      prob = 0.25))
            mtr[k, paste("altitudeQuantile_0.5", classLabel, sep = ".")] = suppressWarnings(
                                                                              modi::weighted.quantile(x = echoesInTimeAndAltitudeBin$feature1.altitude_AGL, 
                                                                                                      w = echoesInTimeAndAltitudeBin$mtr_factor_rf, 
                                                                                                      prob = 0.5))
            mtr[k, paste("altitudeQuantile_0.75", classLabel, sep = ".")] = suppressWarnings(
                                                                              modi::weighted.quantile(x = echoesInTimeAndAltitudeBin$feature1.altitude_AGL, 
                                                                                                      w = echoesInTimeAndAltitudeBin$mtr_factor_rf, 
                                                                                                      prob = 0.75))
            mtr[k, paste("altitudeQuantile_0.95", classLabel, sep = ".")] = suppressWarnings(
                                                                              modi::weighted.quantile(x = echoesInTimeAndAltitudeBin$feature1.altitude_AGL, 
                                                                                                      w = echoesInTimeAndAltitudeBin$mtr_factor_rf, 
                                                                                                      prob = 0.95))  
          }
        }
        
        # progressCnt = progressCnt + progressStep
        # if (floor(progressCnt / progressTotal * 100) > progressPercent){
        #   progressPercent = floor(progressCnt / progressTotal * 100)
        #   message('\r', paste0("MTR computation progress: ", progressPercent, "%"), appendLF = FALSE)
        # }
      }
    }
    
  }
  # message('\r', paste0("MTR computation progress: ", 100, "%"), appendLF = FALSE)
  # message(" ")
  
# Return MTR
# =============================================================================
  return(mtr)
}