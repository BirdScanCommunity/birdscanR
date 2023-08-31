#### computeMTR ------------------------------------------------------
#' @title computeMTR
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Baptiste Schmid, \email{baptiste.schmid@@vogelwarte.ch};  
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @description This function will estimate the Activity / Migration Traffic 
#' Rates (MTR, expressed as #objects / km / hour) based on the observations in 
#' your database.
#' @param dbName Character string, containing the name of the database you are 
#' processing
#' @param echoes dataframe with the echo data from the data list created by the 
#' function ‘extractDBData’ or a subset of it created by the function 
#' ‘filterEchoData’. 
#' @param classSelection character string vector with all classes which should 
#' be used to calculate the MTR. The MTR and number of Echoes will be calculated 
#' for each class as well as for all classes together. 
#' @param altitudeRange numeric vector of length 2 with the start and end of the 
#' altitude range in meter a.g.l. 
#' @param altitudeBinSize numeric, size of the altitude bins in meter. 
#' @param timeRange Character vector of length 2, with start and end of time 
#' range, formatted as "%Y-%m-%d %H:%M" 
#' @param timeBinDuration_sec duration of timeBins in seconds (numeric). for 
#' values <= 0 a duration of 1 hour will be set
#' @param timeZone time zone in which the time bins should be created as string, 
#' e.g. "Etc/GMT0"
#' @param sunriseSunset dataframe with sunrise/sunset, and civil and nautical 
#' dawn/dusk. Computed with the function 'twilight'.
#' @param sunOrCivil sunrise/sunset or civil dawn/dusk used to split day and 
#' night. Supported values: "sun" or "civil". Default: "civil"
#' @param crepuscule optional character variable, Set to “nauticalSolar” to use 
#' the time between nautical dusk/dawn and sunrise/sunset times to define the 
#' crepuscular period, or to "nauticalCivil" to use the time between nautical 
#' and civil dusk/dawn to define the crepuscular period, or to "civilSolar" to use 
#' the time between civil dusk/dawn and sunrise/sunset times to define the 
#' crepuscular period. Default is "nauticalSolar".
#' @param protocolData dataframe with the protocol data from the data list 
#' created by the function \code{extractDBData} or a subset of it created by the 
#' function \code{filterProtocolData}.
#' @param visibilityData dataframe with the visibility data from the data list 
#' created by the function ‘extractDBData’.
#' @param manualBlindTimes dataframe with the manual blind times created by the 
#' function \code{loadManualBlindTimes}.
#' @param saveBlindTimes Logical, determines whether to save the blind times to 
#' a file. Default: False.
#' @param blindTimesOutputDir Character string containing the path to save the 
#' blind times to. Default: 'your-working-directory'
#' @param blindTimeAsMtrZero character string vector with the blind time types 
#' which should be treated as observation time with MTR zero.
#' @param propObsTimeCutoff numeric between 0 and 1. If the MTR is computed per 
#' day and night, time bins with a proportional observation time smaller than 
#' propObsTimeCutoff are ignored when combining the time bins. If the MTR is 
#' computed for each time bin, the parameter is ignored.
#' @param computePerDayNight logical, TRUE: MTR is computed per day and night. 
#' The time bins of each day and night will be combined and the mean MTR is 
#' computed for each day and night. The spread (first and third Quartile) for 
#' each day and night are also computed. The spread is dependent on the chosen 
#' time bin duration/amount of time bins; When FALSE: MTR is computed for each 
#' time bin. This option computes the MTR for each time bin defined in the time 
#' bin dataframe. The time bins that were split due to sunrise/sunset during the 
#' time bin will be combined to one bin. 
#' @param computePerDayCrepusculeNight logical, TRUE: MTR is computed per 
#' crepusculeMorning, day, crepusculeEvening, and night. The time bins of each 
#' of these diel phases will be combined and the mean MTR is computed for each 
#' phase. The spread (first and third Quartile) for each phase is also computed. 
#' The spread is dependent on the chosen time bin duration/amount of time bins; 
#' When FALSE: MTR is computed for each time bin. This option computes the MTR 
#' for each time bin defined in the time bin dataframe. The time bins that were 
#' split due to sunrise/sunset during the time bin will be combined to one bin. 
#' Default = FALSE.
#' @param computeAltitudeDistribution logical, TRUE: compute the mean height and 
#' altitude distribution of MTR for the pre-defined quantiles 0.05, 0.25, 0.5, 
#' 0.75, 0.95
#'
#' @return Migration Traffic Rates
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' # Set server, database, and other input settings
#' # ===========================================================================
#'   dbServer       = "MACHINE\\SERVERNAME"     # Set the name of your SQL server
#'   dbName         = "db_Name"                   # Set the name of your database
#'   dbDriverChar   = "SQL Server"                # Set either "SQL Server" or "PostgreSQL"
#'   mainOutputDir  = file.path(".", "results")
#'   radarTimeZone  = "Etc/GMT0"
#'   targetTimeZone = "Etc/GMT0"
#'   listOfRfFeaturesToExtract = c(167, 168)
#'   siteLocation   = c(47.494427, 8.716432)
#'   sunOrCivil     = "civil"
#'   crepuscule     = "nauticalSolar"
#'   timeRangeData  = c("2021-01-15 00:00", "2021-01-31 00:00")
#'  
#' # Get data
#' # ===========================================================================
#'   dbData = extractDbData(dbDriverChar                   = dbDriverChar,
#'                          dbServer                       = dbServer, 
#'                          dbName                         = dbName, 
#'                          saveDbToFile                   = TRUE,
#'                          dbDataDir                      = mainOutputDir,
#'                          radarTimeZone                  = radarTimeZone,
#'                          targetTimeZone                 = targetTimeZone,
#'                          listOfRfFeaturesToExtract      = listOfRfFeaturesToExtract,
#'                          siteLocation                   = siteLocation, 
#'                          sunOrCivil                     = sunOrCivil, 
#'                          crepuscule                     = crepuscule)
#'                          
#' # Get sunrise/sunset 
#' # ===========================================================================
#'   sunriseSunset = twilight(timeRange = timeRangeData,
#'                            latLon    = c(47.494427, 8.716432),
#'                            timeZone  = targetTimeZone)
#'                           
#' # Get manual blind times
#' # ===========================================================================
#'   data(manualBlindTimes)
#'   cManualBlindTimes = manualBlindTimes
#' 
#' # Compute migration traffic rate
#' # ===========================================================================
#'   classSelection.mtr = c("insect")
#'   mtrData = computeMTR(dbName                       = dbName, 
#'                        echoes                       = dbData$echoData, 
#'                        classSelection               = classSelection.mtr, 
#'                        altitudeRange                = c(25, 1025),
#'                        altitudeBinSize              = 50,
#'                        timeRange                    = timeRangeData, 
#'                        timeBinDuration_sec          = 1800,
#'                        timeZone                     = targetTimeZone,
#'                        sunriseSunset                = sunriseSunset,
#'                        sunOrCivil                   = "civil",
#'                        crepuscule                   = crepuscule,
#'                        protocolData                 = dbData$protocolData, 
#'                        visibilityData               = dbData$visibilityData,
#'                        manualBlindTimes             = cManualBlindTimes,
#'                        saveBlindTimes               = FALSE,
#'                        blindTimesOutputDir          = getwd(),
#'                        blindTimeAsMtrZero           = NULL,
#'                        propObsTimeCutoff            = 0, 
#'                        computePerDayNight           = FALSE,
#'                        computePerDayCrepusculeNight = FALSE 
#'                        computeAltitudeDistribution  = TRUE)   
#' }
#' 
# =============================================================================
computeMTR = function(dbName, 
                      echoes, 
                      classSelection, 
                      altitudeRange,
                      altitudeBinSize,
                      timeRange, 
                      timeBinDuration_sec,
                      timeZone,
                      sunriseSunset,
                      sunOrCivil                   = "civil",
                      crepuscule                   = "nauticalSolar",
                      protocolData, 
                      visibilityData,
                      manualBlindTimes             = NULL,
                      saveBlindTimes               = FALSE,
                      blindTimesOutputDir          = getwd(),
                      blindTimeAsMtrZero           = NULL,
                      propObsTimeCutoff            = 0, 
                      computePerDayNight           = FALSE, 
                      computePerDayCrepusculeNight = FALSE,
                      computeAltitudeDistribution  = TRUE){
# Check whether only one of the options of computePerDayCrepusculeNight and 
#  computePerDayNight has been chosen
# =============================================================================
  if (computePerDayNight & computePerDayCrepusculeNight){
    stop(paste0("Please set only one of the options 'computePerDayNight' or ", 
                "'computePerDayCrepusculeNight' to TRUE, and rerun computeMTR()."))
  }
  
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
 
# Convert the timebin time range input to a POSIXct object
# =============================================================================
  timeRange = as.POSIXct(timeRange, 
                         format = "%Y-%m-%d %H:%M", 
                         tz     = timeZone)
   
# Set variables based on input to determine which kind of time bins to calculate 
# =============================================================================
  if (computePerDayNight){
    dnBins   = TRUE
    crepBins = FALSE
  } else if (computePerDayCrepusculeNight){
    dnBins   = FALSE
    crepBins = TRUE
  } else {
    dnBins   = TRUE
    crepBins = FALSE
  }
  
# Create Timebins
# =============================================================================
  message("Creating time bins..")
  timeBins = createTimeBins(timeRange           = timeRange, 
                            timeBinDuration_sec = timeBinDuration_sec, 
                            timeZone            = timeZone, 
                            dnBins              = dnBins,
                            crepBins            = crepBins,
                            sunriseSunset       = sunriseSunset, 
                            sunOrCivil          = sunOrCivil,
                            crepuscule          = crepuscule)
  
# compute blindtimes
# =====================================================================
  message("Calculating blind times..")
  blindTimes = mergeVisibilityAndManualBlindTimes(visibilityData   = visibilityData, 
                                                  manualBlindTimes = manualBlindTimes, 
                                                  protocolData     = protocolData)
  
# Save blind times to file, if requested
# =============================================================================
  if (saveBlindTimes){
    saveRDS(blindTimes, file = file.path(blindTimesOutputDir, 
                                         paste0(dbName, "_overallBlindTimes.rds")))
  }
  
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
  
# combine time bins split by day/night,                                                        
#  if neither computePerDayNight or computePerDayCrepusculeNight were requested
# =============================================================================
  if ((!computePerDayNight) & (!computePerDayCrepusculeNight)){
    for (i in 2:nrow(timeBins)){
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
    
    # exclude combined time bins and reset time bins id
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
  
# add features to time and altitude chunk IDs                                ### FROM HERE, definitely needs changing
# =============================================================================
  if (computePerDayCrepusculeNight){
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
                           dielPhase                = as.character(timeBins$dielPhase)),
                by = "timeChunkId")
    levels(mtr$dielPhase) <- c("crepusculeMorning", "day", "crepusculeEvening", "night")
  } else {
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
  }
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
  if (computePerDayCrepusculeNight){
    mtr = mtr[, c("timeChunkId" , "timeChunkDate" , "timeChunkBegin" , 
                  "timeChunkEnd" , "timeChunkDateSunset" , "timeChunkDuration_sec" ,           
                  "observationTime_sec" , "observationTime_h" , "operationTime_sec" , 
                  "blindTime_sec" , "proportionalTimeObserved" , "dielPhase" ,      
                  "altitudeChunkId" , "altitudeChunkBegin" , "altitudeChunkEnd" , 
                  "altitudeChunkSize" , "altitudeChunkAvgAltitude")]
  } else {
    mtr = mtr[, c("timeChunkId" , "timeChunkDate" , "timeChunkBegin" , 
                  "timeChunkEnd" , "timeChunkDateSunset" , "timeChunkDuration_sec" ,           
                  "observationTime_sec" , "observationTime_h" , "operationTime_sec" , 
                  "blindTime_sec" , "proportionalTimeObserved" , "dayOrNight" ,      
                  "altitudeChunkId" , "altitudeChunkBegin" , "altitudeChunkEnd" , 
                  "altitudeChunkSize" , "altitudeChunkAvgAltitude")]
  }
  
  
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
                                                       label = altitudeBins$id, 
                                                       right = FALSE)))
  echoes$timeChunkId     = as.integer(as.character(cut(echoes[,"time_stamp_targetTZ"], 
                                                       breaks = c(timeBins$start, 
                                                                  timeBins$stop[nrow(timeBins)]), 
                                                       label = timeBins$id, 
                                                       right = FALSE)))
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
              # sum the MTR-factors of all echoes per timeXheight interval
                "sumOfMTRFactors" = sum(mtr_factor_rf, na.rm = TRUE), 
              # sum the MTR of all echoes per timeXheight interval - equivalent as "sumOfMTRFactors / observationTime_h"
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
  
# replace NA as ZERO for nEchoes, sumMTRfactors, MTR, if "proportionalTimeObserved"] != 0
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
    #  Combine all time bins of one day (grouped by timeChunkDateSunset) to one 
    # =========================================================================
    if ("timeChunkDateSunset" %in% colnames(mtr) && !all(is.na(mtr$timeChunkDateSunset))){
      for (k in 1:length(unique(mtr$altitudeChunkId))){
        # Day MTR ----
        # =====================================================================
          # combine all time bins with the same value in 'timeChunkDateSunset',
          # 'altitudeChunkId' and 'dayOrNight'
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
          # ====================================================================
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
          # ====================================================================
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
          
          # MTR
          # ====================================================================
            mtrDay = data.frame(mtrDay, mtr.allClasses = NA)
            for (i in 1:length(classSelection)){
              mtrDay[, paste("mtr", classSelection[i], sep = ".")] = NA
            }
          
          # first quartiles
          # ====================================================================
            mtrFirstQuartile = suppressWarnings(
                                stats::aggregate(list(mtrFirstQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 FUN = function(x) stats::quantile(x, prob = 0.25)))
            mtrDay = merge(mtrDay, mtrFirstQuartile, 
                           by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrFirstQuartile = suppressWarnings(
                                  stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, 
                                                          paste("mtr", classSelection[i], sep = ".")], 
                                                   list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                   FUN = function(x) stats::quantile(x, prob = 0.25)))
              names(mtrFirstQuartile)[2] = paste("mtrFirstQuartile", classSelection[i], sep = ".")
              mtrDay  = merge(mtrDay, mtrFirstQuartile, 
                              by = "timeChunkDateSunset", all = TRUE)
            }
          
          # third quartiles
          # ====================================================================
            mtrThirdQuartile = suppressWarnings(
                                stats::aggregate(list(mtrThirdQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 FUN = function(x) stats::quantile(x, prob = 0.75)))
            mtrDay = merge(mtrDay, mtrThirdQuartile, 
                           by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrThirdQuartile = suppressWarnings(
                                  stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, 
                                                   paste("mtr", classSelection[i], sep = ".")], 
                                                   list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                   FUN = function(x) stats::quantile(x, prob = 0.75)))
              names(mtrThirdQuartile)[2] = paste("mtrThirdQuartile", classSelection[i], sep = ".")
              mtrDay  = merge(mtrDay, mtrThirdQuartile, 
                              by = "timeChunkDateSunset", all = TRUE)
            }
        
        # NIGHT MTR
        # =====================================================================
          # combine all time bins with the same value in 'timeChunkDateSunset',
          # 'altitudeChunkId' and 'dayOrNight'
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
          
          # sum of MTR factors
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
          
          # MTR
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
                                                  FUN = function(x) stats::quantile(x, prob = 0.25)))
            mtrNight = merge(mtrNight, mtrFirstQuartile, 
                             by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrFirstQuartile = suppressWarnings(
                                   stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, paste("mtr", classSelection[i], sep = ".")], 
                                                    list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                    FUN = function(x) stats::quantile(x, prob = 0.25)))
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
                                                  FUN = function(x) stats::quantile(x, prob = 0.75)))
            mtrNight = merge(mtrNight, mtrThirdQuartile, 
                             by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrThirdQuartile = suppressWarnings(
                                   stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, paste("mtr", classSelection[i], sep = ".")], 
                                                    list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                    FUN = function(x) stats::quantile(x, prob = 0.75)))
              names(mtrThirdQuartile)[2] = paste("mtrThirdQuartile", 
                                                 classSelection[i], 
                                                 sep = ".")
              mtrNight  = merge(mtrNight, mtrThirdQuartile, 
                                by = "timeChunkDateSunset", all = TRUE)
            }
        
            
        # Add day night mtrs for current altitude chunk to overview MTR data
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
                     "'createTimeBins' and set the parameter 'dnBins' ", 
                     "to TRUE."))
    }
  } 
  
# Compute MTR per day, night, and crepuscular twilight phase, if requested        
# =============================================================================
  if (computePerDayCrepusculeNight == TRUE){
    #  Combine all time bins of one day (grouped by timeChunkDateSunset) to one 
    # =========================================================================
    if ("timeChunkDateSunset" %in% colnames(mtr) && !all(is.na(mtr$timeChunkDateSunset))){
      for (k in 1:length(unique(mtr$altitudeChunkId))){
        # Day MTR ----
        # =====================================================================
          # combine all time bins with the same value in 'timeChunkDateSunset',
          # 'altitudeChunkId' and 'dielPhase'
          # ===================================================================
            mtrTmp         = mtr[!is.na(mtr$dielPhase) & 
                                 mtr$dielPhase == "day" & 
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
                                dielPhase               = "day",
                                altitudeChunkId          = min(mtrTmp$altitudeChunkId),
                                altitudeChunkBegin       = min(mtrTmp$altitudeChunkBegin),
                                altitudeChunkEnd         = min(mtrTmp$altitudeChunkEnd),
                                altitudeChunkSize        = min(mtrTmp$altitudeChunkSize),
                                altitudeChunkAvgAltitude = min(mtrTmp$altitudeChunkAvgAltitude))
          
          # nEchoes
          # ====================================================================
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
          # ====================================================================
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
          
          # MTR
          # ====================================================================
            mtrDay = data.frame(mtrDay, mtr.allClasses = NA)
            for (i in 1:length(classSelection)){
              mtrDay[, paste("mtr", classSelection[i], sep = ".")] = NA
            }
          
          # first quartiles
          # ====================================================================
            mtrFirstQuartile = suppressWarnings(
                                stats::aggregate(list(mtrFirstQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 FUN = function(x) stats::quantile(x, prob = 0.25)))
            mtrDay = merge(mtrDay, mtrFirstQuartile, 
                           by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrFirstQuartile = suppressWarnings(
                                  stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, 
                                                          paste("mtr", classSelection[i], sep = ".")], 
                                                   list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                   FUN = function(x) stats::quantile(x, prob = 0.25)))
              names(mtrFirstQuartile)[2] = paste("mtrFirstQuartile", classSelection[i], sep = ".")
              mtrDay  = merge(mtrDay, mtrFirstQuartile, 
                              by = "timeChunkDateSunset", all = TRUE)
            }
          
          # third quartiles
          # ====================================================================
            mtrThirdQuartile = suppressWarnings(
                                stats::aggregate(list(mtrThirdQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                 FUN = function(x) stats::quantile(x, prob = 0.75)))
            mtrDay = merge(mtrDay, mtrThirdQuartile, 
                           by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrThirdQuartile = suppressWarnings(
                                  stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, 
                                                   paste("mtr", classSelection[i], sep = ".")], 
                                                   list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                   FUN = function(x) stats::quantile(x, prob = 0.75)))
              names(mtrThirdQuartile)[2] = paste("mtrThirdQuartile", classSelection[i], sep = ".")
              mtrDay  = merge(mtrDay, mtrThirdQuartile, 
                              by = "timeChunkDateSunset", all = TRUE)
            }
        
        # NIGHT MTR
        # =====================================================================
          # combine all time bins with the same value in 'timeChunkDateSunset',
          # 'altitudeChunkId' and 'dielPhase'
          # ===================================================================
            mtrTmp         = mtr[!is.na(mtr$dielPhase) & 
                                 mtr$dielPhase == "night" & 
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
                                  dielPhase               = "night",
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
          
          # sum of MTR factors
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
          
          # MTR
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
                                                  FUN = function(x) stats::quantile(x, prob = 0.25)))
            mtrNight = merge(mtrNight, mtrFirstQuartile, 
                             by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrFirstQuartile = suppressWarnings(
                                   stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, paste("mtr", classSelection[i], sep = ".")], 
                                                    list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                    FUN = function(x) stats::quantile(x, prob = 0.25)))
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
                                                  FUN = function(x) stats::quantile(x, prob = 0.75)))
            mtrNight = merge(mtrNight, mtrThirdQuartile, 
                             by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrThirdQuartile = suppressWarnings(
                                   stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, paste("mtr", classSelection[i], sep = ".")], 
                                                    list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                    FUN = function(x) stats::quantile(x, prob = 0.75)))
              names(mtrThirdQuartile)[2] = paste("mtrThirdQuartile", 
                                                 classSelection[i], 
                                                 sep = ".")
              mtrNight  = merge(mtrNight, mtrThirdQuartile, 
                                by = "timeChunkDateSunset", all = TRUE)
            }
            
        # crepusculeMorning MTR
        # =====================================================================
          # combine all time bins with the same value in 'timeChunkDateSunset',
          # 'altitudeChunkId' and 'dielPhase'
          # ===================================================================
            mtrTmp         = mtr[!is.na(mtr$dielPhase) & 
                                 mtr$dielPhase == "crepusculeMorning" & 
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
            
            mtrCrepMorn = data.frame(timeChunkDate            = as.Date(timeChunkDate$x), 
                                     timeChunkBegin           = timeChunkBegin$x, 
                                     timeChunkEnd             = timeChunkEnd$x,
                                     timeChunkDateSunset      = timeChunkDateSunset$x,
                                     timeChunkDuration_sec    = timeChunkDuration_sec$x,
                                     blindTime_sec            = blindTime_sec$x,
                                     operationTime_sec        = operationTime_sec$x,
                                     observationTime_sec      = observationTime_sec$x,
                                     observationTime_h        = observationTime_sec$x / 3600,
                                     proportionalTimeObserved = NA,
                                     dielPhase               = "crepusculeMorning",
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
            mtrCrepMorn = data.frame(mtrCrepMorn, nEchoes.allClasses = nEchoes$x)
            for (i in 1:length(classSelection)){
              nEchoes = stats::aggregate(mtrTmp[, paste("nEchoes", classSelection[i], sep = ".")], 
                                         list(mtrTmp$timeChunkDateSunset), 
                                         sum, na.rm = TRUE)
              mtrCrepMorn[, paste("nEchoes", classSelection[i], sep = ".")] = nEchoes$x 
            }
          
          # sum of MTR factors
          # ===================================================================
            sumOfMTRFactors = stats::aggregate(mtrTmp$sumOfMTRFactors.allClasses, 
                                               list(mtrTmp$timeChunkDateSunset), 
                                               sum, na.rm = TRUE)
            mtrCrepMorn = data.frame(mtrCrepMorn, 
                                  sumOfMTRFactors.allClasses = sumOfMTRFactors$x)
            for (i in 1:length(classSelection)){
              sumOfMTRFactors = stats::aggregate(mtrTmp[, paste("sumOfMTRFactors", classSelection[i], sep = ".")], 
                                                 list(mtrTmp$timeChunkDateSunset), 
                                                 sum, na.rm = TRUE)
              mtrCrepMorn[, paste("sumOfMTRFactors", classSelection[i], sep = ".")] = sumOfMTRFactors$x 
            }
          
          # MTR
          # ===================================================================
            mtrCrepMorn = data.frame(mtrCrepMorn, mtr.allClasses = NA)
            for (i in 1:length(classSelection)){
              mtrCrepMorn[, paste("mtr", classSelection[i], sep = ".")] = NA
            }
          
          # first quartiles
          # ===================================================================
            mtrFirstQuartile = suppressWarnings(
                                 stats::aggregate(list(mtrFirstQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  FUN = function(x) stats::quantile(x, prob = 0.25)))
            mtrCrepMorn = merge(mtrCrepMorn, mtrFirstQuartile, 
                             by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrFirstQuartile = suppressWarnings(
                                   stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, paste("mtr", classSelection[i], sep = ".")], 
                                                    list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                    FUN = function(x) stats::quantile(x, prob = 0.25)))
              names(mtrFirstQuartile)[2] = paste("mtrFirstQuartile", 
                                                 classSelection[i], 
                                                 sep = ".")
              mtrCrepMorn  = merge(mtrCrepMorn, mtrFirstQuartile, 
                                by = "timeChunkDateSunset", all = TRUE)
            }
          
          # third quartiles
          # ===================================================================
            mtrThirdQuartile = suppressWarnings(
                                 stats::aggregate(list(mtrThirdQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  FUN = function(x) stats::quantile(x, prob = 0.75)))
            mtrCrepMorn = merge(mtrCrepMorn, mtrThirdQuartile, 
                             by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrThirdQuartile = suppressWarnings(
                                   stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, paste("mtr", classSelection[i], sep = ".")], 
                                                    list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                    FUN = function(x) stats::quantile(x, prob = 0.75)))
              names(mtrThirdQuartile)[2] = paste("mtrThirdQuartile", 
                                                 classSelection[i], 
                                                 sep = ".")
              mtrCrepMorn  = merge(mtrCrepMorn, mtrThirdQuartile, 
                                by = "timeChunkDateSunset", all = TRUE)
            }
        
        # crepusculeEvening MTR
        # =====================================================================
          # combine all time bins with the same value in 'timeChunkDateSunset',
          # 'altitudeChunkId' and 'dielPhase'
          # ===================================================================
            mtrTmp         = mtr[!is.na(mtr$dielPhase) & 
                                 mtr$dielPhase == "crepusculeEvening" & 
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
            
            mtrCrepEve = data.frame(timeChunkDate            = as.Date(timeChunkDate$x), 
                                     timeChunkBegin           = timeChunkBegin$x, 
                                     timeChunkEnd             = timeChunkEnd$x,
                                     timeChunkDateSunset      = timeChunkDateSunset$x,
                                     timeChunkDuration_sec    = timeChunkDuration_sec$x,
                                     blindTime_sec            = blindTime_sec$x,
                                     operationTime_sec        = operationTime_sec$x,
                                     observationTime_sec      = observationTime_sec$x,
                                     observationTime_h        = observationTime_sec$x / 3600,
                                     proportionalTimeObserved = NA,
                                     dielPhase               = "crepusculeEvening",
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
            mtrCrepEve = data.frame(mtrCrepEve, nEchoes.allClasses = nEchoes$x)
            for (i in 1:length(classSelection)){
              nEchoes = stats::aggregate(mtrTmp[, paste("nEchoes", classSelection[i], sep = ".")], 
                                         list(mtrTmp$timeChunkDateSunset), 
                                         sum, na.rm = TRUE)
              mtrCrepEve[, paste("nEchoes", classSelection[i], sep = ".")] = nEchoes$x 
            }
          
          # sum of MTR factors
          # ===================================================================
            sumOfMTRFactors = stats::aggregate(mtrTmp$sumOfMTRFactors.allClasses, 
                                               list(mtrTmp$timeChunkDateSunset), 
                                               sum, na.rm = TRUE)
            mtrCrepEve = data.frame(mtrCrepEve, 
                                  sumOfMTRFactors.allClasses = sumOfMTRFactors$x)
            for (i in 1:length(classSelection)){
              sumOfMTRFactors = stats::aggregate(mtrTmp[, paste("sumOfMTRFactors", classSelection[i], sep = ".")], 
                                                 list(mtrTmp$timeChunkDateSunset), 
                                                 sum, na.rm = TRUE)
              mtrCrepEve[, paste("sumOfMTRFactors", classSelection[i], sep = ".")] = sumOfMTRFactors$x 
            }
          
          # MTR
          # ===================================================================
            mtrCrepEve = data.frame(mtrCrepEve, mtr.allClasses = NA)
            for (i in 1:length(classSelection)){
              mtrCrepEve[, paste("mtr", classSelection[i], sep = ".")] = NA
            }
          
          # first quartiles
          # ===================================================================
            mtrFirstQuartile = suppressWarnings(
                                 stats::aggregate(list(mtrFirstQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  FUN = function(x) stats::quantile(x, prob = 0.25)))
            mtrCrepEve = merge(mtrCrepEve, mtrFirstQuartile, 
                             by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrFirstQuartile = suppressWarnings(
                                   stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, paste("mtr", classSelection[i], sep = ".")], 
                                                    list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                    FUN = function(x) stats::quantile(x, prob = 0.25)))
              names(mtrFirstQuartile)[2] = paste("mtrFirstQuartile", 
                                                 classSelection[i], 
                                                 sep = ".")
              mtrCrepEve  = merge(mtrCrepEve, mtrFirstQuartile, 
                                by = "timeChunkDateSunset", all = TRUE)
            }
          
          # third quartiles
          # ===================================================================
            mtrThirdQuartile = suppressWarnings(
                                 stats::aggregate(list(mtrThirdQuartile.allClasses = mtrTmp$mtr.allClasses[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                  FUN = function(x) stats::quantile(x, prob = 0.75)))
            mtrCrepEve = merge(mtrCrepEve, mtrThirdQuartile, 
                             by = "timeChunkDateSunset", all = TRUE)
            for (i in 1:length(classSelection)){
              mtrThirdQuartile = suppressWarnings(
                                   stats::aggregate(mtrTmp[mtrTmp$proportionalTimeObserved > propObsTimeCutoff, paste("mtr", classSelection[i], sep = ".")], 
                                                    list(timeChunkDateSunset = mtrTmp$timeChunkDateSunset[mtrTmp$proportionalTimeObserved > propObsTimeCutoff]), 
                                                    FUN = function(x) stats::quantile(x, prob = 0.75)))
              names(mtrThirdQuartile)[2] = paste("mtrThirdQuartile", 
                                                 classSelection[i], 
                                                 sep = ".")
              mtrCrepEve  = merge(mtrCrepEve, mtrThirdQuartile, 
                                by = "timeChunkDateSunset", all = TRUE)
            }
                
        # Add day night mtrs for current altitude chunk to overview MTR data
        # ===================================================================== 
          if (exists("mtrCrepuscular")){
            mtrCrepuscular = rbind(mtrCrepuscular, mtrDay, mtrNight, mtrCrepMorn, mtrCrepEve)
          } else {
            mtrCrepuscular = rbind(mtrDay, mtrNight, mtrCrepMorn, mtrCrepEve)
          }
        
      }
      
      mtr = mtrCrepuscular
      
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
      warning(paste0("Compute MTR crepuscular not possible. Set parameter ", 
                     "'computePerDayCrepusculeNight' to FALSE when calling the function ", 
                     "'computeMTR' or create the timeBins using the function ", 
                     "'createTimeBins' and set the parameter 'crepBins' ", 
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