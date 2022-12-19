#### loadManualBlindTimes ------------------------------------------------------
#' @title loadManualBlindTimes
#' @author Fabian Hertner (SBRS), \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest (SOI), \email{birgen.haest@@vogelwarte.ch}
#' @description Load manual blindtimes from csv file.
#' For the MTR computation the times when the radar was blind have to be known. The radar itself can 
#' be blind in case of a protocol change (blocktime at the beginning of each protocol, usually 60s) or 
#' due to rain/snow or clutter (nearby objects, leaves or similar on radome, etc.). These times are 
#' stored in the visibility table or in the time_bins table in relation to the time bins duration (5min). 
#' To be flexible and not fixed to the 5 min time bins created by the radar, the visibility table is used 
#' in this script. 
#' In addition to the radar blind times, manual blind times can be defined. Manual blind times have to 
#' be defined in a csv file and are loaded with the function ‘loadManualBlindTimes’. A example dataset is 
#' available by running: 
#'  `data(manualBlindTimes)`
#'  `write.csv(manualBlindTimes, file = 'the output file destination', row.names = F)`
#' The filepath is defined as a global variable ‘manualBlindTimesFile’. A custom file and filepath 
#' can be used instead. 
#' The manual blind times have to be entered with 3 columns: 
#' start time 'yyyy-mm-dd hh:mm:ss', stop time 'yyyy-MM-dd hh:mm:ss', type.  
#' 
#' Example: 
#' 2021-01-16 04:15:00,2021-01-16 05:42:00,rain 
#' 2021-01-17 16:33:00,2021-01-17 18:04:00,clutter 
#' Manual blind time types can be chosen freely. When computing observation times, it can be decided 
#' if some of the defined manual blind time types should be treated as observed time with MTR zero or 
#' as blind time (e.g. rain). If no file is present or the file is empty, no manual blind times will 
#' be computed. 
#' @param filePath character string, absolute filepath of the manual blind time file
#' @param blindTimesTZ timezone of the blind times 
#' @param targetTZ target timezone of the blind times
#' @return data frame with manual blind times
#' @export
#'
loadManualBlindTimes = function(filePath, 
                                blindTimesTZ, 
                                targetTZ){
  # Check whether file exists
  # ===========================================================================
    if (!file.exists(filePath)){
      paste0("manual blind times file does not exist: '", filePath)
      manualBlindTimes = NULL
    } else {
    # read csv with blindtimes
    # ===========================================================================
      manualBlindTimes = tryCatch(utils::read.csv(file = filePath, header = FALSE), 
                                  error = function(x) x = NULL)
      
    # If not empty, name columns
    # ===========================================================================
      names(manualBlindTimes) = c("start", "stop", "type")
  
    # convert blindtimes to target timezone
    # ===========================================================================
      manualBlindTimes = convertTimeZone(data     = manualBlindTimes, 
                                         colNames = c("start", "stop"), 
                                         originTZ = blindTimesTZ, 
                                         targetTZ = targetTZ )
    }
  
  # Return manual blind times
  # ===========================================================================
    return(manualBlindTimes)
}
