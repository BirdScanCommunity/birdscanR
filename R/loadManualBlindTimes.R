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
#' be defined in a csv file and loaded with the function ‘loadManualBlindTimes’. A default file is 
#' available and can be edited: [your-project-directory]/Code/manualBlindTimes/manualBlindTimes.csv 
#' The filepath is defined as a global variable ‘manualBlindTimesFile’. A custom file and filepath 
#' can be used instead. 
#' The manual blind times have to be entered with 3 columns: 
#' start time [yyyy-mm-dd hh:mm:ss], stop time [yyyy-MM-dd hh:mm:ss], type.  
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
# #' @examples
# #' loadManualBlindTimes( filePath = ManualBlindTimesFile, blindTimesTZ = radarTimeZone, blindTimesTZ = targetTimeZone )
loadManualBlindTimes = function(filePath, blindTimesTZ, targetTZ){
  # Check whether file exists
  # ===========================================================================
    if (!file.exists(filePath)){
      stop(paste0("Cannot read the manual blind times file: '", filePath, 
                  "' because it does not exist.. ", "Check your input!"))
    }  
  
  # read csv with blindtimes
  # ===========================================================================
    manualBlindTimes        = read.csv(file = filePath, header = FALSE) 
    
  # Check whether there are any data in the file, and if not, set to NULL
  # ===========================================================================
    if (nrow(manualBlindTimes) == 0){
      manualBlindTimes = NULL
      return(manualBlindTimes)
    } 
    
  # If not empty, name columns
  # ===========================================================================
    names(manualBlindTimes) = c("start", "stop", "type")

  # convert blindtimes to target timezone
  # ===========================================================================
    manualBlindTimes = convertTimeZone(data     = manualBlindTimes, 
                                       colNames = c("start", "stop"), 
                                       originTZ = blindTimesTZ, 
                                       targetTZ = targetTZ )
  
  # Return manual blind times
  # ===========================================================================
    return(manualBlindTimes)
}
