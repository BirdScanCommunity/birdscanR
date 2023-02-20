#### addDayNightInfoPerEcho ---------------------------------------------------- 
#' @title addDayNightInfoPerEcho 
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}  
#' @description The function ‘addDayNightInfoPerEcho’ adds the two columns 
#' ‘dayOrNight’ and ‘dateSunset’ to the echo data. This information is not 
#' used any further by the script, but allows the user to filter echo data 
#' easily by “day” and “night”. 
#' @param echoData dataframe with the echo data from the data list created by 
#' the function ‘extractDBData’ 
#' @param sunriseSunset dataframe with sunrise/sunset and civil twilight times 
#' created by the function ‘twilight’ 
#' @param sunOrCivil optional character string, “sun” (sunrise/sunset times) or 
#' “civil” (civil twilight times) to group by day and night. Default is "civil". 
#'
#' @return data frame with two columns added for dayOrNight and dateSunset.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set server, database, and other input settings for data extraction
#' # ===========================================================================
#'   dbServer       = "MACHINE\\SERVERNAME" # Set the name of your SQL server
#'   dbName         = "db_Name"               # Set the name of your database
#'   dbDriverChar   = "SQL Server"            # Set either "SQL Server" or "PostgreSQL"
#'   mainOutputDir  = file.path(".", "results")
#'   radarTimeZone  = "Etc/GMT0"
#'   targetTimeZone = "Etc/GMT0"
#'   listOfRfFeaturesToExtract = c(167, 168)
#'   siteLocation   = c(47.494427, 8.716432)
#'   sunOrCivil     = "civil"
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
#'                          sunOrCivil                     = sunOrCivil)
#'                          
#' # Get sunrise/sunset information
#' # ===========================================================================
#'   sunrisesunset = twilight(timeRange = c("2021-01-15 00:00", 
#'                                          "2021-01-31 00:00"),
#'                            latLon    = siteLocation,
#'                            timeZone  = targetTimeZone)
#' 
#' # Add day/night info to echo data
#' # ===========================================================================
#'   echoData = addDayNightInfoPerEcho(echoData      = dbData$echoData,
#'                                     sunriseSunset = pulseLengthSelection, 
#'                                     sunOrCivil    = "civil")   
#' }
#' 
addDayNightInfoPerEcho = function(echoData, sunriseSunset, sunOrCivil = "civil"){
  # Check whether echoData contains any data
  # ===========================================================================
    if (nrow(echoData) == 0){
      stop(paste0("No echoData, can't add day/night information ", 
                  "(function: addDayNightInfoPerEcho)"))
    }
  
  # add columns 'dayOrNight' and 'dateSunset' to echoData
  # ===========================================================================
    echoData = data.frame(echoData, 
                          dayOrNight = NA, 
                          dateSunset = as.POSIXct(NA))
  
  # get day and night periods
  # ===========================================================================
    if (sunOrCivil == "sun"){
      days   = data.frame(start = sunriseSunset$sunStart[sunriseSunset$is_night == 0], 
                          stop = sunriseSunset$sunStop[sunriseSunset$is_night == 0])
      nights = data.frame(start = sunriseSunset$sunStart[sunriseSunset$is_night == 1], 
                          stop = sunriseSunset$sunStop[sunriseSunset$is_night == 1])
    } else {
      days   = data.frame(start = sunriseSunset$civilStart[sunriseSunset$is_night == 0], 
                          stop = sunriseSunset$civilStop[sunriseSunset$is_night == 0])
      nights = data.frame(start = sunriseSunset$civilStart[sunriseSunset$is_night == 1], 
                          stop = sunriseSunset$civilStop[sunriseSunset$is_night == 1])
    }
  
  # set echoes during day
  # ===========================================================================
    for (i in 1:length(days[, 1])){
      echoData$dayOrNight[echoData$time_stamp_targetTZ >= days$start[i] & echoData$time_stamp_targetTZ < days$stop[i]] = "day"
      echoData$dateSunset[echoData$time_stamp_targetTZ >= days$start[i] & echoData$time_stamp_targetTZ < days$stop[i]] = days$stop[i]
    }
    
  # set echoes during night
  # ===========================================================================
    for (i in 1:length(nights[, 1])){
      echoData$dayOrNight[echoData$time_stamp_targetTZ >= nights$start[i] & echoData$time_stamp_targetTZ < nights$stop[i]] = "night"
      echoData$dateSunset[echoData$time_stamp_targetTZ >= nights$start[i] & echoData$time_stamp_targetTZ < nights$stop[i]] = nights$start[i]
    }
  
  # return the output
  # ===========================================================================
    return(echoData)
  
}
