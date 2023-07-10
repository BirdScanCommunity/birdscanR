#### addDayNightInfoPerEcho ---------------------------------------------------- 
#' @title addDayNightInfoPerEcho 
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}  
#' @description The function ‘addDayNightInfoPerEcho’ adds three columns 
#' ‘dayOrNight’, ''dayOrCrepOrNight' and ‘dateSunset’ to the echo data. This 
#' allows the user to filter echo data easily by “day” and “night”, or "day", 
#' "crepuscular", and "night". 
#' @param echoData dataframe with the echo data from the data list created by 
#' the function ‘extractDBData’ 
#' @param sunriseSunset dataframe with sunrise/sunset and civil twilight times 
#' created by the function ‘twilight’ 
#' @param sunOrCivil optional character variable, Set to “sun” to use 
#' sunrise/sunset times or to “civil” to use civil twilight times to group echoes 
#' into day/night. Default is "civil".
#' @param crepuscule optional character variable, Set to “nauticalSolar” to use 
#' the time between nautical dusk/dawn and sunrise/sunset times to define the 
#' crepuscular period, or to "nauticalCivil" to use the time between nautical 
#' and civil dusk/dawn to define the crepuscular period, or to "civilSolar" to use 
#' the time between civil dusk/dawn and sunrise/sunset times to define the 
#' crepuscular period. Default is "nauticalSolar".
#'
#' @return data frame with thre columns added, i.e. 'dayOrNight', 'dayOrCrepOrNight',
#'  and 'dateSunset'.
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
#'   sunOrCivil   = "civil"
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
#'                          sunOrCivil                   = sunOrCivil, 
#'                          crepuscule                     = "nauticalSolar")
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
#'                                     sunOrCivil  = "civil")   
#' }
#' 
addDayNightInfoPerEcho = function(echoData, 
                                  sunriseSunset, 
                                  sunOrCivil = "civil", 
                                  crepuscule   = "nauticalSolar"){
  # Check whether echoData contains any data
  # ===========================================================================
    if (nrow(echoData) == 0){
      stop(paste0("No echoData, can't add day/night information ", 
                  "(function: addDayNightInfoPerEcho)"))
    }
  
  # Add the columns 'dayOrNight', 'dayOrCrepOrNight' and 'dateSunset' to echoData
  # ===========================================================================
    echoData = data.frame(echoData, 
                          dayOrNight       = NA_character_, 
                          dayOrCrepOrNight = NA_character_,
                          dateSunset       = as.POSIXct(NA))
  
  # Get day and night periods
  # ===========================================================================
    if (sunOrCivil %in% "sun"){
      days   = data.frame(start = sunriseSunset$sunStart[sunriseSunset$is_night == 0], 
                          stop  = sunriseSunset$sunStop[sunriseSunset$is_night == 0])
      nights = data.frame(start = sunriseSunset$sunStart[sunriseSunset$is_night == 1], 
                          stop  = sunriseSunset$sunStop[sunriseSunset$is_night == 1])
    } else {
      days   = data.frame(start = sunriseSunset$civilStart[sunriseSunset$is_night == 0], 
                          stop  = sunriseSunset$civilStop[sunriseSunset$is_night == 0])
      nights = data.frame(start = sunriseSunset$civilStart[sunriseSunset$is_night == 1], 
                          stop  = sunriseSunset$civilStop[sunriseSunset$is_night == 1])
    }
  
  # Set echoes during day
  # ===========================================================================
    for (i in 1:length(days[, 1])){
      echoData$dayOrNight[echoData$time_stamp_targetTZ >= days$start[i] & echoData$time_stamp_targetTZ < days$stop[i]] = "day"
      echoData$dateSunset[echoData$time_stamp_targetTZ >= days$start[i] & echoData$time_stamp_targetTZ < days$stop[i]] = days$stop[i]
    }
    
  # Set echoes during night
  # ===========================================================================
    for (i in 1:length(nights[, 1])){
      echoData$dayOrNight[echoData$time_stamp_targetTZ >= nights$start[i] & echoData$time_stamp_targetTZ < nights$stop[i]] = "night"
      echoData$dateSunset[echoData$time_stamp_targetTZ >= nights$start[i] & echoData$time_stamp_targetTZ < nights$stop[i]] = nights$start[i]
    }
  
  # Get day, crepuscular, and night periods
  # ===========================================================================
    if (crepuscule %in% "civilSolar"){
      days              = data.frame(start = sunriseSunset$sunStart[sunriseSunset$is_night == 0], 
                                     stop  = sunriseSunset$sunStop[sunriseSunset$is_night == 0])
      crepusculeMorning = data.frame(start = sunriseSunset$civilStart[sunriseSunset$is_night == 0], 
                                     stop  = sunriseSunset$sunStart[sunriseSunset$is_night == 0])
      crepusculeEvening = data.frame(start = sunriseSunset$sunStart[sunriseSunset$is_night == 1], 
                                     stop  = sunriseSunset$civilStart[sunriseSunset$is_night == 1])
      nights            = data.frame(start = sunriseSunset$civilStart[sunriseSunset$is_night == 1], 
                                     stop  = sunriseSunset$civilStop[sunriseSunset$is_night == 1])
    } else if (crepuscule %in% "nauticalCivil"){
      days              = data.frame(start = sunriseSunset$civilStart[sunriseSunset$is_night == 0], 
                                     stop  = sunriseSunset$civilStop[sunriseSunset$is_night == 0])
      crepusculeMorning = data.frame(start = sunriseSunset$nauticalStart[sunriseSunset$is_night == 0], 
                                     stop  = sunriseSunset$civilStart[sunriseSunset$is_night == 0])
      crepusculeEvening = data.frame(start = sunriseSunset$civilStart[sunriseSunset$is_night == 1], 
                                     stop  = sunriseSunset$nauticalStart[sunriseSunset$is_night == 1])
      nights            = data.frame(start = sunriseSunset$nauticalStart[sunriseSunset$is_night == 1], 
                                     stop  = sunriseSunset$nauticalStop[sunriseSunset$is_night == 1])
    } else { # default 'nauticalSolar' option
      days              = data.frame(start = sunriseSunset$sunStart[sunriseSunset$is_night == 0], 
                                     stop  = sunriseSunset$sunStop[sunriseSunset$is_night == 0])
      crepusculeMorning = data.frame(start = sunriseSunset$nauticalStart[sunriseSunset$is_night == 0], 
                                     stop  = sunriseSunset$sunStart[sunriseSunset$is_night == 0])
      crepusculeEvening = data.frame(start = sunriseSunset$sunStart[sunriseSunset$is_night == 1], 
                                     stop  = sunriseSunset$nauticalStart[sunriseSunset$is_night == 1])
      nights            = data.frame(start = sunriseSunset$nauticalStart[sunriseSunset$is_night == 1], 
                                     stop  = sunriseSunset$nauticalStop[sunriseSunset$is_night == 1])
    }
  
  # Set echoes during day
  # ===========================================================================
    for (i in 1:length(days[, 1])){
      echoData$dayOrCrepOrNight[echoData$time_stamp_targetTZ >= days$start[i] & echoData$time_stamp_targetTZ < days$stop[i]] = "day"
    }
    
  # Set echoes during crepuscular
  # ===========================================================================
    for (i in 1:length(crepusculeMorning[, 1])){
      echoData$dayOrCrepOrNight[echoData$time_stamp_targetTZ >= crepusculeMorning$start[i] & echoData$time_stamp_targetTZ < crepusculeMorning$stop[i]] = "crepusculeMorning"
    }
    for (i in 1:length(crepusculeEvening[, 1])){
      echoData$dayOrCrepOrNight[echoData$time_stamp_targetTZ >= crepusculeEvening$start[i] & echoData$time_stamp_targetTZ < crepusculeEvening$stop[i]] = "crepusculeEvening"
    }
    
  # set echoes during night
  # ===========================================================================
    for (i in 1:length(nights[, 1])){
      echoData$dayOrCrepOrNight[echoData$time_stamp_targetTZ >= nights$start[i] & echoData$time_stamp_targetTZ < nights$stop[i]] = "night"
    }
    
  # return the output
  # ===========================================================================
    return(echoData)
}
