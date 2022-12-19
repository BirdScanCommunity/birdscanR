#### createTimeBins ------------------------------------------------------
#' @title createTimeBins
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest, \email{birgen.haest@@vogelwarte.ch}  
#' @description Create timebins with a given duration. timebins expanding over a day/night change will be split in two timebins.
#'
#' @param timeRange vector of length 2, with start and end of timerange as POSIXct
#' @param timeBinDuration_sec duration of timeBins in seconds (numeric). for values <= 0 a duration of 1 hour will be set
#' @param timeZone timezone in which the timebins should be created as string. e.g. "Etc/GMT0"
#' @param sunriseSunset dataframe with sunrise/sunset, civil dawn/dusk. computed with function 'twilight'
#' @param sunOrCivil sunrise/sunset or civil dawn/dusk used to split day and night. Supported values: "sun" or "civil", default: "civil"
#'
#' @return returns a dataframe with the timebins information
#' 
createTimeBins = function(timeRange, 
                          timeBinDuration_sec, 
                          timeZone, 
                          sunriseSunset, 
                          sunOrCivil = "civil"){
# Check whether input are ok
# =============================================================================
  if (timeRange[1] > timeRange[2]){
    warning("End of timerange is before begin of timerange. Set a valid timerange to create timebins.")  
    return()
  } 
  if (timeBinDuration_sec <= 0){
    timeBinDuration_sec = 60 * 60
  }
  
# Create day/night timeBins
# =============================================================================
  if (sunOrCivil == "sun"){
    timeBinsDN = data.frame(start      = sunriseSunset$sunStart, 
                            stop       = sunriseSunset$sunStop, 
                            dayOrNight = sunriseSunset$is_night, 
                            dateSunset = sunriseSunset$date)
  } else {
    timeBinsDN = data.frame(start      = sunriseSunset$civilStart, 
                            stop       = sunriseSunset$civilStop, 
                            dayOrNight = sunriseSunset$is_night, 
                            dateSunset = sunriseSunset$date)
  }
  
# Limit day/night timeBins to timeRange
# =============================================================================
  timeBinsDN = timeBinsDN[(timeBinsDN$start >= timeRange[1]) & 
                          (timeBinsDN$start <= timeRange[2]),]
  timeBinsDN$dayOrNight[timeBinsDN$dayOrNight == 1] = "night"
  timeBinsDN$dayOrNight[timeBinsDN$dayOrNight == 0] = "day"
  
# Create timeBins
# =============================================================================
  timeStart = timeRange[1] - 
              (as.numeric(format(timeRange[1], "%M"))*60) - 
              (as.numeric(format(timeRange[1], "%S"))) - 
              (60*60*24)
  timeStop  = timeRange[2] + (60*60*24)
  sequence  = seq(timeStart, timeStop, by = timeBinDuration_sec)
  timeBins  = data.frame(start      = sequence[1:(length(sequence)-1)], 
                         stop       = sequence[2:length(sequence)], 
                         dayOrNight = NA, 
                         dateSunset = as.POSIXct(NA, tz = timeZone))
  timeBins  = timeBins[(timeBins$stop > timeRange[1]) & 
                       (timeBins$start < timeRange[2]),]
  timeBins$start[1]                    = timeRange[1]
  timeBins$stop[length(timeBins[, 1])] = timeRange[2]
  
# Remove timeBins with same starttime as any startime in timeBinsDN
# =============================================================================
  timeBins = timeBins[!timeBins$start %in% timeBinsDN$start,]
  
# Combine timeBins
# =============================================================================
  timeBins = rbind(timeBinsDN, timeBins)
  
# Sort timeBins by start time
# =============================================================================
  timeBins = timeBins[order(timeBins$start),]
  
  for (i in 1:(length(timeBins[, 1]))){
    # adjust stop times
    if (i < length(timeBins[, 1])){
      if (timeBins$stop[i] != timeBins$start[i + 1]){
        timeBins$stop[i] = timeBins$start[i + 1]
      }
    }
  }
  
# Get night and day times
# =============================================================================
  if (sunOrCivil == "sun"){
    nightTimes = data.frame(start = sunriseSunset[sunriseSunset$is_night == 1,]$sunStart, 
                            stop  = sunriseSunset[sunriseSunset$is_night == 1,]$sunStop)
    dayTimes   = data.frame(start = sunriseSunset[sunriseSunset$is_night == 0,]$sunStart, 
                            stop  = sunriseSunset[sunriseSunset$is_night == 0,]$sunStop)
  } else if (sunOrCivil == "civil"){
    nightTimes = data.frame(start = sunriseSunset[sunriseSunset$is_night == 1,]$civilStart, 
                            stop  = sunriseSunset[sunriseSunset$is_night == 1,]$civilStop)
    dayTimes   = data.frame(start = sunriseSunset[sunriseSunset$is_night == 0,]$civilStart, 
                            stop  = sunriseSunset[sunriseSunset$is_night == 0,]$civilStop)
  } else {
    nightTimes = data.frame(start = sunriseSunset[sunriseSunset$is_night == 1,]$sunStart, 
                            stop  = sunriseSunset[sunriseSunset$is_night == 1,]$sunStop)
    dayTimes   = data.frame(start = sunriseSunset[sunriseSunset$is_night == 0,]$sunStart, 
                            stop  = sunriseSunset[sunriseSunset$is_night == 0,]$sunStop)
  }
  sunrise = dayTimes$start
  sunset  = dayTimes$stop
  
# assign day/night to timeBins based on meantime of timebin
# =============================================================================
  timeBinsMean = timeBins$start + difftime(timeBins$stop, timeBins$start, "secs") / 2
  
  isNight = vapply(timeBinsMean, 
                   function(x) {x >= nightTimes$start & x < nightTimes$stop}, 
                   logical(length(nightTimes[, 1])))
  isNight = colSums(isNight)
  isDay   = vapply(timeBinsMean, 
                   function(x) {x >= dayTimes$start & x < dayTimes$stop}, 
                   logical(length(dayTimes[, 1]))) 
  isDay   = colSums(isDay)
  
  timeBins$dayOrNight[as.logical(isNight)] = "night"
  timeBins$dayOrNight[as.logical(isDay)]   = "day"
  
# set dateSunset
# =============================================================================
  if (timeBins$dayOrNight[!is.na(timeBins$dayOrNight)][1] == "day"){
    dateSunset = min(timeBins$dateSunset, na.rm = TRUE)
  } else {
    dateSunset = min(timeBins$dateSunset, na.rm = TRUE) - (60*60*24)
  }
  for (i in 1:(length(timeBins[, 1]))){
    # set 'dateSunset'
    if (!is.na(timeBins$dateSunset[i])){
      dateSunset = timeBins$dateSunset[i]
    } else if (!is.na(timeBins$dayOrNight[i])){
      timeBins$dateSunset[i] = dateSunset  
    }
  }
  
# add 'date' column to timeBins
# =============================================================================
  timeBins      = data.frame(date = NA, timeBins)
  timeBins$date = as.Date(timeBins$start , tz = timeZone)
  
# add 'id' column to timBins
# =============================================================================
  timeBins = data.frame(id = seq(1, length(timeBins[, 1]), by = 1), timeBins)
  
# add 'duration_sec' column to timeBins
# =============================================================================
  timeBins              = data.frame(timeBins, duration_sec = NA)
  timeBins$duration_sec = as.numeric(difftime(timeBins$stop, timeBins$start, units = "secs"))
  row.names = timeBins$id
  
# Return output
# =============================================================================
  return(timeBins)
}

#createTimeBins(timeRange = timeRangeTimeBins, sunriseSunset = sunriseSunset, timeBinDuration_sec = timeBinduration_sec, sunOrCivil = sunOrCivil, timeZone = targetTimeZone)
#createTimeBins(timeRange = timeRangeTimeBins, sunriseSunset = sunriseSunset, timeBinDuration_sec = timeBinduration_sec, sunOrCivil = sunOrCivil, timeZone = targetTimeZone)