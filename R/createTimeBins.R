#### createTimeBins ------------------------------------------------------
#' @title createTimeBins
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}  
#' @description Create time bins with a given duration. Time bins expanding over 
#' a day/night change will be split in two time bins.
#'
#' @param timeRange vector of length 2, with start and end of time range as 
#' POSIXct
#' @param timeBinDuration_sec duration of timeBins in seconds (numeric). for 
#' values <= 0 a duration of 1 hour will be set
#' @param timeZone time zone in which the time bins should be created as string, 
#' e.g. "Etc/GMT0"
#' @param sunriseSunset dataframe with sunrise/sunset, civil dawn/dusk. computed 
#' with function 'twilight'
#' @param dnBins Logical. Default TRUE. Determines whether timebins based on 
#' day/night values (determined by the parameter 'sunOrCivil') are created. 
#' @param crepBins Logical. Default FALSE. Determines whether timebins with 
#'  crepuscular time phases are created (determined by the parameter 'crepuscule'). 
#' @param sunOrCivil sunrise/sunset or civil dawn/dusk used to split day and 
#' night. Supported values: "sun" or "civil", default: "civil"
#' @param crepuscule Used to split into crepusculeMorning, day, crepusculeEvening, 
#' and night. Set to “nauticalSolar” to use the time between nautical dusk/dawn 
#' and sunrise/sunset times to define the crepuscular period, or to 
#' "nauticalCivil" to use the time between nautical and civil dusk/dawn to define 
#' the crepuscular period, or to "civilSolar" to use the time between civil 
#' dusk/dawn and sunrise/sunset times to define the crepuscular period. Default 
#' is "nauticalSolar".
#'
#' @return returns a dataframe with the time bins information
#' 
createTimeBins = function(timeRange, 
                          timeBinDuration_sec, 
                          timeZone, 
                          sunriseSunset, 
                          dnBins   = TRUE,
                          crepBins = FALSE,
                          sunOrCivil = "civil",
                          crepuscule = "nauticalSolar"){
# Check whether input are ok
# =============================================================================
  if (timeRange[1] > timeRange[2]){
    warning(paste0("End of time range is before begin of time range. Set a ", 
                   "valid time range to create time bins."))  
    return()
  } 
  if (timeBinDuration_sec <= 0){
    timeBinDuration_sec = 60 * 60
  }
  if (dnBins & crepBins){
    stop(paste0("Please set only one of the options 'dnBins' or ", 
                "'crepBins' to TRUE, and rerun createTimeBins()."))
  }
  
# Create day/night timeBins, if requested
# =============================================================================
  if (dnBins){
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
    timeBinsDN$dayOrNight[timeBinsDN$dayOrNight == 1] = "night"
    timeBinsDN$dayOrNight[timeBinsDN$dayOrNight == 0] = "day"
  }
  
# Create crepuscule timeBins
# =============================================================================
  if (crepBins){
    if (crepuscule %in% "civilSolar"){
      timeBinsCrep = data.frame(start      = sunriseSunset$civilStart[sunriseSunset$is_night == 0],
                                    stop       = sunriseSunset$sunStart[sunriseSunset$is_night == 0],
                                    dielPhase  = "crepusculeMorning",
                                    dateSunset = sunriseSunset$date[sunriseSunset$is_night == 0])
      tmpBinsCrep  = data.frame(start      = sunriseSunset$sunStart[sunriseSunset$is_night == 0],
                                stop       = sunriseSunset$sunStop[sunriseSunset$is_night == 0],
                                dielPhase  = "day",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 0])
      timeBinsCrep = rbind(timeBinsCrep, tmpBinsCrep)
      tmpBinsCrep  = data.frame(start      = sunriseSunset$sunStart[sunriseSunset$is_night == 1],
                                stop       = sunriseSunset$civilStart[sunriseSunset$is_night == 1],
                                dielPhase  = "crepusculeEvening",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 1])
      timeBinsCrep = rbind(timeBinsCrep, tmpBinsCrep)
      tmpBinsCrep  = data.frame(start      = sunriseSunset$civilStart[sunriseSunset$is_night == 1],
                                stop       = sunriseSunset$civilStop[sunriseSunset$is_night == 1],
                                dielPhase  = "night",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 1])
      timeBinsCrep = rbind(timeBinsCrep, tmpBinsCrep)
  
    } else if (crepuscule %in% "nauticalCivil"){
      timeBinsCrep = data.frame(start      = sunriseSunset$nauticalStart[sunriseSunset$is_night == 0],
                                stop       = sunriseSunset$civilStart[sunriseSunset$is_night == 0],
                                dielPhase  = "crepusculeMorning",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 0])
      tmpBinsCrep  = data.frame(start      = sunriseSunset$civilStart[sunriseSunset$is_night == 0],
                                stop       = sunriseSunset$civilStop[sunriseSunset$is_night == 0],
                                dielPhase  = "day",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 0])
      timeBinsCrep = rbind(timeBinsCrep, tmpBinsCrep)
      tmpBinsCrep  = data.frame(start      = sunriseSunset$civilStart[sunriseSunset$is_night == 1],
                                stop       = sunriseSunset$nauticalStart[sunriseSunset$is_night == 1],
                                dielPhase  = "crepusculeEvening",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 1])
      timeBinsCrep = rbind(timeBinsCrep, tmpBinsCrep)
      tmpBinsCrep  = data.frame(start      = sunriseSunset$nauticalStart[sunriseSunset$is_night == 1],
                                stop       = sunriseSunset$nauticalStop[sunriseSunset$is_night == 1],
                                dielPhase  = "night",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 1])
      timeBinsCrep = rbind(timeBinsCrep, tmpBinsCrep)
  
    } else {   # the default "nauticalSolar"
      timeBinsCrep = data.frame(start      = sunriseSunset$nauticalStart[sunriseSunset$is_night == 0],
                                stop       = sunriseSunset$sunStart[sunriseSunset$is_night == 0],
                                dielPhase  = "crepusculeMorning",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 0])
      tmpBinsCrep  = data.frame(start      = sunriseSunset$sunStart[sunriseSunset$is_night == 0],
                                stop       = sunriseSunset$sunStop[sunriseSunset$is_night == 0],
                                dielPhase  = "day",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 0])
      timeBinsCrep = rbind(timeBinsCrep, tmpBinsCrep)
      tmpBinsCrep  = data.frame(start      = sunriseSunset$sunStart[sunriseSunset$is_night == 1],
                                stop       = sunriseSunset$nauticalStart[sunriseSunset$is_night == 1],
                                dielPhase  = "crepusculeEvening",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 1])
      timeBinsCrep = rbind(timeBinsCrep, tmpBinsCrep)
      tmpBinsCrep  = data.frame(start      = sunriseSunset$nauticalStart[sunriseSunset$is_night == 1],
                                stop       = sunriseSunset$nauticalStop[sunriseSunset$is_night == 1],
                                dielPhase  = "night",
                                dateSunset = sunriseSunset$date[sunriseSunset$is_night == 1])
      timeBinsCrep = rbind(timeBinsCrep, tmpBinsCrep)
    }
    
    # Fix twilight timing NA values occuring at higher latitudes
    # NOTE: THIS WAS SO FAR ONLY TESTED FOR NORTHERN HEMISPHERE SUMMER  
    #       SITUATIONS, NOT FOR WINTER NOR FOR SOUTHERN HEMISPHERE 
    #       LIKELY TO STILL NEED FURTHER FIXING FOR THESE SITUATIONS
    # =========================================================================
      # Sort all by dateSunset and factored dielPhase !!!!
      # =======================================================================
        timeBinsCrep$dielPhase = factor(timeBinsCrep$dielPhase, 
                                        levels = c("crepusculeMorning",
                                                   "day", 
                                                   "crepusculeEvening",
                                                   "night"))
        timeBinsCrep = timeBinsCrep[order(timeBinsCrep$dateSunset, timeBinsCrep$dielPhase),]  

      # Find the lines with na for start and stop
      # =======================================================================
        rowsDoubleNA = which((is.na(timeBinsCrep$start)) & (is.na(timeBinsCrep$stop)))
        if (length(rowsDoubleNA) != 0){
          # Update the stop in the line before with the mean of the start of the 
          # line before and the stop after
          # ===================================================================
            if (1 %in% rowsDoubleNA){
          	  timeBinsCrep = timeBinsCrep[2:nrow(timeBinsCrep), ]
          	  rowsDoubleNA = rowsDoubleNA[2:length(rowsDoubleNA)]
          	}
            timeBinsCrep$stop[(rowsDoubleNA-1)]   = as.POSIXct(rowMeans(data.frame(as.numeric(timeBinsCrep$start[(rowsDoubleNA-1)]), 
                                                                                   as.numeric(timeBinsCrep$stop[(rowsDoubleNA+1)]))), 
                                                               tz = "ETC/GMT0")
            
          # Update the start of the line after with the mean of the start of the 
          # line before and the stop after
          # ===================================================================
            if (nrow(timeBinsCrep) %in% rowsDoubleNA){
              timeBinsCrep = timeBinsCrep[1:(nrow(timeBinsCrep)-1), ]
              rowsDoubleNA = rowsDoubleNA[1:(length(rowsDoubleNA)-1)]
            }
            timeBinsCrep$start[(rowsDoubleNA+1)]  = as.POSIXct(rowMeans(data.frame(as.numeric(timeBinsCrep$start[(rowsDoubleNA-1)]), 
                                                                                   as.numeric(timeBinsCrep$stop[(rowsDoubleNA+1)]))), 
                                                               tz = "ETC/GMT0")
            
          # Delete the lines with na for start and stop
          # ===================================================================
            timeBinsCrep = timeBinsCrep[-rowsDoubleNA,]
        }
      
      # Find the lines for which the stop is na and the start of the next line 
      #  is NA
      # =======================================================================
        rowsStopNAStartPlusOneNA = which(is.na(timeBinsCrep$stop))
        if (nrow(timeBinsCrep) %in% rowsStopNAStartPlusOneNA){
          rowsStopNAStartPlusOneNA = rowsStopNAStartPlusOneNA[-(length(rowsStopNAStartPlusOneNA))]
          timeBinsCrep             = timeBinsCrep[1:(nrow(timeBinsCrep)-1), ]
        }
        if (length(rowsStopNAStartPlusOneNA) != 0){
          # Update the stop of the line with the mean of the start of the line 
          #  and the stop of the next line
          # ===================================================================
            timeBinsCrep$stop[rowsStopNAStartPlusOneNA] = as.POSIXct(rowMeans(data.frame(as.numeric(timeBinsCrep$start[rowsStopNAStartPlusOneNA]), 
                                                                                         as.numeric(timeBinsCrep$stop[(rowsStopNAStartPlusOneNA+1)]))), 
                                                                     tz = "ETC/GMT0")
            
          # Update the start of the next line with the mean of the start of the 
          #  line and the stop of the next line
          # ===================================================================
            timeBinsCrep$start[(rowsStopNAStartPlusOneNA+1)]  = as.POSIXct(rowMeans(data.frame(as.numeric(timeBinsCrep$start[(rowsStopNAStartPlusOneNA)]), 
                                                                                               as.numeric(timeBinsCrep$stop[(rowsStopNAStartPlusOneNA+1)]))), 
                                                                           tz = "ETC/GMT0")
        }
  }
  
# Limit timeBins to timeRange
# =============================================================================
  if (dnBins){
    timeBinsDN = timeBinsDN[(timeBinsDN$start >= timeRange[1]) &
                            (timeBinsDN$start <= timeRange[2]),]
  } else if (crepBins){
    timeBinsCrep = timeBinsCrep[(timeBinsCrep$start >= timeRange[1]) &
                                (timeBinsCrep$start <= timeRange[2]),]
  }
  

# Create timeBins with the requested timeBinDuration
# =============================================================================
  timeStart = timeRange[1] - 
              (as.numeric(format(timeRange[1], "%M"))*60) - 
              (as.numeric(format(timeRange[1], "%S"))) - 
              (60*60*24)
  timeStop  = timeRange[2] + (60*60*24)
  sequence  = seq(timeStart, timeStop, by = timeBinDuration_sec)
  if (dnBins){
    timeBins    = data.frame(start            = sequence[1:(length(sequence)-1)], 
                             stop             = sequence[2:length(sequence)], 
                             dayOrNight       = NA,
                             dateSunset       = as.POSIXct(NA, tz = timeZone))
  } else if (crepBins){
    timeBins  = data.frame(start            = sequence[1:(length(sequence)-1)], 
                           stop             = sequence[2:length(sequence)], 
                           dielPhase        = NA_character_,
                           dateSunset       = as.POSIXct(NA, tz = timeZone))
  }
  timeBins                             = timeBins[(timeBins$stop > timeRange[1]) & 
                                                  (timeBins$start < timeRange[2]),]
  timeBins$start[1]                    = timeRange[1]
  timeBins$stop[length(timeBins[, 1])] = timeRange[2]
  
# Remove timeBins with same starttime as any startime in timeBinsCrep or timeBinsDN
# =============================================================================
  if (dnBins){
    timeBins   = timeBins[!timeBins$start %in% timeBinsDN$start,]         
  } else if (crepBins){
    timeBins = timeBins[!timeBins$start %in% timeBinsCrep$start,]
  }
  
# Combine timeBins with crepuscular/dn timebins
# =============================================================================
  if (dnBins){
    timeBins = rbind(timeBinsDN, timeBins)
  } else if (crepBins){
    timeBins = rbind(timeBinsCrep, timeBins)
  }
  
# Sort timeBins by start time
# =============================================================================
  timeBins   = timeBins[order(timeBins$start),]
  
# Make sure all timebins are sequentially timed
# =============================================================================
  if (dnBins){
    for (i in 1:nrow(timeBins)){
      # adjust stop times
      # =======================================================================
        if (i < nrow(timeBins)){
          if (timeBins$stop[i] != timeBins$start[i + 1]){
            timeBins$stop[i] = timeBins$start[i + 1]
          }
        }
    }
  } else if (crepBins){
    for (i in 1:nrow(timeBins)){
      # adjust stop times
      # =======================================================================
        if (i < nrow(timeBins)){
          if (timeBins$stop[i] != timeBins$start[i + 1]){
            if (timeBins$stop[i] > timeBins$stop[i + 1]){
              timeBins$stop[i+1] = timeBins$stop[i]
              timeBins$stop[i] = timeBins$start[i + 1]
            } else {
              timeBins$stop[i] = timeBins$start[i + 1]
            }
          }
        }
    }
  }
  
# Get night and day times, if requested
# =============================================================================
  if (dnBins){
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
  }
  
# assign day/night to timeBins based on meantime of timebin, if dnBins
# =============================================================================
  if (dnBins){
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
  }
 
# Get day, crepuscular, and night periods, if requested
# =============================================================================
  if (crepBins){
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
  }
   
# assign crepuscular phase to timeBins based on meantime of timebin, 
#  if crepBins == TRUE
# =============================================================================
  if (crepBins){
    timeBinsMean = timeBins$start + difftime(timeBins$stop, timeBins$start, "secs") / 2
    isCrepMorning = vapply(timeBinsMean, 
                     function(x) {x >= crepusculeMorning$start & x < crepusculeMorning$stop}, 
                     logical(nrow(crepusculeMorning)))
    isCrepMorning = colSums(isCrepMorning, na.rm = TRUE)
    isCrepEvening = vapply(timeBinsMean, 
                     function(x) {x >= crepusculeEvening$start & x < crepusculeEvening$stop}, 
                     logical(nrow(crepusculeEvening)))
    isCrepEvening = colSums(isCrepEvening, na.rm = TRUE)
    isNight = vapply(timeBinsMean, 
                     function(x) {x >= nights$start & x < nights$stop}, 
                     logical(nrow(nights)))
    isNight = colSums(isNight, na.rm = TRUE)
    isDay   = vapply(timeBinsMean, 
                     function(x) {x >= days$start & x < days$stop}, 
                     logical(nrow(days))) 
    isDay   = colSums(isDay, na.rm = TRUE)
    timeBins$dielPhase[as.logical(isCrepMorning)] = "crepusculeMorning"
    timeBins$dielPhase[as.logical(isCrepEvening)] = "crepusculeEvening"
    timeBins$dielPhase[as.logical(isNight)] = "night"
    timeBins$dielPhase[as.logical(isDay)]   = "day"
    
    # Complete the values for dielPhase where currently NA (fix for locations 
    #  at higher latitudes)
    # =========================================================================
      for (cRow in 2:nrow(timeBins)){
        if (is.na(timeBins$dielPhase[cRow])){
          timeBins$dielPhase[cRow]= timeBins$dielPhase[cRow-1]
        }
      }
  }
  
# Set dateSunset
# =============================================================================
  if (crepBins){
    if (timeBins$dielPhase[!is.na(timeBins$dielPhase)][1] == "day"){
      dateSunset = min(timeBins$dateSunset, na.rm = TRUE)
    } else {
      dateSunset = min(timeBins$dateSunset, na.rm = TRUE) - (60*60*24)
    }
    for (i in 1:nrow(timeBins)){
      # set 'dateSunset'
      # =========================================================================
        if (!is.na(timeBins$dateSunset[i])){
          dateSunset = timeBins$dateSunset[i]
        } else if (!is.na(timeBins$dielPhase[i])){
          timeBins$dateSunset[i] = dateSunset  
        }
    }
  } else {
    if (timeBins$dayOrNight[!is.na(timeBins$dayOrNight)][1] == "day"){
      dateSunset = min(timeBins$dateSunset, na.rm = TRUE)
    } else {
      dateSunset = min(timeBins$dateSunset, na.rm = TRUE) - (60*60*24)
    }
    for (i in 1:nrow(timeBins)){
      # set 'dateSunset'
      # =========================================================================
        if (!is.na(timeBins$dateSunset[i])){
          dateSunset = timeBins$dateSunset[i]
        } else if (!is.na(timeBins$dayOrNight[i])){
          timeBins$dateSunset[i] = dateSunset  
        }
    }
  }
  
# add 'date' column to timeBins 
# =============================================================================
  timeBins      = data.frame(date = NA, timeBins)
  timeBins$date = as.Date(timeBins$start , tz = timeZone)
  
# add 'id' column to timeBins
# =============================================================================
  timeBins = data.frame(id = seq(1, length(timeBins[, 1]), by = 1), timeBins)
  
# add 'duration_sec' column to timeBins
# =============================================================================
  timeBins              = data.frame(timeBins, duration_sec = NA)
  timeBins$duration_sec = as.numeric(difftime(timeBins$stop, timeBins$start, 
                                                units = "secs"))
  row.names(timeBins) <- timeBins$id
  
# Return output
# =============================================================================
  return(timeBins)
}
