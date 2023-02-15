#### twilight -----------------------------------------------------------------
#' @title Calculate the twilight hours for given locations
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @description  Calculate the  duration of night in hours and seconds. It calculate also the time of both solar and civil start of the day and end of the day.
#' 
#' @param timeRange The range of dates in which to calculate the twilight
#' @param latLon A list of X, Y coordinates 
#' @param crs_datum The coordinate reference system and datum of the X, Y coordinates
#' @param timeZone The timezone of the area of interest
#'
#' @return A data frame with the results
#' @export
#' @examples 
#' \dontrun{
#' sunrisesunset = twilight(timeRange = c("2021-01-15 00:00", "2021-01-31 00:00"),
#'                          latLon    = c(47.494427, 8.716432),
#'                          timezone  = Etc/GMT0")
#' }
twilight = function(timeRange, 
                    latLon, 
                    crs_datum = "WGS84", 
                    timeZone){
  dateSeq = seq(as.Date(timeRange[1], tz = timeZone) - 1, 
                as.Date(timeRange[2], tz = timeZone) + 2, 
                "day")
  lon_lat = data.frame(X = latLon[2], Y = latLon[1])
  crds    = sp::CRS(paste0("+proj=longlat +datum=", crs_datum)) # here i used GoogleEarth coodrinates that use the "WGS84" coordinate system
  lon_lat = sp::SpatialPoints(lon_lat, proj4string = crds) # here i used GoogleEarth coodrinates that use the "WGS84" coordinate system
  
  # make sure that date is in UTC
  # ===========================================================================
    dateSeq = format(as.POSIXct(strptime(x = dateSeq, "%Y-%m-%d", tz = "UTC")), 
                     format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE)
    dateSeq = as.POSIXct(strptime(x = dateSeq, "%Y-%m-%d %H:%M:%S", tz = timeZone))
  
  dawn = maptools::crepuscule(lon_lat, dateSeq, solarDep = 6, direction = "dawn", 
                              POSIXct.out = TRUE)$time # civil (see argument: "solarDep = 6") twilight at dawn for the day of observation
  dusk = maptools::crepuscule(lon_lat, dateSeq, solarDep = 6, direction = "dusk", 
                              POSIXct.out = TRUE)$time # civil twilight at dusk

  sunrise = maptools::sunriset(lon_lat, dateSeq, direction="sunrise", 
                               POSIXct.out = TRUE)$time
  sunset  = maptools::sunriset(lon_lat, dateSeq, direction="sunset", 
                               POSIXct.out = TRUE)$time
  
  twilightData = expand.grid("is_night" = 0:1, "date" = dateSeq, 
                             KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  
  # add 'sunStart', 'sunStop', civilStart' and 'civilStop' column to twilight
  # ===========================================================================
    twilightData = data.frame(twilightData, sunStart = NA, sunStop = NA, 
                              civilStart = NA, civilStop = NA)
  
  twilightData$sunStart = twilightData$sunStop = twilightData$civilStart = twilightData$civilStop = NA
  for(i in 1:length(dateSeq)){
    i.date = unique(dateSeq)[i]
    # sunrise/sunset
    twilightData$sunStart[twilightData$date == i.date & twilightData$is_night == 0] = 
      format(as.POSIXct(sunrise[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
    twilightData$sunStop[twilightData$date == i.date & twilightData$is_night == 0]  = 
      format(as.POSIXct(sunset[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
    twilightData$sunStart[twilightData$date == i.date & twilightData$is_night == 1] = 
      format(as.POSIXct(sunset[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
    twilightData$sunStop[twilightData$date == i.date & twilightData$is_night == 1]  = 
      format(as.POSIXct(sunrise[i + 1]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
    # Civil twilight
    twilightData$civilStart[twilightData$date == i.date & twilightData$is_night == 0] = 
      format(as.POSIXct(dawn[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
    twilightData$civilStop[twilightData$date == i.date & twilightData$is_night == 0]  = 
      format(as.POSIXct(dusk[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
    twilightData$civilStart[twilightData$date == i.date & twilightData$is_night == 1] = 
      format(as.POSIXct(dusk[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
    twilightData$civilStop[twilightData$date == i.date & twilightData$is_night == 1]  = 
      format(as.POSIXct(dawn[i + 1]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
  } # end of for-loop >>> for(i in 1:length(dateSeq)){...
  
  twilightData$sunStart   = as.POSIXct(twilightData$sunStart, format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
  twilightData$sunStop    = as.POSIXct(twilightData$sunStop, format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
  twilightData$civilStart = as.POSIXct(twilightData$civilStart, format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
  twilightData$civilStop  = as.POSIXct(twilightData$civilStop, format = "%Y-%m-%d %H:%M:%S", tz = timeZone)

  # omit rows with NA's
  twilightData = twilightData[!is.na(twilightData$sunStart) & !is.na(twilightData$sunStop) & !is.na(twilightData$civilStart) & !is.na(twilightData$civilStop),]
  
  twilightData$durationDayNight_h   = as.numeric(difftime(twilightData$sunStop, twilightData$sunStart, tz = timeZone, units = "hours"))
  twilightData$durationDayNight_sec = as.numeric(difftime(twilightData$sunStop, twilightData$sunStart, tz = timeZone, units = "secs"))
  
  return(twilightData)
  
} # end of twilight-function

#twilight(timeRange = timeRangeEchoData, latLon = siteLocation, timeZone = targetTimeZone)
