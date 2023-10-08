#### twilight -----------------------------------------------------------------
#' @title Get the nautical, civil, and solar dawn and dusk for a given timerange 
#' and locations.
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @description Get the time of nautical (sun at 12 degrees below horizon), civil 
#' (sun at 6 degrees below horizon) and solar (sun at 0 degrees below horizon) 
#' dawn and dusk for each day over a given time range.
#' @param timeRange A two-element character vector with elements of the form 
#' %Y-%m-%d defining the start and end of the timerange for which you want to 
#' get the twilight information.
#' @param latLon A list of X, Y coordinates 
#' @param crs_datum The coordinate reference system and datum of the X, Y 
#' coordinates. Default = "WGS84.
#' @param timeZone The time zone of the area of interest
#'
#' @return A data frame with the results
#' @export
#' @examples 
#' \dontrun{
#' sunrisesunset = twilight(timeRange = c("2021-01-15 00:00", 
#'                                        "2021-01-31 00:00"),
#'                          latLon    = c(47.494427, 8.716432),
#'                          timeZone  = "Etc/GMT0")
#' }
twilight = function(timeRange, 
                    latLon, 
                    crs_datum = "WGS84", 
                    timeZone){
  # Prepare date and location variables
  # ===========================================================================
    dateSeq = seq(as.Date(timeRange[1], tz = timeZone) - 1, 
                  as.Date(timeRange[2], tz = timeZone) + 2, 
                  "day")
    lon_lat = data.frame(X = latLon[2], Y = latLon[1])
    crds    = sp::CRS(paste0("+proj=longlat +datum=", crs_datum)) 
    lon_lat = sp::SpatialPoints(lon_lat, proj4string = crds) 
  
  # make sure that date is in UTC
  # ===========================================================================
    dateSeq = format(as.POSIXct(strptime(x = dateSeq, "%Y-%m-%d", tz = "UTC")), 
                     format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE)
    dateSeq = as.POSIXct(strptime(x = dateSeq, "%Y-%m-%d %H:%M:%S", tz = timeZone))
  
  # Get the nautical dawn and dusk
  # ===========================================================================
    nauticalDawn = suntools::crepuscule(crds        = lon_lat, 
                                        dateTime    = dateSeq, 
                                        solarDep    = 12, 
                                        direction   = "dawn", 
                                        POSIXct.out = TRUE)$time 
    nauticalDusk = suntools::crepuscule(crds        = lon_lat, 
                                        dateTime    = dateSeq, 
                                        solarDep    = 12, 
                                        direction   = "dusk", 
                                        POSIXct.out = TRUE)$time 
  
  # Get the civil dawn and dusk
  # ===========================================================================
    civilDawn = suntools::crepuscule(crds        = lon_lat, 
                                    dateTime    = dateSeq, 
                                    solarDep    = 6, 
                                    direction   = "dawn", 
                                    POSIXct.out = TRUE)$time  
    civilDusk = suntools::crepuscule(crds        = lon_lat, 
                                    dateTime    = dateSeq, 
                                    solarDep    = 6, 
                                    direction   = "dusk", 
                                    POSIXct.out = TRUE)$time 
    
  # Get the solar dawn and dusk
  # ===========================================================================
    sunrise = suntools::crepuscule(crds        = lon_lat, 
                                   dateTime    = dateSeq, 
                                   solarDep    = 0, 
                                   direction   = "dawn", 
                                   POSIXct.out = TRUE)$time  
    sunset  = suntools::crepuscule(crds        = lon_lat, 
                                   dateTime    = dateSeq, 
                                   solarDep    = 0, 
                                   direction   = "dusk", 
                                   POSIXct.out = TRUE)$time
  
  # Create output dataframe to hold all twilight information
  # ===========================================================================
    twilightData = expand.grid("is_night"       = 0:1, 
                               "date"           = dateSeq, 
                               KEEP.OUT.ATTRS   = FALSE, 
                               stringsAsFactors = FALSE)
    twilightData$sunStart      = NA 
    twilightData$sunStop       = NA 
    twilightData$civilStart    = NA 
    twilightData$civilStop     = NA
    twilightData$nauticalStart = NA 
    twilightData$nauticalStop  = NA
  
  # Update daily 'sunStart', 'sunStop', civilStart', 'civilStop', 'nauticalStart', and 
  #  'nauticalStop' values in the output twilight dataframe
  # ===========================================================================
    for(i in 1:length(dateSeq)){
      # Get the current date
      # =======================================================================
        # i.date = unique(dateSeq)[i]
        cDate  = dateSeq[i]
      
      # update sunrise/sunset
      # =======================================================================
        twilightData$sunStart[twilightData$date == cDate & twilightData$is_night == 0] = 
          format(as.POSIXct(sunrise[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        twilightData$sunStop[twilightData$date == cDate & twilightData$is_night == 0]  = 
          format(as.POSIXct(sunset[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        twilightData$sunStart[twilightData$date == cDate & twilightData$is_night == 1] = 
          format(as.POSIXct(sunset[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        twilightData$sunStop[twilightData$date == cDate & twilightData$is_night == 1]  = 
          format(as.POSIXct(sunrise[i + 1]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        
      # update civil twilight
      # =======================================================================
        twilightData$civilStart[twilightData$date == cDate & twilightData$is_night == 0] = 
          format(as.POSIXct(civilDawn[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        twilightData$civilStop[twilightData$date == cDate & twilightData$is_night == 0]  = 
          format(as.POSIXct(civilDusk[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        twilightData$civilStart[twilightData$date == cDate & twilightData$is_night == 1] = 
          format(as.POSIXct(civilDusk[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        twilightData$civilStop[twilightData$date == cDate & twilightData$is_night == 1]  = 
          format(as.POSIXct(civilDawn[i + 1]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        
      # update nautical twilight
      # =======================================================================
        twilightData$nauticalStart[twilightData$date == cDate & twilightData$is_night == 0] = 
          format(as.POSIXct(nauticalDawn[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        twilightData$nauticalStop[twilightData$date == cDate & twilightData$is_night == 0]  = 
          format(as.POSIXct(nauticalDusk[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        twilightData$nauticalStart[twilightData$date == cDate & twilightData$is_night == 1] = 
          format(as.POSIXct(nauticalDusk[i]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
        twilightData$nauticalStop[twilightData$date == cDate & twilightData$is_night == 1]  = 
          format(as.POSIXct(nauticalDawn[i + 1]), format = "%Y-%m-%d %H:%M:%S", tz = timeZone)
    } 
  
  # Convert all twilight time variables to POSICct class
  # ===========================================================================
    twilightData$sunStart      = as.POSIXct(twilightData$sunStart, 
                                            format = "%Y-%m-%d %H:%M:%S", 
                                            tz = timeZone)
    twilightData$sunStop       = as.POSIXct(twilightData$sunStop, 
                                            format = "%Y-%m-%d %H:%M:%S", 
                                            tz = timeZone)
    twilightData$civilStart    = as.POSIXct(twilightData$civilStart, 
                                            format = "%Y-%m-%d %H:%M:%S", 
                                            tz = timeZone)
    twilightData$civilStop     = as.POSIXct(twilightData$civilStop, 
                                            format = "%Y-%m-%d %H:%M:%S", 
                                            tz = timeZone)
    twilightData$nauticalStart = as.POSIXct(twilightData$nauticalStart, 
                                            format = "%Y-%m-%d %H:%M:%S", 
                                            tz = timeZone)
    twilightData$nauticalStop  = as.POSIXct(twilightData$nauticalStop, 
                                            format = "%Y-%m-%d %H:%M:%S", 
                                            tz = timeZone)

  # Omit rows for which any of the twilight times contains an NA
  #  --- 2023.08.30: These lines are commented out now, because they lead to
  #  ---             days being deleted undwantedly for locations at higher 
  #  ---             latitudes
  # ===========================================================================
    # twilightCols = c("sunStart", "sunStop", "civilStart", "civilStop", "nauticalStart", "nauticalStop")
    # twilightData = twilightData[apply(twilightData[, twilightCols], 
    #                                   MARGIN = 1, 
    #                                   FUN = function(x){all(!is.na(x))}),]
  
  # Calculate day and night durations in hours and seconds -- THIS IS NO LONGER USED.
  # ===========================================================================
    # twilightData$durationDayNight_h   = as.numeric(difftime(twilightData$sunStop, 
    #                                                         twilightData$sunStart, 
    #                                                         tz    = timeZone, 
    #                                                         units = "hours"))
    # twilightData$durationDayNight_sec = as.numeric(difftime(twilightData$sunStop, 
    #                                                         twilightData$sunStart, 
    #                                                         tz    = timeZone, 
    #                                                         units = "secs"))
    
  
  # Return the resulting dataframe
  # ===========================================================================
    return(twilightData)
} 

