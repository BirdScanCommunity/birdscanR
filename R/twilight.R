#### twilight -----------------------------------------------------------------
#' @title Calculate the twilight hours for given locations
#' 
#' @description  Calculate the  duration of night in hours and seconds. It calculate also the time of both solar and civil start of the day and end of the day.
#' 
#' @param timeRange the range of dates in which to calculate the twilight
#' @param latLon  a list of X, Y coordinates 
#' @param crs_datum="WGS84" the coordinate reference system and datum of the X, Y coordinates
#' @param timeZone the timezone of the area of interest
#'
#' @return a data frame with the results
#' @export
#' 
twilight <- function( timeRange, latLon, crs_datum = "WGS84", timeZone )
{
  dateSeq <- seq( as.Date( timeRange[ 1 ], tz = timeZone ) - 1, as.Date( timeRange[ 2 ], tz = timeZone ) + 2, "day")
  lon_lat <- data.frame( X = latLon[ 2 ], Y = latLon[ 1 ] )
  crds <- CRS( paste0( "+proj=longlat +datum=", crs_datum ) ) # here i used GoogleEarth coodrinates that use the "WGS84" coodrinate system
  lon_lat <- SpatialPoints( lon_lat, proj4string = crds ) # here i used GoogleEarth coodrinates that use the "WGS84" coodrinate system
  
  # make sure that date is in
  dateSeq <- format( as.POSIXct( strptime( x = dateSeq, "%Y-%m-%d", tz = "UTC" ) ), format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE )
  dateSeq <- as.POSIXct( strptime( x = dateSeq, "%Y-%m-%d %H:%M:%S", tz = timeZone ) )
  
  dawn <- crepuscule( lon_lat, dateSeq, solarDep = 6, direction = "dawn", POSIXct.out = TRUE )$time # civil (see argument: "solarDep = 6") twilight at dawn for the day of observation
  dusk <- crepuscule( lon_lat, dateSeq, solarDep = 6, direction = "dusk", POSIXct.out = TRUE )$time # civil twilight at dusk

  sunrise <- sunriset( lon_lat, dateSeq, direction="sunrise", POSIXct.out = TRUE )$time
  sunset <- sunriset( lon_lat, dateSeq, direction="sunset", POSIXct.out = TRUE )$time
  
  twilight <- expand.grid( "is_night" = 0:1, "date" = dateSeq, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE )
  
  # add 'sunStart', 'sunStop', civilStart' and 'civilStop' column to twilight
  twilight <- data.frame( twilight, sunStart = NA, sunStop = NA, civilStart = NA, civilStop = NA )
  
  twilight$sunStart <- twilight$sunStop <- twilight$civilStart <- twilight$civilStop <- NA
  for( i in 1:length( dateSeq ) ){
    i.date <- unique( dateSeq )[ i ]
    # sunrise/sunset
    twilight$sunStart[ twilight$date == i.date & twilight$is_night == 0 ] <- format( as.POSIXct( sunrise[ i ] ), format = "%Y-%m-%d %H:%M:%S", tz = timeZone )
    twilight$sunStop[ twilight$date == i.date & twilight$is_night == 0 ] <- format( as.POSIXct( sunset[ i ] ), format = "%Y-%m-%d %H:%M:%S", tz = timeZone )
    twilight$sunStart[ twilight$date == i.date & twilight$is_night == 1 ] <- format( as.POSIXct( sunset[ i ] ), format = "%Y-%m-%d %H:%M:%S", tz = timeZone )
    twilight$sunStop[ twilight$date == i.date & twilight$is_night == 1 ] <- format( as.POSIXct( sunrise[ i + 1 ] ), format = "%Y-%m-%d %H:%M:%S", tz = timeZone )
    # Civil twilight
    twilight$civilStart[ twilight$date == i.date & twilight$is_night == 0 ] <- format( as.POSIXct( dawn[ i ] ), format = "%Y-%m-%d %H:%M:%S", tz = timeZone )
    twilight$civilStop[ twilight$date == i.date & twilight$is_night == 0 ] <- format( as.POSIXct( dusk[ i ] ), format = "%Y-%m-%d %H:%M:%S", tz = timeZone )
    twilight$civilStart[ twilight$date == i.date & twilight$is_night == 1 ] <- format( as.POSIXct( dusk[ i ] ), format = "%Y-%m-%d %H:%M:%S", tz = timeZone )
    twilight$civilStop[ twilight$date == i.date & twilight$is_night == 1 ] <- format( as.POSIXct( dawn[ i + 1 ] ), format = "%Y-%m-%d %H:%M:%S", tz = timeZone )
  } # end of for-loop >>> for( i in 1:length( dateSeq ) ){...
  
  twilight$sunStart <- as.POSIXct( twilight$sunStart, format = "%Y-%m-%d %H:%M:%S", tz = targetTimeZone )
  twilight$sunStop <- as.POSIXct( twilight$sunStop, format = "%Y-%m-%d %H:%M:%S", tz = targetTimeZone )
  twilight$civilStart <- as.POSIXct( twilight$civilStart, format = "%Y-%m-%d %H:%M:%S", tz = targetTimeZone )
  twilight$civilStop <- as.POSIXct( twilight$civilStop, format = "%Y-%m-%d %H:%M:%S", tz = targetTimeZone )

  # omit rows with NA's
  twilight <- twilight[ !is.na( twilight$sunStart ) & !is.na( twilight$sunStop ) & !is.na( twilight$civilStart ) & !is.na( twilight$civilStop ), ]
  
  twilight$durationDayNight_h <- as.numeric( difftime( twilight$sunStop, twilight$sunStart, tz = timeZone, units = "hours" ) )
  twilight$durationDayNight_sec <- as.numeric( difftime( twilight$sunStop, twilight$sunStart, tz = timeZone, units = "secs" ) )
  
  out <- twilight
  
} # end of twilight-function

#twilight( timeRange = timeRangeEchoData, latLon = siteLocation, timeZone = targetTimeZone )
