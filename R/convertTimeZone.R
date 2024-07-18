#### convertTimeZone ----------------------------------------------------------------
#' @title Converts timestamps from radar time zone to an user-defined time zone
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com};
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @description Converts timestamps from radar time zone to an user-defined time
#' zone
#' @param data a data frame containing BirdScan data
#' @param colNames a character vector containing valid column names, as present
#' in `data`
#' @param originTZ character, the time zone name of data to be converted
#' (default is "etc/GMT0")
#' @param targetTZ character, the time zone name to convert data into
#' (default is "etc/GMT0")
#'
#' @return a data frame identical to `data`, any columns declared in `colNames`
#' will have their name changed with a suffix (`_originTZ` or `_targetTZ`) added.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set server, database, and other input settings for data extraction
#' # ===========================================================================
#' dbServer <- "MACHINE\\SERVERNAME" # Set the name of your SQL server
#' dbName <- "db_Name" # Set the name of your database
#' dbDriverChar <- "SQL Server" # Set either "SQL Server" or "PostgreSQL"
#' mainOutputDir <- file.path(".", "results")
#' radarTimeZone <- "Etc/GMT0"
#' targetTimeZone <- "Etc/GMT0"
#' listOfRfFeaturesToExtract <- c(167, 168)
#' siteLocation <- c(47.494427, 8.716432)
#' sunOrCivil <- "civil"
#'
#' # Get data
#' # ===========================================================================
#' dbData <- extractDbData(
#'   dbDriverChar = dbDriverChar,
#'   dbServer = dbServer,
#'   dbName = dbName,
#'   saveDbToFile = TRUE,
#'   dbDataDir = mainOutputDir,
#'   radarTimeZone = radarTimeZone,
#'   targetTimeZone = targetTimeZone,
#'   listOfRfFeaturesToExtract = listOfRfFeaturesToExtract,
#'   siteLocation = siteLocation,
#'   sunOrCivil = sunOrCivil
#' )
#'
#' # Add day/night info to echo data
#' # ===========================================================================
#' echoData <- convertTimeZone(
#'   data = dbData$echoData,
#'   colNames = c("time_stamp"),
#'   originTZ = "Etc/GMT0",
#'   targetTZ = "Etc/GMT-2"
#' )
#' }
#'
convertTimeZone <- function(data = NULL,
                            colNames = "",
                            originTZ = "Etc/GMT0",
                            targetTZ = "Etc/GMT0") {
  if (!is.null(data)) {
    for (i in seq_along(colNames)) {
      # new column names
      originTzColName <- paste(colNames[i], "originTZ", sep = "_")
      targetTzColName <- paste(colNames[i], "targetTZ", sep = "_")

      # make sure data is available
      colNr <- match(colNames[i], names(data))
      if (is.na(colNr)) {
        warning("no data to convert to time zone (function: convertTimeZone)")
        return()
      }

      # insert target time zone column to data
      names(data)[names(data) == colNames[i]] <- originTzColName
      tmp <- data.frame(data[, names(data) == originTzColName])
      names(tmp) <- targetTzColName
      data <- data.frame(data[, 1:colNr, drop = FALSE], tmp, data[, (colNr + 1):length(data[1, ]), drop = FALSE])

      # add timezones to blind_from and blind_to in visibilityData
      data[, names(data) == originTzColName] <- as.POSIXct(x = data[, names(data) == originTzColName], tz = originTZ)
      tmp <- format(data[, names(data) == originTzColName], "%Y-%m-%d %H:%M:%S", tz = targetTZ)
      data[, names(data) == targetTzColName] <- as.POSIXct(strptime(x = tmp, "%Y-%m-%d %H:%M:%S", tz = targetTZ))
    }
  }

  return(data)
}

# convertTimeZone()
# convertTimeZone(data = data$visibilityData,
#                colNames = c("blind_from", "blind_to"),
#                originTZ = radarTimeZone, targetTZ = targetTimeZone)
# convertTimeZone(data = manualBlindTimes, colNames= c("start", "stop"),
#                originTZ = blindTimesTimeZone, targetTZ = targetTimeZone)
# convertTimeZone(data = data$siteData, colNames = c("projectStart", "projectEnd"),
#                originTZ = radarTimeZone, targetTZ = targetTimeZone)
# convertTimeZone(data = data$echoData, colNames = c("time_stamp"),
#                originTZ = radarTimeZone, targetTZ = targetTimeZone)
