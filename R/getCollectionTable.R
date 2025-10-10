#' @title Get BirdScan collection table
#' @author Fabian Hertner, Birgen Haest, Bart Kranstauber
#' @description Load collection from 'Birdscan MR1' 'SQL' database.
#' @inheritParams QUERY
#' @param timeInterval Null An optional vector of timestamps (either as `Date` or `POSIXct`)
#' to limit the data retrieved from the collections table. The filtering is done
#' based on the original radar timezone.
#'
#' @return A dataframe with the collection table
#' @family read SQL database functions
#' @export
#' @examples
#' \dontrun{
#' # Set server and database settings
#' # ==========================================================================
#' # Using and Microsoft SQL database
#' # ========================================================================
#' dbServer = "MACHINE\\SERVERNAME" # Set the name of your SQL server
#' dbName = "db_Name" # Set the name of your database
#' dbDriverChar = "SQL Server" # Set to "SQL Server"
#'
#' # Using a PostgreSQL
#' # ========================================================================
#' dbServer = "cloud.birdradar.com" # Set the name or IP of your postgreSQL
#' dbName = "db_Name" # Set the name of your database
#' dbDriverChar = "PostgreSQL" # Set to "PostgreSQL"
#'
#' # Open the connection with the database
#' # ==========================================================================
#' dbConnection = dbConnectBirdscanSQL(
#'   dbDriverChar = dbDriverChar,
#'   dbServer     = dbServer,
#'   dbName       = dbName,
#' )
#'
#' collectionTable = getCollectionTable(dbConnection)
#' }
#'
getCollectionTable = function(dbConnection, dbDriverChar, timeInterval = NULL) {
  if (!missing(dbDriverChar)) {
    lifecycle::deprecate_warn("0.4.0", "getCollectionTable(dbDriverChar)")
  }
  # Set feature name translations
  # ===========================================================================
  featureNames = data.frame(
    feature = c(
      "feature1", "feature2", "feature3",
      "feature4", "feature5", "feature6",
      "feature7", "feature8", "feature9",
      "feature10", "feature11", "feature12",
      "feature13", "feature14", "feature15",
      "feature16", "feature17", "feature18",
      "feature19", "feature20", "feature21",
      "feature22", "feature23", "feature24",
      "feature25", "feature26", "feature27",
      "feature28", "feature29", "feature30",
      "feature31", "feature32", "feature33",
      "feature34", "feature35", "feature36",
      "feature37", "feature38", "feature39",
      "feature40", "feature41", "feature42",
      "feature43", "feature44", "feature45",
      "feature46", "feature47", "feature48",
      "feature49", "feature50"
    ),
    featureNames = c(
      "altitude_AGL", # Altitude above ground level (m)
      "azimuth", # Azimuth (true north)
      "speed", # Speed (m/s)
      NA, NA, "rotationFreq", # Rotation frequency in relation to sampling frequency
      NA, NA, NA, NA, NA, NA,
      "freqRatio", # Frequency Ratio
      "maxLevel", # Maximum level
      "polRatio", # Polarisation ratio
      "absPolarisation", # Absolute polarisation
      "rcs", # Radar cross section (m^2)
      "sqrt(RCS)", # Square root of radar cross section (m)
      "durationOfEcho", # Duration of echo (seconds)
      "durationOfEchoInSTC", # Duration of echo in STC
      NA, NA, NA,
      "alpha", # Alpha: |AlphaEnd – AlphaStart|
      "theta", # Theta: 2⋅EpsilonCalc
      NA, NA, NA, NA,
      "altitudeLeftSideOfEcho", # Altitude left side of echo (m)
      "altitudeRightSideOfEcho", # Altitude right side of echo (m)
      NA,
      "distLeftToBottom", # Distance between left side and bottom of echo (samples)
      "nSamplesInEcho", # Length of echo (samples)
      "areaOfEcho", # Area of echo (seconds multiplied by meter)
      NA,
      "speed", # Speed (m/s) - This is the new speed variable, included in the Birdscan software as of v1.7
      NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA
    )
  )

  # Set where clause
  # ===========================================================================
  whereClause = ""
  if (!is.null(timeInterval)) {
    whereClause = paste0(
      "WHERE time_stamp BETWEEN '",
      format(min(timeInterval)),
      "' AND '",
      format(max(timeInterval)),
      "' "
    )
  }

  # load collection from 'MS-SQL' database
  # ===========================================================================
  if (class(dbConnection) %in% "RODBC") {
    collectionTable = QUERY(
      dbConnection,
      query =
        paste0(
          "SELECT * FROM collection ",
          whereClause, " order by row asc"
        )
    )
    collectionTable_time_stamp = QUERY(dbConnection,
      query =
        paste0(
          "SELECT time_stamp FROM collection ",
          whereClause, " order by row asc"
        ),
      as.is = TRUE
    )
    collectionTable$time_stamp = collectionTable_time_stamp$time_stamp

    # load collection from 'PostgreSQL' database
    # ===========================================================================
  } else if (class(dbConnection) %in% c("PqConnection", "PostgreSQLConnection")) {
    collectionTable = QUERY(
      dbConnection,
      query =
        paste0(
          "SELECT *, time_stamp::character varying ts FROM collection ",
          whereClause, " order by row asc"
        )
    )
    collectionTable$time_stamp = collectionTable$ts
    collectionTable$ts = NULL
    # colnames(collectionTable)[colnames(collectionTable) == "ts"] = "time_stamp"
  }
  colnames(collectionTable)[colnames(collectionTable) == "echoid"] = "echoID"
  colnames(collectionTable)[colnames(collectionTable) == "protocolid"] = "protocolID"

  # rename "old" features and remove unused feature columns in collectionTable
  # ===========================================================================
  featureCols = match(names(collectionTable), featureNames$feature)
  colNames = paste(featureNames$feature, featureNames$featureNames, sep = ".")
  colNames[is.na(featureNames$featureNames)] = NA
  names(collectionTable)[!is.na(featureCols)] = colNames[stats::na.omit(featureCols)]
  collectionTable = collectionTable[!is.na(names(collectionTable))]

  names(collectionTable)[names(collectionTable) == "mtr_fact"] = "mtr_factor_old"
  names(collectionTable)[names(collectionTable) == "statistical_classification"] = "statistical_classification_old"

  # Adjust feature37.speed to set unreasonable values to NA
  # ===========================================================================
  collectionTable = filterSpeedFeature37(echoData = collectionTable)

  # Return collection table
  # ===========================================================================
  return(collectionTable)
}
