#' @title Get BirdScan echo features
#' @author Fabian Hertner, Birgen Haest
#' @description Load echo rffeature map from 'Birdscan MR1' 'SQL' database.
#' @inheritParams QUERY
#' @param listOfRfFeaturesToExtract Either NULL (i.e., don't extract any of the
#' rf features), "all" (i.e., extract all rf features) or a vector of the
#' feature numbers to extract. Default is NULL. Feature IDs can be found in the
#' 'rfFeatures' table in the sql database.
#' @param echoIDRange NULL A two-element vector of integers to subset the rf
#' feature extraction to a range of echoIDs. Default is to extract for all echoes.
#'
#' @return A list of the features extracted
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
#' # Set list of Rf features you also want to extract
#' # Vector with RF features to extract. Feature IDs can be found in the
#' # 'rfFeatures' table in the sql database.
#' # Example: Get wing beat frequency and credibility: c(167, 168)
#' # Set to NULL to not extract any.
#' # Set to "all" to extract all features.
#' # ===========================================================================
#' listOfRfFeaturesToExtract = c(167, 168)
#'
#' echoFeatures = getEchoFeatures(
#'   dbConnection,
#'   listOfRfFeaturesToExtract
#' )
#' }
#'
getEchoFeatures = function(dbConnection, dbDriverChar,
                           listOfRfFeaturesToExtract,
                           echoIDRange = NULL) {
  if (!missing(dbDriverChar)) {
    lifecycle::deprecate_warn("0.4.0", "getEchoFeatures(dbDriverChar)")
  }
  # load 'rfFeatures' table from 'MS-SQL' database
  # ===========================================================================
  rffeaturesTable = QUERY(
    dbConnection,
    query =
      "SELECT * FROM rffeatures"
  )

  # Set listOfRfFeaturesToExtract to all features if "all" is provided
  # ===========================================================================
  if (!is.null(listOfRfFeaturesToExtract)) {
    if (is.character(listOfRfFeaturesToExtract)) {
      if (listOfRfFeaturesToExtract == "all") {
        listOfRfFeaturesToExtract = rffeaturesTable$id
      }
    }
  }

  # load 'echo_rffeature_map' table from 'MS-SQL' database
  # ===========================================================================
  if (!is.null(listOfRfFeaturesToExtract)) {
    # CASE: Load features for all echoes
    # =======================================================================
    if (is.null(echoIDRange)) {
      echorffeaturesMapTable = QUERY(
        dbConnection,
        query =
          paste(
            "SELECT * FROM echo_rffeature_map WHERE feature IN ( ",
            paste(listOfRfFeaturesToExtract, collapse = ", "),
            " )"
          )
      )
      # CASE: Load features for a subset of echoes
      # =======================================================================
    } else {
      echorffeaturesMapTable = QUERY(
        dbConnection,
        query =
          paste(
            "SELECT * FROM echo_rffeature_map WHERE feature IN ( ",
            paste(listOfRfFeaturesToExtract, collapse = ", "),
            " ) AND echo BETWEEN ",
            min(echoIDRange), " AND ", max(echoIDRange)
          )
      )
    }


    featurelist = echorffeaturesMapTable$feature
    echorffeaturesMapTable$feature = rffeaturesTable$name[match(featurelist, rffeaturesTable$id)]
    echoRfFeatureMap = reshape2::dcast(echorffeaturesMapTable,
      echo ~ feature,
      value.var = "value",
      fun.aggregate = mean
    )

    return(list(echoRfFeatureMap = echoRfFeatureMap, rfFeatures = rffeaturesTable))
  }
  return(list(echoRfFeatureMap = NULL, rfFeatures = rffeaturesTable))
}
