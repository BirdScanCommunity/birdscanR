#' @title Get BirdScan time bins table
#' @author Fabian Hertner, Birgen Haest
#' @description Load time bins table from an already connected 'Birdscan MR1'
#' 'SQL' database.
#' @inheritParams QUERY
#' @return A dataframe with the time bins table
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
#' timeBinsTable = getTimeBinsTable(dbConnection)
#' }
#'
getTimeBinsTable = function(dbConnection, dbDriverChar) {
  if (!missing(dbDriverChar)) {
    lifecycle::deprecate_warn("0.4.0", "getTimeBinsTable(dbDriverChar)")
  }
  # load protocol table from local MS-SQL DB
  # ============================================================================
  if (class(dbConnection) %in% "RODBC") {
    timeBinsTable = QUERY(
      dbConnection,
      query =
        "SELECT * FROM time_bins order by id asc"
    )
    timeBinsTable_times = QUERY(dbConnection,
      query =
        paste0(
          "SELECT time_start, time_stop ",
          "FROM time_bins order by id asc"
        ),
      as.is = TRUE
    )
    timeBinsTable$time_start = timeBinsTable_times$time_start
    timeBinsTable$time_stop = timeBinsTable_times$time_stop

    # load protocol table from PostGreSQL
    # ============================================================================
  } else if (class(dbConnection) %in% c("PqConnection", "PostgreSQLConnection")) {
    timeBinsTable = QUERY(
      dbConnection,
      query = paste0(
        "SELECT *,time_start::character ",
        "varying as ",
        "start,time_stop::character ",
        "varying as stop FROM time_bins ",
        "order by id asc"
      )
    )
    timeBinsTable$time_start = timeBinsTable$start
    timeBinsTable$time_stop = timeBinsTable$stop
    timeBinsTable$start = NULL
    timeBinsTable$stop = NULL
  }

  colnames(timeBinsTable)[colnames(timeBinsTable) == "siteid"] = "siteID"
  return(timeBinsTable)
}
