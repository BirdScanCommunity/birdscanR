#' @title Get manual visibility table
#' @author Baptiste Schmid, Birgen Haest
#' @description Load visibility table from an already connected 'Birdscan MR1'
#' 'SQL' database.
#' @inheritParams QUERY
#'
#' @return A `data.frame` with the manual visibility table.
#' @family read file functions
#' @export
#' @examples
#' \dontrun{
#' # Set server and database settings
#' # ===========================================================================
#' dbServer = "MACHINE\\SERVERNAME" # Set the name of your SQL server
#' dbName = "db_Name" # Set the name of your database
#' dbDriverChar = "SQL Server" # Set either "SQL Server" or "PostgreSQL"
#'
#' # Open the connection with the database
#' # ===========================================================================
#' dsn = paste0(
#'   "driver=", dbDriverChar, ";server=", dbServer,
#'   ";database=", dbName,
#'   ";uid=", rstudioapi::askForPassword("Database user"),
#'   ";pwd=", rstudioapi::askForPassword("Database password")
#' )
#' dbConnection = RODBC::odbcDriverConnect(dsn)
#'
#' manualVisibilityTable = getManualVisibilityTable(dbConnection)
#' }
#'
getManualVisibilityTable = function(dbConnection, dbDriverChar) {
  if (!missing(dbDriverChar)) {
    lifecycle::deprecate_warn("0.4.0", "getManualVisibilityTable(dbDriverChar)")
  }
  # load protocol table from local MS-SQL DB
  # ===========================================================================
  if (class(dbConnection) %in% "RODBC") {
    manualVisibilityTable = QUERY(
      dbConnection,
      query =
        "SELECT * FROM visibility_manual order by blind_from asc"
    )
  } else if (class(dbConnection) %in% "PqConnection") {
    message("Fetching manual visibility table from PostgrSQL not yet implemented.")
  }

  return(manualVisibilityTable)
}
