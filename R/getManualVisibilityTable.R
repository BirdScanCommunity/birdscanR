#' @title Get manual visibility table
#' @author Baptiste Schmid, Birgen Haest
#' @description Load visibility table from an already connected 'Birdscan MR1'
#' 'SQL' database.
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL'
#' it connects to cloud.birdradar.com
#'
#' @return A dataframe with the manual visibility table
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
#' manualVisibilityTable = getManualVisibilityTable(dbConnection, dbDriverChar)
#' }
#'
getManualVisibilityTable = function(dbConnection, dbDriverChar) {
  # load protocol table from local MS-SQL DB
  # ===========================================================================
  if (dbDriverChar == "SQL Server") {
    manualVisibilityTable = QUERY(
      dbConnection,
      dbDriverChar,
      "SELECT * FROM visibility_manual order by blind_from asc"
    )
  } else if (dbDriverChar == "PostgreSQL") {
    message("Fetching manual visibility table from PostgrSQL not yet implemented.")
  }

  return(manualVisibilityTable)
}
