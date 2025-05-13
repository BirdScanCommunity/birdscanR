#' @title Get a BirdScan echo validation table
#' @author Fabian Hertner, Birgen Haest
#' @description Gets the echoValidationTable from an already connected database.
#' @inheritParams QUERY
#'
#' @return A dataframe called echovalidationTable
#' @family read SQL database functions
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
#' echovalidationTable = getEchoValidationTable(dbConnection)
#' }
#'
getEchoValidationTable = function(dbConnection, dbDriverChar) {
  if (!missing(dbDriverChar)) {
    lifecycle::deprecate_warn("0.4.0", "getEchoValidationTable(dbDriverChar)")
  }
  echovalidationTypesTable = QUERY(
    dbConnection,
    query = "SELECT * FROM echo_validation_type"
  )

  echovalidationTable = QUERY(
    dbConnection,
    query = "SELECT * FROM echo_validation order by echo_id asc"
  )

  echoValidationList = echovalidationTable$type
  echovalidationTable$type = echovalidationTypesTable$name[match(echoValidationList, echovalidationTypesTable$id)]
  rm(list = "echovalidationTypesTable", "echoValidationList")
  names(echovalidationTable)[names(echovalidationTable) == "echo_id"] = "echo"
  names(echovalidationTable)[names(echovalidationTable) == "type"] = "echoValidationType"

  return(echovalidationTable)
}
