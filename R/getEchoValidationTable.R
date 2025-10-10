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
