#### getEchoValidationTable ----------------------------------------------------
#' @title  Get a BirdScan echo validation table
#' @description  gets the echoValidationTable from an already connected database
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com};
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar dbDriverChar 'SQL Server' The name of the driver. Should
#' be either 'SQL Server' or 'PostgreSQL'. If 'PostgreSQL', it connects to cloud.birdradar.com
#'
#' @return A dataframe called echovalidationTable
#' @export
#' @examples
#' \dontrun{
#' # Set server and database settings
#' # ===========================================================================
#' dbServer <- "MACHINE\\SERVERNAME" # Set the name of your SQL server
#' dbName <- "db_Name" # Set the name of your database
#' dbDriverChar <- "SQL Server" # Set either "SQL Server" or "PostgreSQL"
#'
#' # Open the connection with the database
#' # ===========================================================================
#' dsn <- paste0(
#'   "driver=", dbDriverChar, ";server=", dbServer,
#'   ";database=", dbName,
#'   ";uid=", rstudioapi::askForPassword("Database user"),
#'   ";pwd=", rstudioapi::askForPassword("Database password")
#' )
#' dbConnection <- RODBC::odbcDriverConnect(dsn)
#'
#' echovalidationTable <- getEchoValidationTable(dbConnection, dbDriverChar)
#' }
#'
getEchoValidationTable <- function(dbConnection, dbDriverChar) {
  echovalidationTypesTable <- QUERY(
    dbConnection,
    dbDriverChar,
    "Select * From echo_validation_type"
  )

  echovalidationTable <- QUERY(
    dbConnection,
    dbDriverChar,
    "Select * From echo_validation order by echo_id asc"
  )

  echoValidationList <- echovalidationTable$type
  echovalidationTable$type <- echovalidationTypesTable$name[match(echoValidationList, echovalidationTypesTable$id)]
  rm(list = "echovalidationTypesTable", "echoValidationList")
  names(echovalidationTable)[names(echovalidationTable) == "echo_id"] <- "echo"
  names(echovalidationTable)[names(echovalidationTable) == "type"] <- "echoValidationType"

  return(echovalidationTable)
}
