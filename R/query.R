#### QUERY ------------------------------------------------------------
#' @title  Query 'SQL' database
#' @description  Run an 'SQL' query on an already connected database
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com};
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver
#' @param query an 'SQL' string with your query
#' @param as.is If TRUE, leaves data as it is
#'
#' @return the result of the query
#' @export
#'
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
#' QUERY(
#'   dbConnection = dbConnection,
#'   dbDriverChar = dbDriverChar,
#'   query = "Select * From collection order by row asc"
#' )
#' }
#'
QUERY <- function(dbConnection, dbDriverChar, query, as.is = FALSE) {
  if (dbDriverChar == "PostgreSQL") {
    t <- DBI::dbGetQuery(dbConnection, query, as.is = as.is)
  } else {
    t <- RODBC::sqlQuery(dbConnection, query, as.is = as.is)
  }
  return(t)
}
