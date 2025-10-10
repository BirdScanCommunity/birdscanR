#' @title  Query 'SQL' database
#' @author Fabian Hertner, Birgen Haest
#' @description Run an 'SQL' query on an already connected database.
#' @param dbConnection A valid database connection.
#' @param dbDriverChar This was the name of the driver, and is now automatically detected, therefore it should be omitted into the future.
#' @param query an 'SQL' string with your query
#' @param as.is If TRUE, leaves data as it is
#'
#' @return the result of the query
#' @family read SQL database functions
#' @export
#'
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
#' QUERY(
#'   dbConnection = dbConnection,
#'   query = "Select * From collection order by row asc"
#' )
#' }
#'
QUERY <- function(dbConnection, dbDriverChar, query, as.is = FALSE) {
  if (!missing(dbDriverChar)) {
    lifecycle::deprecate_warn("0.4.0", "QUERY(dbDriverChar)")
  }
  if (class(dbConnection) %in% c("PqConnection", "PostgreSQLConnection")) {
    t <- DBI::dbGetQuery(dbConnection, query, as.is = as.is)
  } else if (class(dbConnection) %in% "RODBC") {
    t <- RODBC::sqlQuery(dbConnection, query, as.is = as.is)
  } else {
    stop("The `dbConnection` argument now only supports connections of the
         class `PqConnection`, `PostgreSQLConnection` and `RODBC`.")
  }

  return(t)
}
