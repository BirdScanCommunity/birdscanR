#' @title Connect to a Birdscan SQL database
#' @author Birgen Haest
#' @description Connects to a Birdscan SQL database, be it a Microsoft SQL or
#' postgreSQL database3
#' @param dbDriverChar 'SQL Server' The name of the driver. Should be either
#' 'SQL Server' or 'PostgreSQL'.
#' @param dbServer NULL The name of the SQL Server. For a 'PostgreSQL' this can
#' be a the host address or the internal IP.
#' @param dbName NULL The name of the SQL database
#' @param dbUser NULL The username of the SQL server
#' @param dbPwd NULL The password for the user name
#' @param dbPort 5432 The dbPort parameter specifies the TCP/IP port number on
#' which the PostgreSQL server is listening for connections (default is 5432)
#'
#' @return A database connection to your target database
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
#' }
#'
dbConnectBirdscanSQL = function(dbDriverChar = "SQL Server",
                                dbServer = NULL,
                                dbName = NULL,
                                dbUser = NULL,
                                dbPwd = NULL,
                                dbPort = 5432) {
  # Open the database connection
  # =============================================================================
  # CASE: "SQL Server"
  # ===========================================================================
  if (dbDriverChar == "SQL Server") {
    # CASE: Username and Password are provided
    # =======================================================================
    if (!is.null(dbUser) | !is.null(dbPwd)) {
      dsn = paste0(
        "driver=", dbDriverChar, ";server=", dbServer,
        ";database=", dbName,
        ";uid=", dbUser,
        ";pwd=", dbPwd
      )

      # CASE: Username and Password are NOT provided
      #       Request the username and pwd via the rstudioAPI
      # =======================================================================
    } else {
      dsn = paste0(
        "driver=", dbDriverChar, ";server=", dbServer,
        ";database=", dbName,
        ";uid=", rstudioapi::askForPassword("Database user"),
        ";pwd=", rstudioapi::askForPassword("Database password")
      )
    }
    # Connect to the Microsoft SQL database
    # =======================================================================
    dbConnection = RODBC::odbcDriverConnect(dsn)

    # CASE: "PostgreSQL"
    # ===========================================================================
  } else if (dbDriverChar == "PostgreSQL") {
    # CASE: Username and Password are provided
    # =======================================================================
    if (!is.null(dbUser) | !is.null(dbPwd)) {
      dbConnection = DBI::dbConnect(RPostgreSQL::PostgreSQL(),
        host     = dbServer,
        dbname   = dbName,
        user     = dbUser,
        password = dbPwd,
        port     = dbPort
      )

      # CASE: Username and Password are NOT provided
      #       Request the username and pwd via the rstudioAPI
      # =====================================================================
    } else {
      dbConnection = DBI::dbConnect(RPostgreSQL::PostgreSQL(),
        host     = dbServer,
        dbname   = dbName,
        user     = rstudioapi::askForPassword("Database user"),
        password = rstudioapi::askForPassword("Database password"),
        port     = dbPort
      )
    }
  }

  # Check whether there is a connection
  # ===========================================================================
  connected = if (dbDriverChar == "PostgreSQL") {
    RPostgreSQL::isPostgresqlIdCurrent(dbConnection)
  } else {
    dbConnection != -1
  }
  if (!connected) {
    stop("Could not open database. Make sure to set dbServer, dbName, and
            your credentials right.")
  }

  # Return the connection
  # ===========================================================================
  return(dbConnection)
}
