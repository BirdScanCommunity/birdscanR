#' @title Get BirdScan visibility table
#' @author Fabian Hertner, Birgen Haest
#' @description Load visibility table from an already connected 'Birdscan MR1'
#' 'SQL' database.
#' @inheritParams QUERY
#' @return A `data.frame` with the visibility table.
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
#' visibilityTable = getVisibilityTable(dbConnection)
#' }
#'
getVisibilityTable = function(dbConnection, dbDriverChar) {
  if (!missing(dbDriverChar)) {
    lifecycle::deprecate_warn("0.4.0", "getVisibilityTable(dbDriverChar)")
  }
  # load protocol table from local MS-SQL DB
  # ============================================================================
  if (class(dbConnection) != "PqConnection") {
    visibilityTable = QUERY(dbConnection,
      query = paste0(
        "SELECT * FROM visibility ",
        "order by visibilityLogID asc"
      )
    )
    visibilityTable_times = QUERY(dbConnection,
      query = paste0(
        "SELECT blind_from, blind_to ",
        "FROM visibility order by ",
        "visibilityLogID asc"
      ),
      as.is = TRUE
    )
    visibilityTable$blind_from = visibilityTable_times$blind_from
    visibilityTable$blind_to = visibilityTable_times$blind_to

    # load protocol table from PostGreSQL
    # ============================================================================
  } else {
    visibilityTable = QUERY(dbConnection,
      query = paste0(
        "SELECT *,blind_from::character",
        " varying as ",
        "blindfrom,blind_to::character",
        " varying as blindto FROM ",
        "visibility order by ",
        "visibilitylogid asc"
      )
    )
    visibilityTable$blind_from = visibilityTable$blindfrom
    visibilityTable$blind_to = visibilityTable$blindto
    visibilityTable$blindfrom = NULL
    visibilityTable$blindto = NULL
  }


  colnames(visibilityTable)[colnames(visibilityTable) == "visibilitylogid"] =
    "visibilityLogID"
  colnames(visibilityTable)[colnames(visibilityTable) == "protocolid"] =
    "protocolID"
  return(visibilityTable)
}
