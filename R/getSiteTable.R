#' @title Get BirdScan site table
#' @author Fabian Hertner, Birgen Haest
#' @description Load site table from an already connected 'Birdscan MR1' 'SQL'
#' database.
#' @inheritParams QUERY
#' @return A `data.frame` with the site table
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
#' siteTable <- getSiteTable(dbConnection)
#' }
#'
getSiteTable <- function(dbConnection, dbDriverChar) {
  if (!missing(dbDriverChar)) {
    lifecycle::deprecate_warn("0.4.0", "getSiteTable(dbDriverChar)")
  }
  # load protocol table from local MS-SQL DB
  # ==============================================================================
  siteTable <- QUERY(dbConnection,
    query = "SELECT * FROM site order by row asc"
  )
  colnames(siteTable)[colnames(siteTable) == "siteid"] <- "siteID"
  colnames(siteTable)[colnames(siteTable) == "sitecode"] <- "siteCode"
  colnames(siteTable)[colnames(siteTable) == "radarid"] <- "radarID"
  colnames(siteTable)[colnames(siteTable) == "sitename"] <- "siteName"
  colnames(siteTable)[colnames(siteTable) == "sitedesc"] <- "siteDesc"
  colnames(siteTable)[colnames(siteTable) == "projectstart"] <- "projectStart"
  colnames(siteTable)[colnames(siteTable) == "projectend"] <- "projectEnd"
  colnames(siteTable)[colnames(siteTable) == "timeshift"] <- "timeShift"
  colnames(siteTable)[colnames(siteTable) == "radarorientation"] <- "radarOrientation"
  colnames(siteTable)[colnames(siteTable) == "ftpupload"] <- "ftpUpload"
  colnames(siteTable)[colnames(siteTable) == "automode"] <- "autoMode"
  siteTable_times <- QUERY(dbConnection,
    query = "SELECT projectStart, projectEnd FROM site order by row asc",
    as.is = TRUE
  )
  colnames(siteTable_times)[colnames(siteTable_times) == "projectstart"] <- "projectStart"
  colnames(siteTable_times)[colnames(siteTable_times) == "projectend"] <- "projectEnd"
  siteTable$projectStart <- siteTable_times$projectStart
  siteTable$projectEnd <- siteTable_times$projectEnd
  siteTable$radarID <- as.character(siteTable$radarID)
  return(siteTable)
}
