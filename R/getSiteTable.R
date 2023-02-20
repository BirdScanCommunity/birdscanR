#### getSiteTable ------------------------------------------------------------
#' @title  Get BirdScan site table
#' @description load site table from an already connected 'Birdscan MR1' 'SQL' 
#' database
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' 
#' it connects to cloud.birdradar.com
#'
#' @return A dataframe with the site table
#' @export
#' @examples
#' \dontrun{
#' # Set server and database settings
#' # ===========================================================================
#'   dbServer       = "MACHINE\\SERVERNAME"     # Set the name of your SQL server
#'   dbName         = "db_Name"                   # Set the name of your database
#'   dbDriverChar   = "SQL Server"                # Set either "SQL Server" or "PostgreSQL"
#'
#' # Open the connection with the database
#' # ===========================================================================
#'   dsn = paste0("driver=", dbDriverChar, ";server=", dbServer,
#'                ";database=", dbName,
#'                ";uid=", rstudioapi::askForPassword("Database user"),
#'                ";pwd=", rstudioapi::askForPassword("Database password"))
#'   dbConnection = RODBC::odbcDriverConnect(dsn)
#'
#' siteTable = getSiteTable(dbConnection, dbDriverChar)
#' }
#'
getSiteTable = function(dbConnection, dbDriverChar){
# load protocol table from local MS-SQL DB
# ==============================================================================
  siteTable = QUERY(dbConnection, 
                    dbDriverChar, 
                    "Select * From site order by row asc")
  colnames(siteTable)[colnames(siteTable) == "siteid"]           = "siteID"
  colnames(siteTable)[colnames(siteTable) == "sitecode"]         = "siteCode"
  colnames(siteTable)[colnames(siteTable) == "radarid"]          = "radarID"
  colnames(siteTable)[colnames(siteTable) == "sitename"]         = "siteName"
  colnames(siteTable)[colnames(siteTable) == "sitedesc"]         = "siteDesc"
  colnames(siteTable)[colnames(siteTable) == "projectstart"]     = "projectStart"
  colnames(siteTable)[colnames(siteTable) == "projectend"]       = "projectEnd"
  colnames(siteTable)[colnames(siteTable) == "timeshift"]        = "timeShift"
  colnames(siteTable)[colnames(siteTable) == "radarorientation"] = "radarOrientation"
  colnames(siteTable)[colnames(siteTable) == "ftpupload"]        = "ftpUpload"
  colnames(siteTable)[colnames(siteTable) == "automode"]         = "autoMode"
  siteTable_times = QUERY(dbConnection, dbDriverChar, 
                          "Select projectStart, projectEnd From site order by row asc", 
                          as.is = TRUE)
  colnames(siteTable_times)[colnames(siteTable_times) == "projectstart"] = "projectStart"
  colnames(siteTable_times)[colnames(siteTable_times) == "projectend"]   = "projectEnd"
  siteTable$projectStart = siteTable_times$projectStart
  siteTable$projectEnd   = siteTable_times$projectEnd
  return(siteTable)
}