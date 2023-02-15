#### getTimeBinsTable ----------------------------------------------------------
#' @title  Get BirdScan timebins table
#' @description load timebins table from an already connected 'Birdscan MR1' 'SQL' database
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' it connects to cloud.birdradar.com
#'
#' @return A dataframe with the timebins table
#' @export
#' @examples
#' \dontrun{
#' # Set server and database settings
#' # =============================================================================
#'   dbServer       = "MACHINE\\\\SERVERNAME"     # Set the name of your SQL server
#'   dbName         = "db_Name"                   # Set the name of your database
#'   dbDriverChar   = "SQL Server"                # Set either "SQL Server" or "PostgreSQL"
#'
#' # Open the connection with the database
#' # =============================================================================
#'   dsn = paste0("driver=", dbDriverChar, ";server=", dbServer,
#'                ";database=", dbName,
#'                ";uid=", rstudioapi::askForPassword("Database user"),
#'                ";pwd=", rstudioapi::askForPassword("Database password"))
#'   dbConnection = RODBC::odbcDriverConnect(dsn)
#'
#' timeBinsTable = getTimeBinsTable(dbConnection, dbDriverChar)
#' }
#'
getTimeBinsTable = function(dbConnection, dbDriverChar){
  # load protocol table from local MS-SQL DB
  # ==============================================================================
    if (dbDriverChar != 'PostgreSQL'){
      timeBinsTable            = QUERY(dbConnection, dbDriverChar, 
                                       "Select * From time_bins order by id asc")
      timeBinsTable_times      = QUERY(dbConnection, 
                                       dbDriverChar, 
                                       "Select time_start, time_stop From time_bins order by id asc", 
                                       as.is = TRUE)
      timeBinsTable$time_start = timeBinsTable_times$time_start
      timeBinsTable$time_stop  = timeBinsTable_times$time_stop
  
  # load protocol table from PostGreSQL
  # ==============================================================================
    } else {
      timeBinsTable            = QUERY(dbConnection, 
                                       dbDriverChar, 
                                       "Select *,time_start::character varying as start,time_stop::character varying as stop FROM time_bins order by id asc")
      timeBinsTable$time_start = timeBinsTable$start
      timeBinsTable$time_stop  = timeBinsTable$stop
      timeBinsTable$start      = NULL
      timeBinsTable$stop       = NULL
    }
  
  colnames(timeBinsTable)[colnames(timeBinsTable) == "siteid"] = "siteID"
  return(timeBinsTable)
}