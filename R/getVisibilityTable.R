#### getVisibilityTable --------------------------------------------------------
#' @title  Get BirdScan visibility table
#' @description load visibility table from an already connected 'Birdscan MR1' 
#' 'SQL' database
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' 
#' it connects to cloud.birdradar.com
#'
#' @return A dataframe with the visibility table
#' @export
#' 
#' @examples
#' \dontrun{
#' # Set server and database settings
#' # ===========================================================================
#'   dbServer       = "MACHINE\\SERVERNAME" # Set the name of your SQL server
#'   dbName         = "db_Name"               # Set the name of your database
#'   dbDriverChar   = "SQL Server"            # Set either "SQL Server" or "PostgreSQL"
#'
#' # Open the connection with the database
#' # ===========================================================================
#'   dsn = paste0("driver=", dbDriverChar, ";server=", dbServer,
#'                ";database=", dbName,
#'                ";uid=", rstudioapi::askForPassword("Database user"),
#'                ";pwd=", rstudioapi::askForPassword("Database password"))
#'   dbConnection = RODBC::odbcDriverConnect(dsn)
#'
#' visibilityTable = getVisibilityTable(dbConnection, dbDriverChar)
#' }
#'
getVisibilityTable = function(dbConnection, dbDriverChar){
  # load protocol table from local MS-SQL DB
  # ============================================================================
    if (dbDriverChar != 'PostgreSQL'){
      visibilityTable            = QUERY(dbConnection, 
                                         dbDriverChar, 
                                         paste0("Select * From visibility ", 
                                                "order by visibilityLogID asc"))
      visibilityTable_times      = QUERY(dbConnection, 
                                         dbDriverChar, 
                                         paste0("Select blind_from, blind_to ", 
                                                "From visibility order by ", 
                                                "visibilityLogID asc"), 
                                         as.is = TRUE)
      visibilityTable$blind_from = visibilityTable_times$blind_from
      visibilityTable$blind_to   = visibilityTable_times$blind_to
      
  # load protocol table from PostGreSQL
  # ============================================================================
    } else {
      visibilityTable            = QUERY(dbConnection, 
                                         dbDriverChar, 
                                         paste0("Select *,blind_from::character", 
                                                " varying as ", 
                                                "blindfrom,blind_to::character", 
                                                " varying as blindto From ", 
                                                "visibility order by ", 
                                                "visibilitylogid asc"))
      visibilityTable$blind_from = visibilityTable$blindfrom 
      visibilityTable$blind_to   =  visibilityTable$blindto 
      visibilityTable$blindfrom  = NULL
      visibilityTable$blindto    =  NULL
    }
  
  
    colnames(visibilityTable)[colnames(visibilityTable) == "visibilitylogid"] = 
      "visibilityLogID"
    colnames(visibilityTable)[colnames(visibilityTable) == "protocolid"]      = 
      "protocolID"
    return(visibilityTable)
}