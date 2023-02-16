#### getProtocolTable ------------------------------------------------------------
#' @title  Get BirdScan protocol table
#' @description load protocol table from an already connected 'Birdscan MR1' 'SQL' database
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' 
#' it connects to cloud.birdradar.com
#'
#' @return A dataframe with the protocol table
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
#' protocolTable = getProtocolTable(dbConnection, dbDriverChar)
#' }
#'
getProtocolTable = function(dbConnection, dbDriverChar){
# load protocol table from MS-SQL DB
# ============================================================================
  if (dbDriverChar != 'PostgreSQL'){
    protocolTable = QUERY(dbConnection, dbDriverChar, "Select * From protocol order by protocolID asc")
    colnames(protocolTable)[colnames(protocolTable) == "starttime"] = "startTime"
    colnames(protocolTable)[colnames(protocolTable) == "stoptime"]  = "stopTime"
      
    protocolTable_times = QUERY(dbConnection, dbDriverChar, "Select startTime, stopTime From protocol order by protocolID asc", as.is = TRUE)
    colnames(protocolTable_times)[colnames(protocolTable_times) == "starttime"] = "startTime"
    colnames(protocolTable_times)[colnames(protocolTable_times) == "stoptime"]  = "stopTime"
    protocolTable$startTime = protocolTable_times$startTime
    protocolTable$stopTime  = protocolTable_times$stopTime
   
# load protocol table from PostGreSQL
# ============================================================================ 
  } else {
    protocolTable = QUERY(dbConnection, 
                          dbDriverChar, 
                          "Select *,starttime::character varying as start,stoptime::character varying as stop From protocol order by protocolid asc")
    protocolTable$starttime = protocolTable$start
    protocolTable$stoptime  = protocolTable$stop
    colnames(protocolTable)[colnames(protocolTable) == "starttime"] = "startTime"
    colnames(protocolTable)[colnames(protocolTable) == "stoptime"]  = "stopTime"
    protocolTable$start = NULL
    protocolTable$stop  = NULL
  }
   
# Rename columns
# ============================================================================
  colnames(protocolTable)[colnames(protocolTable) == "protocolid"]              = "protocolID"
  colnames(protocolTable)[colnames(protocolTable) == "protocolname"]            = "protocolName"
  colnames(protocolTable)[colnames(protocolTable) == "siteid"]                  = "siteID"
  colnames(protocolTable)[colnames(protocolTable) == "pulsetype"]               = "pulseType"
  colnames(protocolTable)[colnames(protocolTable) == "hystfact"]                = "hystFact"
  colnames(protocolTable)[colnames(protocolTable) == "autothreshold"]           = "autoThreshold"
  colnames(protocolTable)[colnames(protocolTable) == "visibilitythreshold"]     = "visibilityThreshold"
  colnames(protocolTable)[colnames(protocolTable) == "blocktime"]               = "blockTime"
  colnames(protocolTable)[colnames(protocolTable) == "maxmatcherror"]           = "maxMatchError"
  colnames(protocolTable)[colnames(protocolTable) == "measurementnoisefactor"]  = "measurementNoiseFactor"
  colnames(protocolTable)[colnames(protocolTable) == "clutterrejectiondb"]      = "clutterRejectionDB"
  colnames(protocolTable)[colnames(protocolTable) == "clutterrejectionseconds"] = "clutterRejectionSeconds"
  colnames(protocolTable)[colnames(protocolTable) == "clutterfactor"]           = "clutterFactor"
  
  return(protocolTable)
}