#### getProtocolTable ------------------------------------------------------------
#' @title  Get BirdScan protocol table
#' @description load protocol table from an already connect local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' it connects to cloud.birdradar.com
#'
#' @return A dataframe with the protocal table
#' @export
#'
getProtocolTable = function(dbConnection, dbDriverChar){
# load protocol table from local MS-SQL DB
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