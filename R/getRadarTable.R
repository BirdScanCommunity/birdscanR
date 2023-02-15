#### getRadarTable ------------------------------------------------------------
#' @title  Get a BirdScan radar table
#' @description  get the Radar table from  an already connected DB and rename the columns appropiately
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. 
#'
#' @return the radar table  as a data frame
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
#' radarTable = getRadarTable(dbConnection, dbDriverChar)
#' }
#'
getRadarTable = function(dbConnection, dbDriverChar){
  radarTable = QUERY(dbConnection, dbDriverChar, "Select * From radar")
   
  colnames(radarTable)[colnames(radarTable) == "radarid"]              = "radarID"
  colnames(radarTable)[colnames(radarTable) == "serialno"]             = "serialNo"
  colnames(radarTable)[colnames(radarTable) == "northoffset"]          = "northOffset"
  colnames(radarTable)[colnames(radarTable) == "short0v"]              = "short0V"
  colnames(radarTable)[colnames(radarTable) == "medium0v"]             = "medium0V"
  colnames(radarTable)[colnames(radarTable) == "long0v"]               = "long0V"
  colnames(radarTable)[colnames(radarTable) == "shortsteepness"]       = "shortSteepness"
  colnames(radarTable)[colnames(radarTable) == "mediumsteepness"]      = "mediumSteepness"
  colnames(radarTable)[colnames(radarTable) == "longsteepness"]        = "longSteepness"
  colnames(radarTable)[colnames(radarTable) == "shortsatlower"]        = "shortSatLower"
  colnames(radarTable)[colnames(radarTable) == "mediumsatlower"]       = "mediumSatLower"
  colnames(radarTable)[colnames(radarTable) == "longsatlower"]         = "longSatLower"
  colnames(radarTable)[colnames(radarTable) == "antennagainindbi"]     = "antennaGainInDBi"
  colnames(radarTable)[colnames(radarTable) == "waveguideattenuation"] = "waveGuideAttenuation"
  colnames(radarTable)[colnames(radarTable) == "pulselengthshort"]     = "pulseLengthShort"
  colnames(radarTable)[colnames(radarTable) == "pulselengthmedium"]    = "pulseLengthMedium"
  colnames(radarTable)[colnames(radarTable) == "pulselengthlong"]      = "pulseLengthLong"
  colnames(radarTable)[colnames(radarTable) == "tiltangle"]            = "tiltAngle"
  colnames(radarTable)[colnames(radarTable) == "transmitpower"]        = "transmitPower"
  
  return(radarTable)
}