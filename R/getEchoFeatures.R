#### getEchoFeatures -----------------------------------------------------------
#' @title  Get BirdScan echo features
#' @description load echo rffeature map from 'Birdscan MR1' 'SQL' database
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' 
#' it connects to cloud.birdradar.com
#' @param listOfRfFeaturesToExtract a list of feature to extract
#'
#' @return A list of the features extracted
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
#' # Set list of Rf features you also want to extract
#' # Vector with RF features to extract. Feature IDs can be found in the  
#' # 'rfFeatures' table in the sql database.  
#' # Example: Get wing beat frequency and credibility: c(167, 168)
#' # Set to NULL to not extract any.
#' # ===========================================================================
#'   listOfRfFeaturesToExtract = c(167, 168) 
#'
#' echoFeatures = getEchoFeatures(dbConnection, dbDriverChar, 
#'                                listOfRfFeaturesToExtract)
#' }
#'
getEchoFeatures = function(dbConnection, dbDriverChar, listOfRfFeaturesToExtract){
  # load echo 'rfFeatures' table from 'MS-SQL' database
  # ===========================================================================
    rffeaturesTable = QUERY(dbConnection, dbDriverChar, 
                            "Select * From rffeatures")
   
  # load echo_rffeature_map table from 'MS-SQL' database
  # =============================================================================
    if(!is.null(listOfRfFeaturesToExtract)){
      echorffeaturesMapTable = QUERY(dbConnection, 
                                     dbDriverChar, 
                                     paste("Select * From echo_rffeature_map where feature in ( ", 
                                           paste(listOfRfFeaturesToExtract, collapse = ", "), " )"))   
      
      featurelist                    = echorffeaturesMapTable$feature
      echorffeaturesMapTable$feature = rffeaturesTable$name[match(featurelist, rffeaturesTable$id)]
      echoRfFeatureMap               = reshape2::dcast(echorffeaturesMapTable, 
                                                       echo ~ feature, 
                                                       value.var = "value", 
                                                       fun.aggregate = mean)
      
      return(list(echoRfFeatureMap = echoRfFeatureMap, rfFeatures = rffeaturesTable))
    }
    
  return(list(echoRfFeatureMap = NULL, rfFeatures = rffeaturesTable))
}