#### reclassToBats -------------------------------------------------------
#' @title  integrate bat classification
#' @description  reclassifies echoes based on bat classification
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}
#' @param echoData echodata dataframe, output from extractDbData
#' @param batClassProbabilitiesAndMtrFactors probabilities of bat classification,
#' output from extractDbData' 
#' @param reclassToBatCutoff Threshold (0..1), classification of echoes with 
#' bat probability higher than reclassToBatCutoff will be set to 'bat'
#'
#' @return echoData dataframe
#' @export
#' @examples
#' \dontrun{
#' # Set server, database, and other input settings
#' # ===========================================================================
#'   dbServer       = "MACHINE\\SERVERNAME"     # Set the name of your SQL server
#'   dbName         = "db_Name"                   # Set the name of your database
#'   dbDriverChar   = "SQL Server"                # Set either "SQL Server" or "PostgreSQL"
#'   mainOutputDir  = file.path(".", "results")
#'   radarTimeZone  = "Etc/GMT0"
#'   targetTimeZone = "Etc/GMT0"
#'   listOfRfFeaturesToExtract = c(167, 168)
#'   siteLocation   = c(47.494427, 8.716432)
#'   sunOrCivil   = "civil"
#'  
#' # Get data
#' # ===========================================================================
#'   dbData = extractDbData(dbDriverChar                   = dbDriverChar,
#'                          dbServer                       = dbServer, 
#'                          dbName                         = dbName, 
#'                          saveDbToFile                   = TRUE,
#'                          dbDataDir                      = mainOutputDir,
#'                          radarTimeZone                  = radarTimeZone,
#'                          targetTimeZone                 = targetTimeZone,
#'                          listOfRfFeaturesToExtract      = listOfRfFeaturesToExtract,
#'                          siteLocation                   = siteLocation, 
#'                          sunOrCivil                   = sunOrCivil,
#'                          crepuscule                     = "nauticalSolar")
#' #' 
#' # Reclass To Bats
#' # ===========================================================================
#'   dbData$echoData = reclassToBats(echoData = dbData$echoData,
#'                                   batClassProbabilitiesAndMtrFactors = 
#'                                       dbData$batClassProbabilitiesAndMtrFactors,
#'                                   reclassToBatCutoff = 0.5)
#' }
reclassToBats = function( echoData = NULL,
                          batClassProbabilitiesAndMtrFactors = NULL,
                          reclassToBatCutoff = -1 )
{
  # reclass by bat probability
  if( !is.null( echoData )
      && !is.null( batClassProbabilitiesAndMtrFactors )
      && !is.null( reclassToBatCutoff ) 
      && is.numeric( reclassToBatCutoff )
      && reclassToBatCutoff >= 0
      && reclassToBatCutoff <= 1 )
  {
    if( nrow( batClassProbabilitiesAndMtrFactors ) == 0 )    
    {
	  stop( "no bat class probabilities, check database and settings")
    }
    
    echoDataTmp <- merge( echoData, batClassProbabilitiesAndMtrFactors, by = "echo", all.x =TRUE, all.y = FALSE )
    
    echoData[ !is.na( echoDataTmp$classProb.bat ) & echoDataTmp$classProb.bat > reclassToBatCutoff, ]$class = "bat"
    echoData[ !is.na( echoDataTmp$classProb.bat ) & echoDataTmp$classProb.bat > reclassToBatCutoff, ]$class_probability = echoDataTmp[ !is.na( echoDataTmp$classProb.bat ) & echoDataTmp$classProb.bat > reclassToBatCutoff, ]$classProb.bat
    echoData[ !is.na( echoDataTmp$classProb.bat ) & echoDataTmp$classProb.bat > reclassToBatCutoff, ]$mtr_factor_rf = echoDataTmp[ !is.na( echoDataTmp$classProb.bat ) & echoDataTmp$classProb.bat > reclassToBatCutoff, ]$MTRFact.bat    
  }  
  return( echoData )
}
