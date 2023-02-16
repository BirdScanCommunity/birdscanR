#### filterProtocolData ------------------------------------------------------
#' @title filterProtocolData
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch} 
#' @description With the function \code{filterProtocolData} the protocol data 
#' can be filtered by the operation mode (pulse-type and antenna rotation). The 
#' function returns the filtered subset of the protocol data which can later be 
#' used to filter the echoes based on the operation mode/protocol
#' 
#' @param protocolData dataframe with the protocol data from the data list 
#' created by the function \code{extractDBData}
#' @param pulseTypeSelection character vector with the pulse types which should 
#' be included in the subset. Options: “S”, “M”, “L” (short-, medium-, 
#' long-pulse). Default is NULL: no filtering applied based on pulseType.
#' @param rotationSelection numeric vector to select the operation modes with 
#' and/or without antenna rotation. Options: 0, 1. (0 = no rotation, 
#' 1 = rotation). Default is NULL: no filtering applied based on rotation mode.
#'
#' @return returns the filtered protocol data in the same format as provided in 
#' the parameter \code{protocolData}.
#' @export
#' @examples
#' \dontrun{
#' # Set server, database, and other input settings for data extraction
#' # ===========================================================================
#'   dbServer       = "MACHINE\\\\SERVERNAME"     # Set the name of your SQL server
#'   dbName         = "db_Name"                   # Set the name of your database
#'   dbDriverChar   = "SQL Server"                # Set either "SQL Server" or "PostgreSQL"
#'   mainOutputDir  = file.path(".", "results")
#'   radarTimeZone  = "Etc/GMT0"
#'   targetTimeZone = "Etc/GMT0"
#'   listOfRfFeaturesToExtract = c(167, 168)
#'   siteLocation   = c(47.494427, 8.716432)
#'   sunOrCivil     = "civil"
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
#'                          sunOrCivil                     = sunOrCivil)
#'                          
#' # Set input settings for filtering of the data
#' # ===========================================================================
#'   pulseLengthSelection = "S"
#'   rotationSelection    = 1
#' 
#' # Filter the echo data
#' # ===========================================================================
#'   filteredProtocolData = filterProtocolData(protocolData       = dbData$protocolData,
#'                                             pulseTypeSelection = pulseLengthSelection, 
#'                                             rotationSelection  = rotationSelection)   
#' }
#' 
filterProtocolData = function(protocolData       = NULL, 
                              pulseTypeSelection = NULL, 
                              rotationSelection  = NULL){
  # Check whether 'protocolData' is not empty
  # ===========================================================================
    if ((is.null(protocolData)) || (nrow(protocolData) == 0)){
      warning("protocolData does not contain any data, so it cannot be filtered..")
      return(protocolData)
    }
    
  # Check whether the necessary protocol columns are present in the protocolData
  # ===========================================================================
    requiredProtocolDataCols = c("pulseType", "rotate", 
                                 "startTime_targetTZ", "stopTime_targetTZ")
    if (!(all(requiredProtocolDataCols %in% names(protocolData)))){
      warning(paste0("protocolData does not contain all of the necessary columns, ", 
                     "so it cannot be filtered. Please check the protocol ", 
                     "table in your database for the following columns: ", 
                     paste(requiredProtocolDataCols, collapse = ", "), "."))
      return(protocolData)
    }
  
  # Exclude invalid times
  # =========================================================================
    validTimesInd = (protocolData$startTime_targetTZ > "1950-01-01") & 
                     (protocolData$stopTime_targetTZ > "1950-01-01")
    protocolData  = protocolData[validTimesInd,]
  
  # Subset protocolData by pulseLength, if requested
  # =========================================================================
    if (!is.null(pulseTypeSelection)){
      protocolData = protocolData[protocolData$pulseType %in% pulseTypeSelection,]
    }
  
  # Subset protocolData by rotation mode, if requested
  # =========================================================================
    if (!is.null(rotationSelection)){
      protocolData = protocolData[protocolData$rotate %in% rotationSelection,]
    }
  
  # Return filtered protocol data
  # ===========================================================================
    return(protocolData)
}