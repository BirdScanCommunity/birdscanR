#### filterData -----------------------------------------------------------
#' @title filterData
#' @author Birgen Haest, \email{birgen.haest@@vogelwarte.ch} 
#' @description With the function \code{filterData} both the echo and protocol data can be filtered by several parameters. The function returns the filtered echo and protocol data.
#'
#' @param echoData dataframe with the echo data from the data list created by the function \code{extractDBData}.
#' @param protocolData dataframe with the protocol data from the data list created by the function \code{extractDBData} or a subset of it created by the function \code{filterProtocolData}. Echoes not detected during the listed protocols will be excluded.
#' @param pulseTypeSelection character vector with the pulse types which should be included in the subset. Options: “S”, “M”, “L” (short-, medium-, long-pulse). Default is NULL: no filtering applied based on pulseType.
#' @param rotationSelection numeric vector to select the operation modes with and/or without antenna rotation. Options: 0, 1. (0 = no rotation, 1 = rotation). Default is NULL: no filtering applied based on rotation mode.
#' @param timeRangeTargetTZ Character vector of length 2, with start and end of 
#' time range, formatted as "%Y-%m-%d %H:%M". Echoes outside the time range will
#'  be excluded.
#' @param targetTimeZone "Etc/GMT0" String specifying the target time zone. 
#' Default is "Etc/GMT0".
#' @param classSelection character string vector with the classes that should be 
#' included.
#' @param classProbCutOff numeric cutoff value for class probabilities. Echoes 
#' with a lower class probability will be excluded.
#' @param altitudeRange_AGL numeric vector of length 2 with start and end of the 
#' altitude range. Echoes outside the altitude range will be excluded.
#' @param manualBlindTimes dataframe with the manual blind times created by the 
#' function \code{loadManualBlindTimes}.
#' @param echoValidator logical, if set to TRUE, echoes labelled by the echo 
#' validator as “non-bio scatterer” will be excluded. If set to FALSE, all 
#' echoes are included.
#' 
#' @return returns the filtered echo and protocol data in the same format as 
#' provided in the parameters \code{echoData} and \code{protocolData}.
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
#'   timeRangeData        = c("2021-01-15 00:00", "2021-01-31 00:00")
#'   classSelection       = c("passerine_type", "wader_type", "swift_type", 
#'                            "large_bird", "unid_bird", "bird_flock")
#'   classProbCutoff      = NULL
#'   altitudeRange        = c(50, 1000)
#'   data(manualBlindTimes)
#'   cManualBlindTimes    = manualBlindTimes
#'   useEchoValidator     = FALSE
#' 
#' # Filter the data
#' # ===========================================================================
#'   filteredData = filterData(echoData           = dbData$echoData,
#'                             protocolData       = dbData$protocolData, 
#'                             pulseTypeSelection = pulseLengthSelection, 
#'                             rotationSelection  = rotationSelection,
#'                             timeRangeTargetTZ  = timeRangeData,
#'                             targetTimeZone     = targetTimeZone,
#'                             classSelection     = classSelection, 
#'                             classProbCutOff    = classProbCutoff, 
#'                             altitudeRange_AGL  = altitudeRange, 
#'                             manualBlindTimes   = cManualBlindTimes, 
#'                             echoValidator      = useEchoValidator)   
#' }
#' 
filterData = function(echoData           = NULL, 
                      protocolData       = NULL,
                      pulseTypeSelection = NULL, 
                      rotationSelection  = NULL,
                      timeRangeTargetTZ  = NULL,
                      targetTimeZone    = "Etc/GMT0",
                      classSelection     = NULL, 
                      classProbCutOff    = NULL, 
                      altitudeRange_AGL  = NULL, 
                      manualBlindTimes   = NULL, 
                      echoValidator      = FALSE){
# Start filtering protocol data
# =============================================================================
  message("Filtering protocol Data..")

# Filter protocol data
# =============================================================================
  protocolDataSubset = filterProtocolData(protocolData       = protocolData, 
                                          pulseTypeSelection = pulseTypeSelection, 
                                          rotationSelection  = rotationSelection)  
  
# Start filtering echo data
# =============================================================================
  message("Filtering echo Data..")

# Filter echo data
# =============================================================================
  echoDataSubset = filterEchoData(echoData          = echoData, 
                                  timeRangeTargetTZ = timeRangeTargetTZ, 
                                  targetTimeZone    = targetTimeZone,
                                  protocolData      = protocolDataSubset, 
                                  classSelection    = classSelection, 
                                  classProbCutOff   = classProbCutOff, 
                                  altitudeRange_AGL = altitudeRange_AGL, 
                                  manualBlindTimes  = manualBlindTimes, 
                                  echoValidator     = echoValidator) 
  
# Return the filtered protocol and echo data
# =============================================================================
  filteredData = list(echoData     = echoDataSubset,
                      protocolData = protocolDataSubset)
  return(filteredData)
}