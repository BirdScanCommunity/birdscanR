#### compileData -----------------------------------------------------------
#' @title compileData
#' @author Baptiste Schmid, \email{baptiste.schmid@@vogelwarte.ch}
#' @description The function \code{compileData} aim to filter database-extracts and 
#' save metadata used to compute MTR \code{computeMTR}. The function \code{compileData} 
#' is a list of filtered data and parameters. It takes the output from \code{extractDbData} 
#' and trunk the needed dataset to the restricted settings, e.g. time frame, pulse type.
#' @param echoData dataframe with the echo data from the data list created by 
#' the function \code{extractDBData}.
#' @param protocolData dataframe with the protocol data from the data list created by
#'  the function \code{extractDBData}. Echoes not detected during the listed protocols 
#'  will be excluded.
#' @param blindTimesData dataframe with the manual blind times created by 
#' the function \code{loadManualBlindTimes}. 
#' It include the automated blind times induced by changes in measurment protocol, 
#' and blind time added manually to remove periods of incoherent data collection.
#' @param radrSiteData
#' @param dbName Name of the database. Can be a useful meta data.
#' @param pulseTypeSelection character vector with the pulse types which should 
#' be included in the subset. Options: “S”, “M”, “L”, i.e. short-, medium-, long-pulse, respectively. 
#' Default is NULL: no filtering applied based on pulseType.
#' @param rotationSelection numeric vector to select the operation modes with and/or without
#'  antenna rotation. Options: 0, 1. (0 = no rotation, 1 = rotation). 
#'  Default is NULL: no filtering applied based on rotation mode.
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
#' @param echoValidator logical, If set to FALSE - default -, no additional filters 
#' is applied; if set to TRUE, echoes labelled by the echo validator as “non-bio scatterer” 
#' will be excluded.
#' @param filePath If given, the data-list is saved as RDS.
#' @param tagOutputFile Vector of two elements for prefix & suffix to file name, given 'filePath' is not NULL.
#' @param saveCSV if true, save tables as CSV in a folder nested in 'filePath'.
#' 
#' @return Returns filtered data table - echo, protocol, blindTimes, sunriseSunset, 
#' radarSite - and necessary parameters as input for \code{computeMTR}.
#' @export
#' @examples
#' \dontrun{
#'   
#' }
#' 
compileData = function( 
                      echoData           = NULL, 
                      protocolData       = NULL,
                      blindTimesData     = NULL,
                      sunriseSunsetData  = NULL,
                      radarSiteData      = NULL,
                      dbName             = NULL,
                      pulseTypeSelection = NULL, 
                      rotationSelection  = NULL,
                      timeRangeTargetTZ  = NULL,
                      targetTimeZone     = "Etc/GMT0",
                      classSelection     = NULL, 
                      classProbCutOff    = NULL, 
                      altitudeRange_AGL  = NULL, 
                      echoValidator      = FALSE,
                      filePath           = NULL,
                      tagOutputFile      = c(NULL, NULL),
                      saveCSV            = FALSE
                      ){
  
  # set the time window
  startTime = timeRangeTargetTZ[1]
  stopTime  = timeRangeTargetTZ[2]
  
  # Filter parameters
  # =============================================================================
  ls_filters <- list(
    timeRangeTargetTZ    = timeRangeTargetTZ,
    pulseTypeSelection   = pulseTypeSelection,
    rotationSelection    = rotationSelection,
    classSelection       = classSelection,
    classProbCutOff      = classProbCutOff,
    altitudeRange_AGL    = altitudeRange_AGL,
    echoValidator        = echoValidator
  )
  
  #-----------------------------------------------------------------------------
  # meta data
  metaFilters <- data.frame(
    "colname" = c('timeRangeTargetTZ',
                  'pulseTypeSelection',
                  'rotationSelection',
                  'classSelection',
                  'classProbCutOff',
                  'altitudeRange_AGL',
                  'echoValidator'
    ),
    "type" = c('POSIXct',
               'char',
               'integer',
               'char',
               'num',
               'num',
               'logical'
    ),
    "description" = c('Time range (from beginning to end) of the filter period, in the time zone used for analyses.',
                      'Pulse type is either "S" for Short-pulse, "M" for Medium pulse, or "L" for Long-pulse. See radar table for pulse duration',
                      'Rotation is either "0"when the antenna is static, or "1" if the antena is rotating on its vertical axis. Flight speed and direction available only if the anteanna is rotating',
                      'List of class - can be a subset of all avaialble classes',
                      'PostHoc filter on classification. if "0", all echoes are used, if e.g. 0.4, only echoes with a class probability >= 0.4 are kept.',
                      'Altitude range (agl) from the lowest to the highest.',
                      'If set to FALSE (default), no additional filters is applied; if set to TRUE, echoes labelled by the echo validator as “non-bio scatterer” will be excluded.'
    )
  )
  
  # Filter protocol data
  # =============================================================================
  protocolDataSubset = filterProtocolData(protocolData       = protocolData, 
                                          pulseTypeSelection = pulseTypeSelection, 
                                          rotationSelection  = rotationSelection)  
  TimesInd = (protocolDataSubset$startTime_targetTZ < stopTime) & 
    (protocolDataSubset$stopTime_targetTZ > startTime)
  protocolDataSubset =  protocolDataSubset[TimesInd, ]
  #-----------------------------------------------------------------------------
  # meta data for ProtocolData
  metaProtocol <- data.frame(
    "colname" = c("protocolID", "siteID",
                  "startTime_originTZ", "startTime_targetTZ", "stopTime_originTZ", "stopTime_targetTZ",
                  "pulseType", "rotate", "stc", "threshold",               
                  "softwareVersion" 
                  
    ),
    "type" = c('int', 'int', 
               'POSIXct','POSIXct','POSIXct','POSIXct',
               'char', 'int', 'num', 'num',
               'logi' # obviously missing information!?
               
    ),
    "description" = c('Incremental ID of measurement periods - linked to EchoData and BlindTimes.',
                      'Site ID - linked to the site & radar data.',
                      'Timestamp upon the start of the measurement period. TimeZone as given in DB',
                      'Timestamp upon the start of the measurement period. TimeZone defined by the user, since 2020 usually UTC',
                      'Timestamp upon the end of the measurement period. TimeZone as given in DB',
                      'Timestamp upon the end of the measurement period. TimeZone defined by the user, since 2020 usually UTC',
                      'Either "S" for Short-pulse, "M" for Medium pulse, "L" for Long-pulse. See radar table for pulse duration',
                      '"0"when the antenna is static, "1" if the antena is rotating on its vertical axis. Flight speed and direction available only if the anteanna is rotating',
                      'Sensitivity Time Control in meter. Bascially a distance to set the minial detected object size. Key feature to calcualte the MTR-factor of the echo.',
                      'Detection threshold in DBm. Key feature to calcualte the MTR-factor of the echo.',
                      'not Available. Software version updon detection. Can differ from the classifier versions'
    )
  )
  
  protocolDataSubset =  protocolDataSubset[, metaProtocol$colname]
  
  # Filter Site & Radar data
  # =============================================================================
  radarSiteData$targetTimeZone = targetTimeZone
  
  mycols_site <- c("radarID", "siteID", "siteCode", "siteName", "siteDesc",             
    "targetTimeZone", # "originTimeZone",
    "projectStart_originTZ", "projectStart_targetTZ", "projectEnd_originTZ", "projectEnd_targetTZ",
    "longitude", "latitude", "altitude",
    "customer"
  )
  # Select according to Pulse Type
  mycols_radar<- c("type", "serialNo", "northOffset", "delta", "tiltAngle",  
                   "transmitPower","antennaGainInDBi", "waveGuideAttenuation"
                   ) 
                   
  if(pulseTypeSelection == 'S') 
  {
    mycols_radar <- c(
      mycols_radar, 
      c("short0V", "shortSatLower", "shortSteepness", "shortSatUpper", "pulseLengthShort")
    )
  }
  if(pulseTypeSelection == 'M') 
  {
    mycols_radar <- c(
      mycols_radar, 
      c("medium0V", "mediumSatLower", "mediumSteepness", "mediumSatUpper", "pulseLengthMedium")
    )
  }
  if(pulseTypeSelection == 'L') 
  {
    mycols_radar <- c(
      mycols_radar, 
      c("long0V", "longSatLower", "longSteepness", "longSatUpper","pulseLengthLong")
    )
  }
  # filter variables
  radarSiteData <- radarSiteData[, c(mycols_site, mycols_radar)]
  # if( is.na(radarSiteData$timeShift) ) warning("The 'timeShift' parameter is missing. Edit the site table!")
 
  #-----------------------------------------------------------------------------
  # meta data
  metaRadarSiteData <- data.frame(
    "colname" = c("radarID", "siteID", "siteCode", "siteName", "siteDesc",             
                  "timeZone_targetTZ", # "timeZone_originTZ", 
                  "projectStart_originTZ", "projectStart_targetTZ", "projectEnd_originTZ", "projectEnd_targetTZ",
                  "longitude", "latitude", "altitude",
                  "customer",
                  "type", "serialNo", "northOffset", "delta", "tiltAngle",  
                  "transmitPower","antennaGainInDBi", "waveGuideAttenuation",
                  "xxx0V", "xxxSatLower", "xxxSteepness", "xxxSatUpper", "pulseLengthXxx"
    ),
    "type" = c('int',
               'int',
               'char',
               'char',
               'char',
               'char',
               'POSIXct',
               'POSIXct',
               'POSIXct',
               'POSIXct',
               'num',
               'num',
               'int',
               'char',
               'char',
               'int',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num'
               
    ),
    "description" = c('Serial number of radar unit - abrevaited.',
                      'Radar location: Site ID (integer) given by radar operator.',
                      'Radar location: Site code (three letters) given by radar operator.',
                      'Radar location: full name.',
                      'Radar location: optional further description',
                      'Time Zone used for analyses, usually UTC',
                      'Beginning of the data collection, using the time zone set on radar.',
                      'Beginning of the data collection, using the time zone set for the analyses - see variable "timeZone_targetTZ".',
                      'End of the data collection, using the time zone set on radar.',
                      'End of the data collection, using the time zone set for the analyses - see variable "timeZone_targetTZ".',
                      'Radar location: Longitude', # toDo: specify format
                      'Radar location: Latitude',
                      'Radar location: altitude above sea level',
                      'Radar operator',
                      'Model of radar unit, e.r. "BirdScan MR1" from Swiss Birdradar Solution.',
                      'Serial number of radar unit - full',
                      'Radar parameter: northOffset',
                      'Radar parameter: delta',
                      'Radar parameter: titltAngle - a contstant for BirdScan MR1.',
                      'Radar parameter: transmitted power [W] - can vary between years because of exchange of the magnetron.',
                      'Radar parameter: Antenna gain [dBi] is given by the antenna - a contstant for BirdScan MR1.',
                      'Radar parameter: Wave Guide attenuation []is given by the antenna - a contstant for BirdScan MR1.',
                      'Pulse type parameter: xxx0V - Calibration. ',
                      'Pulse type  parameter: xxxSatLower - Calibration.',
                      'Pulse type  parameter: xxxSteepness - Calibration.',
                      'Pulse type  parameter: xxxSatUpper - Calibration.',
                      'Pulse type  parameter: pulseLengthXxx - Calibration - duration of the pulse length. This value ultimately define the range resolution.'
                      
    )
  )
  
  
  # Filter blindTimes data
  # =============================================================================
  # restrict the time range
  if(!any( names(blindTimesData) == 'type') ) warning("The 'type' column is missing in the dataset 'blindTimesData'. Use the output of the function 'mergeVisibilityAnd ManualBlinfTime'.")
  TimesInd = (blindTimesData$start_targetTZ < stopTime) & 
    (blindTimesData$stop_targetTZ > startTime)
  blindTimesDataSubset =  blindTimesData[TimesInd, ]
  
  #-----------------------------------------------------------------------------
  # meta data
  metaBlindTimes <- data.frame(
    "colname" = c('type',
                  'start_targetTZ',
                  'stop_targetTZ',
                  'protocolID'    
    ),
    "type" = c('char',
               'POSIXct',
               'POSIXct',
               'char'
    ),
    "description" = c('Type of BlindTime. 
                          Blindtime is used to calcualte the effective duration of measurements during a teporal bin of the MTR table.
                          Common denominations are: 
                          "protocolChange" that include the blindtime subsequent to the start of a new measrurement period (protocolID),
                          "technical" denote periods with technical misfunction of the radar,
                          "rain" denote periods of precipitation.',
                      'Beginning of the blind period',
                      'End of the blind period',
                      'ID of measurement periods - linked to protocol table'
                      
    )
  )
  
  # need to wait for update of the function mergeVisibilityAndManualBlindTimes 
  # blindTimesDataSubset =  blindTimesDataSubset[, metaBlindTimes$colname]
  
  
  # Filter twilight data 
  # =============================================================================
  # restrict the time range on sunStart and sunStop
  TimesInd = (sunriseSunsetData$sunStart < stopTime) & 
    (sunriseSunsetData$sunStop > startTime)
  sunriseSunsetDataSubset =  sunriseSunsetData[TimesInd, ]
  # ToDo: use the twilight function if no dataset is included, but the site table include the necessary info on location.
  
  #-----------------------------------------------------------------------------
  # meta data
  metaSunriseSunset <- data.frame(
    "colname" = c("is_night", "date",
                  "sunStart", "sunStop",
                  "civilStart",  "civilStop",
                  "nauticalStart", "nauticalStop" 
                  
    ),
    "type" = c('int', 'POSIXct',
               'POSIXct','POSIXct',
               'POSIXct','POSIXct',
               'POSIXct','POSIXct'
               
    ),
    "description" = c('"0" if daytime, "1" if nighttime,',
                      "Date of event (in UTC)",
                      "Time of sunrise in UTC - see site table for location",
                      "Time of sunset in UTC - see site table for location",
                      "Time of dawn (civil-twilight, 6° below horizon) in UTC - see site table for location",
                      "Time of dusk (civil-twilight, 6° below horizon) in UTC - see site table for location",
                      "Time of dawn (nautical-twilight, 9° below horizon) in UTC - see site table for location",
                      "Time of dusk (nautical-twilight, 9° below horizon) in UTC - see site table for location"
                      
    )
  )
  
  sunriseSunsetDataSubset =  sunriseSunsetDataSubset[, metaSunriseSunset$colname]
  
  
  # Filter echo data
  # =============================================================================
  echoDataSubset = filterEchoData(echoData          = echoData, 
                                  timeRangeTargetTZ = timeRangeTargetTZ, 
                                  targetTimeZone    = targetTimeZone,
                                  protocolData      = protocolDataSubset, 
                                  classSelection    = classSelection, 
                                  classProbCutOff   = classProbCutOff, 
                                  altitudeRange_AGL = altitudeRange_AGL, 
                                  manualBlindTimes  = blindTimesDataSubset[which(blindTimesDataSubset$type != "protocolChange"), ], 
                                  echoValidator     = echoValidator) 
  
  #-----------------------------------------------------------------------------
  # meta data
  metaEcho = NULL
  # metaEcho <- data.frame(
  #   "colname" = c("dummy" 
  #                 
  #   ),
  #   "type" = c('dummy'
  #              
  #   ),
  #   "description" = c('dummy'
  #                     
  #   )
  # )
  
  if(!is.null(metaEcho)){
    # subset to target columns
     echoDataSubset =  echoDataSubset[, metaEcho$colname]
  } 
  
  
 
  # compile meta data into a list
  # =============================================================================
  ls_metaData <- list(
    echoData          = metaEcho,
    protocolData      = metaProtocol,
    blindTimesData    = metaBlindTimes,
    sunriseSunsetData = metaSunriseSunset,
    radarSiteData     = metaRadarSiteData,
    filterParameters  = metaFilters,
    database          = dbName, # at the moment, only keep the name of the database, but additional information could be used: version of BirdscanR-package, name of the person who extracted the data, etc.
    birdscanR         = packageVersion("birdScanR") # classifier version is included in the echo-dataset
  )
  

  
  # Return the filtered protocol and echo data
  # =============================================================================
  compiledData = list(
    echoData           = echoDataSubset,
    protocolData       = protocolDataSubset,
    blindTimesData     = blindTimesDataSubset,
    sunriseSunsetData  = sunriseSunsetDataSubset,
    radarSiteData      = radarSiteData,
    filterParameters   = ls_filters,
    metaData           = ls_metaData
  )
  
  
  # save output
  if( !is.null(filePath) && length(filePath) == 1){
    # =============================================================================
    # create filename to save plot
    # =========================================================================
    fileName = "compiledData"
    
    # Add prefix from tagOutputFile to fileName
    # =========================================================================
    if (!is.null(tagOutputFile[1]) && length(tagOutputFile) == 2){
      prefix = tagOutputFile[1]     
      fileName = paste(prefix, fileName, sep = "_")
    }     
    
    # dbName for fileName
    # =========================================================================
    if (!is.null(dbName) && length(dbName) == 1){
        fileName = paste(fileName, dbName, sep = "_")
    } 
    
    # time range for fileName
    # =========================================================================
    if (!is.null(timeRangeTargetTZ) && length(timeRangeTargetTZ) == 2){
      startTime = format(timeRangeTargetTZ[1], "%Y%m%d")
      stopTime = format(timeRangeTargetTZ[2], "%Y%m%d")
      time = paste("time", startTime, "to", stopTime, sep = "")
      fileName = paste(fileName, time, sep = "_")
    } 
    
    # altitude range for fileName
    # =========================================================================
    if (!is.null(altitudeRange_AGL) && length(altitudeRange_AGL) == 2){
      altitudeRangeStart = altitudeRange_AGL[1]
      altitudeRangeStop = paste0(altitudeRange_AGL[2], "m")
      altitude = paste("alt", altitudeRangeStart, "to", altitudeRangeStop, sep = "")
      fileName = paste(fileName, altitude, sep = "_")
    }
    
    # pulseTypeSelection for fileName
    # =========================================================================
    if (!is.null(pulseTypeSelection) && length(pulseTypeSelection) == 1){
      pulseTypeSelection_char = paste( sort(pulseTypeSelection), collapse = "")
      pulseTypeSelection_char = paste0('pulse', pulseTypeSelection_char, sep ="")    
      fileName = paste(fileName, pulseTypeSelection_char, sep = "_")
    }     
    
    # rotationSelection for fileName
    # =========================================================================
    if ( !is.null(rotationSelection) && any(rotationSelection %in% c(1, 0)) ){
      rotationSelection_char = paste( sort(rotationSelection), collapse = "")
      rotationSelection_char = paste0('rotation', rotationSelection_char, sep ="")    
      fileName = paste(fileName, rotationSelection_char, sep = "_")
    }     
    
    # classSelection for fileName
    # =========================================================================
    if (!is.null(classSelection)){
      classAbbreviations$class <- trimws(classAbbreviations$class, which = "right")
      classAbbreviations$abbr <- trimws(classAbbreviations$abbr, which = "right")
      classes = paste(classAbbreviations$abbr[which(classAbbreviations$class %in% 
                                                      classSelection)], 
                      collapse = "")
      fileName = paste(fileName, classes, sep = "_")
    } else {
      fileName = paste(fileName, "allClasses", sep = "_")
    }
    
    # classProbCutOff for fileName
    # =========================================================================
    if (!is.null(classProbCutOff) && length(classProbCutOff) == 1){
      classProbCutOff_char <- substr(classProbCutOff, 3, 4)
      classProbCutOff_char = paste0('classProbCutOff.', classProbCutOff_char, sep ="")    
      fileName = paste(fileName, classProbCutOff_char, sep = "_")
    }     
    
    # echoValidator for fileName
    # =========================================================================
    if (echoValidator && length(echoValidator) == 1){
      echoValidator_char = paste0('echoValidator', echoValidator, sep ="")    
      fileName = paste(fileName, echoValidator_char, sep = "_")
    }     
    
    # Add suffix from tagOutputFile to fileName
    # =========================================================================
    if (!is.null(tagOutputFile[2]) && length(tagOutputFile) == 2){
      suffix = tagOutputFile[2]     
      fileName = paste(fileName, suffix, sep = "_")
    }     
 
    # save RDS
    # =========================================================================
    rdsFileName = paste0(fileName, ".rds")
    
    # add output folder 'filePath'
    rdsFilePathName <- file.path(filePath, rdsFileName)# ; print(filePathName)
   
    saveRDS(compiledData, file = rdsFilePathName)
    
    # Save CSV
    # =========================================================================
    if(saveCSV  && length(filePath) == 1){
      csvDirPath = file.path(filePath, fileName)
      
      # Create a directory to store the CSV files (optional)
      dir.create(csvDirPath, showWarnings = FALSE)
   
      # Loop through each element in the list
      # Loop through each element in the list
      for (name in names(compiledData)) {
        if (name %in% c("filterParameters", "metaData")) {
          # Save as YAML for list elements
          file_path <- file.path(csvDirPath, paste0(name, ".yaml"))
          yaml::write_yaml(compiledData[[name]], file = file_path)
        } else {
          # Save as CSV for table elements
          file_path <- file.path(csvDirPath, paste0(name, ".csv"))
          write.csv(compiledData[[name]], file = file_path, row.names = FALSE)
        }
      }
      
    }
   
  } # end of if( !is.null(filePath) && length(filePath) == 1){

  return(compiledData)
}