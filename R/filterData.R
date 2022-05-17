#### filterProtocolData ------------------------------------------------------
#' @title filterProtocolData
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}
#' @description With the function \code{filterProtocolData} the protocol data can be filtered by the operation mode (pulse-type and antennarotation). The function returns the filtered subset of the protocoldata which can later be used to filter the echoes based on the operation mode/protocol
#' 
#' @param protocolData=NULL dataframe with the protocol data from the data list created by the function \code{extractDBData}
#' @param pulseTypeSelection=NULL character vector with the pulse types which should be included in the subset. Options: “S”, “M”, “L” (short-, medium-, long-pulse)
#' @param rotationSelection=NULL numeric vector to select the operation modes with and/or without antennarotation. Options: 0, 1. (0 = no rotation, 1 = rotation)
#'
#' @return returns the filtered protocol data in the same format as provided in the parameter \code{protocolData}.
#' @export

filterProtocolData = function(protocolData = NULL, pulseTypeSelection = NULL, rotationSelection = NULL){
  requiredProtocolDataCols <- c("pulseType", "rotate", "startTime_targetTZ", "stopTime_targetTZ")
  if (!is.null(protocolData) && length(protocolData[, 1]) > 0 && sum(requiredProtocolDataCols %in% names(protocolData)) == length(requiredProtocolDataCols)) 
  {
    # exclude invalid times
    protocolData <- protocolData[protocolData$startTime_targetTZ > "1950-01-01" & protocolData$stopTime_targetTZ > "1950-01-01",]
    
    # subset protocolData by pulseLength
    if (!is.null(pulseTypeSelection))
    {
      protocolData <- protocolData[protocolData$pulseType %in% pulseTypeSelection,]
    }
    
    # subset protocolData by rotation mode
    if (!is.null(rotationSelection))
    {
      protocolData <- protocolData[protocolData$rotate %in% rotationSelection,]
    }
  }
  
  return(protocolData)
}

#### filterEchoData ------------------------------------------------------
#' @title filterEchoData
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}
#' @description With the function \code{filterEchoData} the echo data can be filtered by several parameters. The function returns the filtered echo data.
#'
#' @param echoData=NULL dataframe with the echo data from the data list created by the function \code{extractDBData}.
#' @param timeRangeTargetTZ=NULL POSIXct vector of length 2 with start and end time with target timezone. Echoes outside the time range will be excluded.
#' @param protocolData=NULL dataframe with the protocol data from the data list created by the function \code{extractDBData} or a subset of it created by the function \code{filterProtocolData}. Echoes not detected during the listed protocols will be excluded.
#' @param classSelection=NULL character string vector with the classes that should be included.
#' @param classProbCutOff=NULL numeric cutoff value for class probabilities. Echoes with a lower class probability will be excluded.
#' @param altitudeRange_AGL=NULL numeric vector of length 2 with start and end of the altitude range. Echoes outside the altitude range will be excluded.
#' @param manualBlindTimes=NULL dataframe with the manual blind times created by the function \code{loadManualBlindTimes}.
#' @param echoValidator=NULL logical, if set to TRUE, echoes labelled by the echo validator as “non-bio scatterer” will be excluded. If set to FALSE, all echoes are included.
#' 
#' @return returns the filtered echo data in the same format as provided in the parameter \code{echoData}.
#' @export
filterEchoData = function(echoData = NULL, timeRangeTargetTZ = NULL, protocolData = NULL, classSelection = NULL, classProbCutOff = NULL, altitudeRange_AGL = NULL, manualBlindTimes = NULL, echoValidator = FALSE)
{
  requiredEchoDataCols <- c("time_stamp_targetTZ", "protocolID", "class", "class_probability", "feature1.altitude_AGL")
  if (!is.null(echoData) && length(echoData[, 1]) > 0 && sum(requiredEchoDataCols %in% names(echoData)) == length(requiredEchoDataCols)) 
  {
    # filter by timerange
    if (!is.null(timeRangeTargetTZ) && length(timeRangeTargetTZ) == 2 && is(timeRangeTargetTZ, "POSIXct"))
    {
      echoData <- echoData[echoData$time_stamp_targetTZ > timeRangeTargetTZ[1] & echoData$time_stamp_targetTZ < timeRangeTargetTZ[2],]
    }
    
    # filter by protocols
    if (!is.null(protocolData) && length(protocolData) > 0 && sum(c ("protocolID") %in% names(protocolData)) == 1)
    {
      echoData <- echoData[echoData$protocolID %in% protocolData$protocolID,]
    }
    
    # filter by classes
    if (!is.null(classSelection) && is.character(classSelection))
    {
      echoData <- echoData[echoData$class %in% classSelection,]
    }
    
    # filter by classprobability
    if (!is.null(classProbCutOff) && is.numeric(classProbCutOff))
    {
      echoData <- echoData[echoData$class_probability > classProbCutOff,]
    }
    
    # filter by altitudeRange
    if (!is.null(altitudeRange_AGL) && length(altitudeRange_AGL) == 2 && is.numeric(altitudeRange_AGL))
    {
      echoData <- echoData[echoData$feature1.altitude_AGL > altitudeRange_AGL[1] & echoData$feature1.altitude_AGL < altitudeRange_AGL[2],]
    }
    
    # filter by manualBlindTimes
    if (!is.null(manualBlindTimes) && sum(c ("start_targetTZ", "stop_targetTZ") %in% names(manualBlindTimes)) == 2 && is(manualBlindTimes$start_targetTZ, "POSIXct") && is(manualBlindTimes$stop_targetTZ, "POSIXct"))
    {
      echoDataInBlindTime <- rep(FALSE, length(echoData[, 1]))
      for (i in 1 : length(manualBlindTimes[, 1]))
      {
        echoDataInBlindTime <- echoDataInBlindTime | (echoData$time_stamp_targetTZ >= manualBlindTimes$start_targetTZ[i] & echoData$time_stamp_targetTZ <= manualBlindTimes$stop_targetTZ[i])
      }
      echoData <- echoData[!echoDataInBlindTime,]
    }
    
    # filter by echovalidator
    if (echoValidator == TRUE)
    {
      echoData <- echoData[echoData$echoValidationType == "bio scatterer" | is.na(echoData$echoValidationType),]
    }
    
  }
  
  return(echoData)
}

#filterProtocolData(data$protocolData, pulseTypeSelection, rotationSelection)
#filterEchoData(echoData = data$echoData, timeRangeTargetTZ = timeRangeEchoData, protocolData = protocolDataSubset, classSelection = classSelection, classProbCutOff = classProbCutoff, altitudeRange_AGL = altitudeRange_AGL_25_5000, manualBlindTimes = manualBlindTimes, echoValidator = TRUE)