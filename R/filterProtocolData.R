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

filterProtocolData = function(protocolData = NULL, 
                              pulseTypeSelection = NULL, 
                              rotationSelection = NULL){
  # Set variables
  # ===========================================================================
    requiredProtocolDataCols = c("pulseType", "rotate", 
                                  "startTime_targetTZ", "stopTime_targetTZ")
  
  if (!is.null(protocolData) && length(protocolData[, 1]) > 0 && sum(requiredProtocolDataCols %in% names(protocolData)) == length(requiredProtocolDataCols)){
    # exclude invalid times
    # =========================================================================
      validTimesInd = (protocolData$startTime_targetTZ > "1950-01-01") & 
                       (protocolData$stopTime_targetTZ > "1950-01-01")
      protocolData = protocolData[validTimesInd,]
    
    # subset protocolData by pulseLength
    # =========================================================================
      if (!is.null(pulseTypeSelection)){
        protocolData = protocolData[protocolData$pulseType %in% pulseTypeSelection,]
      }
    
    # subset protocolData by rotation mode
    # =========================================================================
      if (!is.null(rotationSelection)){
        protocolData = protocolData[protocolData$rotate %in% rotationSelection,]
      }
  }
  
  # Return output
  # ===========================================================================
    return(protocolData)
}