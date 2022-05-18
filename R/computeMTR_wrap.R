#### computeMTR_wrap.R ------------------------------------------------------
#' @title Wrapper for computing MTR.
#' @author Baptiste Schmid, \email{baptiste.schmid@@vogelwarte.ch}
#' @description Wrapped function of compute MTR to incl. data formating.
#' @seealso \link{list.dirs} This is optional if you want to point to another function description.

#' @param echoData dataframe with the echo data from the data list created by the function ‘extractDBData’ or a subset of it created by the function ‘filterEchoData’.
#' @param classSelection character string vector with all classes which should be used to calculate the MTR. The MTR and number of Echoes will be calculated for each class as well as for all classes together. 
#' @param echoValidator logical, NOT YET IMPLEMENTED, so default is FALSE. 2if set to TRUE, echoes labelled by the echo validator as “non-bio scatterer” will be excluded. If set to FALSE, all echoes are included.
#' @param timeRangeTargetTZ POSIXct vector of length 2 with start and end time with target timezone. Echoes outside the time range will be excluded.
#' @param timeBinDuration_sec numeric, time bin duration in seconds.
#' @param propObsTimeCutoff numeric between 0 and 1. If the MTR is computed per day and night, time bins with a proportional observation time smaller than propObsTimeCutoff are ignored when combining the time bins. If the MTR is computed for each time bin, the parameter is ignored.
#' @param timeZone character string, timezone of the time bins -> adived to be target timezone
#' @param sunriseSunset dataframe with the sunrise/sunset times and the civil twilight times created by the function ‘twilight’ \link{scripts/MR1Analysis/Code/Functions/Twilight.R}
#' @param sunOrCivil character string, “sun” (sunrise/sunset times) or “civil” (civil twilight times) to group by day and night. Default if not set is civil twilight.
#' @param computePerDayNight logical, TRUE: MTR is computed per day and night FALSE: MTR is computed for each time bin.
#' @param altitudeRange_AGL numeric vector of length 2 with the start and end of the altitude range in meter a.g.l.
#' @param altitudeBinSize numeric, size of the altitude bins in meter.
#' @param protocolData dataframe with the protocol data from the data list created by the function ‘extractDBData’ or a subset of it created by the function ‘filterProtocolData’. Echoes not detected during the listed protocols will be excluded.
#' @param pulseTypeSelection 
#' @param rotationSelection 
#' @param visibilityData dataframe with the visibility data from the data list created by the function ‘extractDBData’.
#' @param manualBlindTimes dataframe with the manual blind times created by the function ‘loadManualBlindTimes’
#' @param blindTimeAsMtrZero 
#' @param saveRDS_MTR logical, TRUE (default): use the function saveMTR.R to save the output.
#' @param filepath character string, path location to save the MTR table, e.g. '~/projectName/MTRtable'.
#' @param dbName Name of the data-base.
#'
#' @return MTR table
#' @export
#'
computeMTR_wrap = function(echoData,
                           classSelection,
                           echoValidator = FALSE, # not yet implemented 
                           timeRangeTargetTZ,
                           timeBinDuration_sec, 
                           propObsTimeCutoff,
                           timeZone, 
                           sunriseSunset, 
                           sunOrCivil, # default
                           computePerDayNight,
                           altitudeRange_AGL,
                           altitudeBinSize,
                           protocolData,
                           pulseTypeSelection, 
                           rotationSelection,
                           visibilityData,
                           manualBlindTimes, # defined in settings
                           blindTimeAsMtrZero,
                           saveRDS_MTR = TRUE,
                           filepath, 
                           dbName){
# Filter protocol data
# =============================================================================
  protocolDataSubset = filterProtocolData(protocolData       = protocolData, 
                                          pulseTypeSelection = pulseLengthSelection, 
                                          rotationSelection  = rotationSelection)
# add stop if(nrow == 0, pulseType not existing, ...)

# Compute BlindTimes 
# =============================================================================
  blindTimes = mergeVisibilityAndManualBlindTimes(visibilityData   = visibilityData, 
                                                  manualBlindTimes = manualBlindTimes, 
                                                  protocolData     = protocolDataSubset) # internal output >> filterProtocolData()

# filter echo data
# =============================================================================
  echoDataSubset = filterEchoData(echoData          = echoData,
                                  timeRangeTargetTZ = timeRangeTargetTZ, 
                                  protocolData      = protocolDataSubset, # internal output >> filterProtocolData()
                                  classSelection    = classSelection,
                                  #   classProbCutOff = NULL, # Ignore
                                  altitudeRange_AGL = altitudeRange_AGL,
                                  manualBlindTimes  = manualBlindTimes,
                                  echoValidator     = echoValidator)

# Create Time Bins & Compute Observation Time for each Time Bin
# =============================================================================
  # TimeBin size in seconds
  # ===========================================================================
    timeBins = createTimeBins(timeRange           = timeRangeTargetTZ, 
                              timeBinDuration_sec = timeBinduration_sec, 
                              timeZone            = targetTimeZone, 
                              sunriseSunset       = sunriseSunset, 
                              sunOrCivil          = sunOrCivil)
  
  # Compute observation times
  # =============================================================================
    timeBins = computeObservationTime(timeBins           = timeBins, # internal output >> createTimeBins()
                                      protocolData       = protocolDataSubset, # internal output >> filterProtocolData()
                                      blindTimes         = blindTimes, # internal output >> mergeVisibilityAndManualBlindTimes
                                      blindTimeAsMtrZero = blindTimeAsMtrZero)

# compute MTR
# =============================================================================
  MTR = computeMTR(echoes             = echoDataSubset, # internal output >> filterEchoData()
                   classSelection     = classSelection, 
                   altitudeRange      = altitudeRange_AGL, 
                   altitudeBinSize    = altitudeBinSize,
                   timeBins           = timeBins, 
                   propObsTimeCutoff  = propObsTimeCutoff, 
                   computePerDayNight = computePerDayNight)

# Save MTR, if requested
# =============================================================================
  if(saveRDS_MTR){
    saveMTR(mtr                = MTR, # internal output >> computeMTR()
            filepath           = filepath, 
            dbName             = dbName, 
            rotSelection       = rotationSelection, 
            pulseTypeSelection = pulseLengthSelection)
  }
  
# Return output
# =============================================================================
  return(MTR)
} 
