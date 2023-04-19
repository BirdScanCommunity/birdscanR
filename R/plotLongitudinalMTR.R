#### plotLongitudinalMTR ------------------------------------------------------
#' @title plotLongitudinalMTR
#'
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch} 
#' @description Plots a time series of MTR values as a bar plot. For each bar 
#' the spread (first and third Quartile) is shown as error bars as well as the
#' numbers of echoes. Periods with no observation are indicated with grey, 
#' negative bars.
#'
#' @param mtr data frame with MTR values created by the function ‘computeMTR’. 
#' @param maxMTR optional numeric variable, fixes the maximum value of the 
#' y-Scale of the plot to the given value. If negative or not set, the y-Scale 
#' is auto-scaled.
#' @param timeRange optional list of string vectors length 2, start and end 
#' time of the time ranges that should be plotted. The date/time format is 
#' “yyyy-MM-dd hh:mm”. 
#' @param targetTimeZone "Etc/GMT0" String specifying the target time zone. 
#' Default is "Etc/GMT0".
#' @param plotClass character string with the class of which the MTR data 
#' should be plotted. If not set or set to “allClasses”, MTR of all classes 
#' will be plotted. 
#' @param propObsTimeCutoff numeric between 0 and 1. If the MTR is computed per 
#' day and night, time bins with a proportional observation time smaller than 
#' propObsTimeCutoff are ignored when combining the time bins. If the MTR is 
#' computed for each time bin, the parameter is ignored.
#' @param plotSpread logical, choose if the spread (first and third quartile) 
#' should be plotted.
#' @param filePath character string, path of the directory where the plot 
#' should be saved. The function ‘savePlotToFile’ is used to save the plots as 
#' png files with an auto-generated filename.
#'
#' @return png files stored in the directory specified with 'filePath'
#' @export
#'
#' @examples 
#' \dontrun{
#' # Set server, database, and other input settings
#' # ===========================================================================
#'   dbServer       = "MACHINE\\SERVERNAME" # Set the name of your SQL server
#'   dbName         = "db_Name"               # Set the name of your database
#'   dbDriverChar   = "SQL Server"            # Set either "SQL Server" or "PostgreSQL"
#'   mainOutputDir  = file.path(".", "results")
#'   radarTimeZone  = "Etc/GMT0"
#'   targetTimeZone = "Etc/GMT0"
#'   listOfRfFeaturesToExtract = c(167, 168)
#'   siteLocation   = c(47.494427, 8.716432)
#'   sunOrCivil     = "civil"
#'   timeRangeData       = c("2021-01-15 00:00", "2021-01-31 00:00")
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
#' # Get sunrise/sunset 
#' # ===========================================================================
#'   sunriseSunset = twilight(timeRange = timeRangeData,
#'                            latLon    = c(47.494427, 8.716432),
#'                            timeZone  = targetTimeZone)
#'                           
#' # Get manual blind times
#' # ===========================================================================
#'   data(manualBlindTimes)
#'   cManualBlindTimes = manualBlindTimes
#' 
#' # Compute migration traffic rate
#' # ===========================================================================
#'   classSelection.mtr = c("insect")
#'   mtrData = computeMTR(dbName                      = dbName, 
#'                        echoes                      = dbData$echoData, 
#'                        classSelection              = classSelection.mtr, 
#'                        altitudeRange               = c(25, 1025),
#'                        altitudeBinSize             = 50,
#'                        timeRange                   = timeRangeData, 
#'                        timeBinDuration_sec         = 1800,
#'                        timeZone                    = targetTimeZone,
#'                        sunriseSunset               = sunriseSunset,
#'                        sunOrCivil                  = "civil",
#'                        protocolData                = dbData$protocolData, 
#'                        visibilityData              = dbData$visibilityData,
#'                        manualBlindTimes            = cManualBlindTimes,
#'                        saveBlindTimes              = FALSE,
#'                        blindTimesOutputDir         = getwd(),
#'                        blindTimeAsMtrZero          = NULL,
#'                        propObsTimeCutoff           = 0, 
#'                        computePerDayNight          = FALSE, 
#'                        computeAltitudeDistribution = TRUE)
#' 
#' # Make Plot 
#' # ===========================================================================
#'   timeRangePlot = list(c("2021-01-15 00:00", "2021-01-22 00:00"),
#'                        c("2021-01-23 00:00", "2021-01-31 00:00"))
#'   plotExplorationplotLongitudinalMTR(mtr               = mtrData, 
#'                                      maxMTR            = -1, 
#'                                      timeRange         = timeRangePlot,
#'                                      targetTimeZone    = "Etc/GMT0",
#'                                      plotClass         = "allClasses",
#'                                      propObsTimeCutoff = 0.2,
#'                                      plotSpread        = TRUE, 
#'                                      filePath          = "./") 
#' }
#'
plotLongitudinalMTR = function(mtr, 
                               maxMTR, 
                               timeRange         = NULL, 
                               targetTimeZone    = "Etc/GMT0",
                               plotClass         = "allClasses", 
                               propObsTimeCutoff = 0.2, 
                               plotSpread        = TRUE, 
                               filePath          = NULL){
# Check whether any mtr data is provided
# =============================================================================
  if (length(mtr[, 1]) == 0){
    warning("no MTR data to plot.")
    return()
  }
  
# Convert the timeRange input to a POSIXct object, if it was defined
# =============================================================================
  if (!is.null(timeRange)){
    posixCTListCon = function(x){as.POSIXct(x, 
                                            format = "%Y-%m-%d %H:%M", 
                                            tz = targetTimeZone)}
    timeRange = lapply(timeRange, posixCTListCon)
  }
  
# extract classSelection
# =============================================================================
  if (plotClass == "allClasses"){
    classSelection = names(mtr)[grepl("mtr.", names(mtr), fixed = TRUE) & 
                                (!grepl("allClasses", names(mtr), 
                                        fixed = TRUE))]  
  } else {
    classSelection = names(mtr)[grepl("mtr.", names(mtr), fixed = TRUE) & 
                                grepl(plotClass, names(mtr), fixed = TRUE)]
  }
  classSelection = gsub("mtr.", "", classSelection)
  
# Check whether requested class is present in mtr data
# =============================================================================
  if (length(classSelection) == 0){
    warning(paste0("class '", plotClass, "' not present in mtr data."))
    return()
  }
  
# timeRanges to plot
# =============================================================================
  if (is.null(timeRange)){
    nPlots = 1
  } else {
    nPlots = length(timeRange)
  }
  
# Check how many altitude bins there are in the mtr data
# =============================================================================
  nrAltBins = length(unique(mtr$altitudeChunkId))
  altBinIds = unique(mtr$altitudeChunkId)
  # altitudeChunkId = min(unique(mtr$altitudeChunkId))

# Do for each altitude bin
# =============================================================================
  for (cAltBin in altBinIds){
    # Make a plot for each time range    
    # =========================================================================
      for (i in 1:nPlots){
        # Subset mtr data to current time range
        # =====================================================================
          if (is.null(timeRange)){
            mtrPlot = mtr[mtr$altitudeChunkId == cAltBin,]
            timeRange    = c(min(mtrPlot$timeChunkBegin), max(mtrPlot$timeChunkEnd))
          } else {
            mtrPlot = mtr[(mtr$altitudeChunkId == cAltBin) & 
                          (mtr$timeChunkBegin > timeRange[[i]][1]) & 
                          (mtr$timeChunkBegin < timeRange[[i]][2]),]  
          }
        
        # Proceed processing when there is data for this time period
        # =====================================================================
          if (length(mtrPlot[, 1]) > 0){
            # Get mtr data needed based on classSelection
            # =================================================================
              mtrPlot = mtrPlot[, (!grepl(".", names(mtrPlot), fixed = TRUE)) | 
                                   grepl(plotClass, names(mtrPlot), 
                                         fixed = TRUE)]
              names(mtrPlot) = gsub(paste0(".", plotClass), "", names(mtrPlot))
            
            # Mark timeBins as not Observed if proportional observation time is 
            # smaller than propObsTimeCutoff
            # =================================================================
              mtrPlot$proportionalTimeObserved[is.na(mtrPlot$proportionalTimeObserved)]                           = 0
              mtrPlot$mtr[mtrPlot$proportionalTimeObserved < propObsTimeCutoff | is.na(mtrPlot$mtr)]              = 0
              mtrPlot$nEchoes[mtrPlot$proportionalTimeObserved < propObsTimeCutoff | is.na(mtrPlot$mtr)]          = 0
              mtrPlot$mtrFirstQuartile[mtrPlot$proportionalTimeObserved < propObsTimeCutoff | is.na(mtrPlot$mtr)] = 0
              mtrPlot$mtrThirdQuartile[mtrPlot$proportionalTimeObserved < propObsTimeCutoff | is.na(mtrPlot$mtr)] = 0
              mtrPlot                                        = data.frame(mtrPlot, obs = 0)
              mtrPlot                                        = data.frame(mtrPlot, obsType = "dayObserved")
              levels(mtrPlot$obsType)                        = c("dayObserved", "nightObserved")
              mtrPlot$obsType[mtrPlot$dayOrNight == "night"] = "nightObserved"
              if (maxMTR >= 0){
                mtrPlot$obs[mtrPlot$proportionalTimeObserved < propObsTimeCutoff | is.na(mtrPlot$mtr)] = maxMTR*(-0.015)
                yScale = c(maxMTR * -0.07, maxMTR)
              } else {
                mtrPlot$obs[mtrPlot$proportionalTimeObserved < propObsTimeCutoff | is.na(mtrPlot$mtr)] = max(mtrPlot$mtrThirdQuartile)*(-0.015)
                maxScale = max(c(max(mtrPlot$mtrThirdQuartile), max(mtrPlot$mtr)), na.rm =  TRUE)
                yScale   = c(maxScale*(-0.07), maxScale)
              }
            
            # Add a timeStamp for the middle of each time bin
            # =================================================================
              mtrPlot$timeChunkMiddle = mtrPlot$timeChunkBegin + 
                                          ((mtrPlot$timeChunkEnd - mtrPlot$timeChunkBegin)/2)
              
            # Plot
            # =================================================================
              subtitle = paste0(format(mtrPlot$timeChunkDate[1], "%d-%b-%Y"), " to ", 
                                format(mtrPlot$timeChunkDate[length(mtrPlot$timeChunkDate)], 
                                       "%d-%b-%Y"), "\n",
                                min(mtrPlot$altitudeChunkBegin), "m" ," to ",
                                max(mtrPlot$altitudeChunkEnd), "m", "\n",
                                paste(classSelection, collapse = ", "))
              
              longPlot = ggplot2::ggplot(mtrPlot, 
                                         ggplot2::aes(x     = timeChunkMiddle, 
                                                      y     = mtr, 
                                                      label = paste0("N=", nEchoes), 
                                                      fill  = dayOrNight)) + 
                         ggplot2::geom_col(ggplot2::aes(timeChunkMiddle, 
                                                        mtr, 
                                                        fill = dayOrNight), 
                                           position = "dodge2")
              if (plotSpread == TRUE){
                longPlot = longPlot + 
                           ggplot2::geom_errorbar(ggplot2::aes(ymin = mtrFirstQuartile, 
                                                               ymax = mtrThirdQuartile), 
                                                  width    = 0.2, 
                                                  position = ggplot2::position_dodge(0.9), 
                                                  color    = "grey40")
              }
              longPlot = longPlot + 
                          ggplot2::geom_col(ggplot2::aes(timeChunkMiddle, 
                                                         obs, 
                                                         fill = obsType), 
                                            position = "dodge2", 
                                            fill = "grey50") + 
                          ggplot2::ggtitle(label = "MTR", subtitle = subtitle) + 
                          ggplot2::xlab("Date") + 
                          ggplot2::ylab("MTR [ind./h/km]") +
                          ggplot2::geom_text(position = ggplot2::position_dodge(width = 0.9), 
                                             ggplot2::aes(y = yScale[1] * 1.75 , 
                                                          hjust = "bottom", 
                                                          vjust = "center"), 
                                             color = "grey60", 
                                             size = 2.5, 
                                             angle = 90) +
                          ggplot2::scale_fill_manual(" ", 
                                                     values = c("day" = "goldenrod1", "night" = "navy")) +
                          ggplot2::scale_x_datetime(labels = function(x) format(x, "%d-%b-%Y")) +
                          ggplot2::coord_cartesian(ylim = yScale) + 
                          ggplot2::theme(plot.title    = ggplot2::element_text(size = 12, 
                                                                               face = "bold", 
                                                                               hjust = 0.5),
                                         plot.subtitle = ggplot2::element_text(size = 10, 
                                                                               color = "grey40", 
                                                                               hjust = 0.5),
                                         axis.text.x   = ggplot2::element_text(angle = 90))
            
            # save plot to file
            # =================================================================
              plotWidth_mm   = as.numeric(difftime(timeRange[[i]][2], 
                                                   timeRange[[i]][1], 
                                                   "days") * 10 + 50)
              plotHeight_mm  = 150
              if (plotWidth_mm < plotHeight_mm){
                plotWidth_mm = plotHeight_mm
              }
              savePlotToFile(plot            = longPlot,
                             filePath       = filePath, 
                             plotType       = "mtrPerDay", 
                             plotWidth_mm   = plotWidth_mm, 
                             plotHeight_mm  = plotHeight_mm, 
                             timeRange      = c(timeRange[[i]][1], timeRange[[i]][2]), 
                             classSelection = plotClass, 
                             altitudeRange  = c(min(mtrPlot$altitudeChunkBegin), 
                                                max(mtrPlot$altitudeChunkEnd)))
          }
      }
  }
}

