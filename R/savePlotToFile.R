#### savePlotToFile ------------------------------------------------------
#' @title savePlotToFile
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest, \email{birgen.haest@@vogelwarte.ch}  
#' @description saves created plots as .png. 
#' 
#' @param plot plot to be saved (ggplot)
#' @param filePath character string, path of the directory, e.g. "your-project-directory/Data/MTR". If the directory does not exist it will be created if possible.
#' @param plotType character string, name/description of the plot, used to create the filename. If not set, the pulse type selection will not be appended to the filename
#' @param plotWidth_mm numeric, width of the plot in mm. If not set, the size of the png will be set automatically.
#' @param plotHeight_mm numeric, height of the plot in mm. If not set, the size of the png will be set automatically.
#' @param timeRange POSIXct vector of size 2, timeRange of the plot, used to create the filename. If not set, the pulse type selection will not be appended to the filename
#' @param classSelection character string vector, classes that were used to create the plot, used to create the filename. If not set, the pulse type selection will not be appended to the filename
#' @param altitudeRange numeric vector of size 2, altitude range used to create the plot, used to create the filename. If not set, the pulse type selection will not be appended to the filename
#' @param classAbbreviations Two-column dataframe with character first column named 'class' and character second 'abbr', containing the full names of the classes and their abbreviations to use in the output filename. Default = NULL, meaning the abbreviations will be used that are stored in the package; See data(classAbbreviations) 
#'
#' @export
#' 

savePlotToFile = function(plot               = NULL, 
                          filePath           = NULL, 
                          plotType           = NULL, 
                          plotWidth_mm       = NULL, 
                          plotHeight_mm      = NULL, 
                          timeRange          = NULL, 
                          classSelection     = NULL, 
                          altitudeRange      = NULL, 
                          classAbbreviations = NULL){
# sanity check plot data
# =============================================================================
  if (!is.null(plot)){
    # create filePath to save plot
    # =========================================================================
      if (!is.null(filePath)){
        if (!is.null(plotType)){
          filePath = paste(filePath, plotType, sep = "/") 
        }
        dir.create(filePath, showWarnings = F, recursive = T)
      } 
    
    # create filename to save plot
    # =========================================================================
      fileName = ifelse(is.null(plotType), "", plotType)
      
    # time range for fileName
    # =========================================================================
      if (!is.null(timeRange) && length(timeRange) == 2){
        startTime = format(timeRange[1], "%Y%m%d")
        stopTime = format(timeRange[2], "%Y%m%d")
        time = paste(startTime, stopTime, sep = "-")
        fileName = paste(fileName, time, sep = "_")
      } 
      
    # altitude range for fileName
    # =========================================================================
      if (!is.null(altitudeRange) && length(altitudeRange) == 2){
        altitudeRangeStart = paste0(altitudeRange[1], "m")
        altitudeRangeStop = paste0(altitudeRange[2], "m")
        altitude = paste(altitudeRangeStart, altitudeRangeStop, sep = "-")
        fileName = paste(fileName, altitude, sep = "_")
      }
    
    # classSelection for fileName
    # =========================================================================
      if (!is.null(classSelection)){
        # classes = paste(classAbbreviations$abbr[match(classSelection, 
        #                                                classAbbreviations$class)], 
        #                  collapse = "")
        fileName = paste(fileName, classSelection, sep = "_")
      } else {
        fileName = paste(fileName, "allClasses", sep = "_")
      }
      
    # check for existing files
    # =========================================================================
      # idx = 0
      # while (file.exists(paste(filePath, paste0(fileName, "_", idx, ".png"), sep = "/"))){
      #   idx = idx + 1
      # }
    
    # add index and png ending
    # =========================================================================
      # fileName = paste0(fileName, "_", idx, ".png")
      fileName = paste0(fileName, ".png")
      
    # save plot
    # =========================================================================
      if (!is.null(plotWidth_mm) && !is.null(plotHeight_mm)){
        ggplot2::ggsave(filename  = fileName, 
                        plot      = plot, 
                        device    = grDevices::png(), 
                        path      = filePath, 
                        scale     = 1, 
                        width     = plotWidth_mm, 
                        height    = plotHeight_mm, 
                        units     = "mm", 
                        dpi       = 300, 
                        limitsize = FALSE)  
      } else {
        ggplot2::ggsave(filename  = fileName, 
                        plot      = plot, 
                        device    = grDevices::png(), 
                        path      = filePath, 
                        scale     = 1, 
                        dpi       = 300, 
                        limitsize = FALSE)  
      }
    
    # Close plotting device
    # =========================================================================
      dev.off()
  }
}

#saveMTR(mtr = mtr_DayNight_25mto1025m_50m, filepath = mtrDataDir, dbName = dbName)
#savePlotToFile(plot = directionPlot, filePath = filePath, plotType = "direction", plotWidth_mm = 150, plotHeight_mm = 150, timeRange = c(timeRange[[i]][1], timeRange[[i]][2]), classSelection = plotClasses, altitudeRange = c(min(altitudeRange_AGL), max(altitudeRange_AGL)))
