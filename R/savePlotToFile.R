#### savePlotToFile ------------------------------------------------------
#' @title savePlotToFile
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}
#' @description saves created plots as .png. 
#' 
#' @param plot=NULL plot to be saved (ggplot)
#' @param filePath=NULL character string, path of the directory, e.g. "[your-project-directory]/Data/MTR". If the directory does not exist it will be created if possible.
#' @param plotType=NULL character string, name/description of the plot, used to create the filename. If not set, the pulse type selection will not be appended to the filename
#' @param plotWidth_mm=NULL numeric, width of the plot in mm. If not set, the size of the png will be set automatically.
#' @param plotHeight_mm=NULL numeric, height of the plot in mm. If not set, the size of the png will be set automatically.
#' @param timeRange=NULL POSIXct vector of size 2, timeRange of the plot, used to create the filename. If not set, the pulse type selection will not be appended to the filename
#' @param classSelection=NULL character string vector, classes that were used to create the plot, used to create the filename. If not set, the pulse type selection will not be appended to the filename
#' @param altitudeRange=NULL numeric vector of size 2, altitude range used to create the plot, used to create the filename. If not set, the pulse type selection will not be appended to the filename
#' @param classAbbreviations=NULL Two-column dataframe with character first column named 'class' and character second 'abbr', containing the full names of the classes and their abbreviations to use in the output filename. Default = NULL, meaning the abbreviations will be used that are stored in the package; See data(classAbbreviations) 
#'
#' @export
#' 

savePlotToFile = function(plot = NULL, 
                          filePath = NULL, 
                          plotType = NULL, 
                          plotWidth_mm = NULL, 
                          plotHeight_mm = NULL, 
                          timeRange = NULL, 
                          classSelection = NULL, 
                          altitudeRange = NULL, 
                          classAbbreviations = NULL)
{
  # sanity check plot data
  if( !is.null( plot ) )
  {
    # create filePath to save plot
    if( !is.null( filePath) )
    {
      if( !is.null( plotType ) )
      {
        filePath <- paste( filePath, plotType, sep = "/" ) 
      }
      ifelse( !dir.exists( filePath ), dir.create( filePath ), FALSE )
    } 
    # create filename to save plot
    ifelse( is.null( plotType ), fileName <- "", fileName <- plotType )
    # time range for fileName
    if( !is.null( timeRange ) && length( timeRange ) == 2 )
    {
      startTime <- format( timeRange[ 1 ], "%Y%m%d" )
      stopTime <- format( timeRange[ 2 ], "%Y%m%d" )
      time <- paste( startTime, stopTime, sep = "-" )
      fileName <- paste( fileName, time, sep = "_" )
    } 
    # altitude range for fileName
    if( !is.null( altitudeRange ) && length( altitudeRange ) == 2 )
    {
      altitudeRangeStart <- paste0( altitudeRange[ 1 ], "m" )
      altitudeRangeStop <- paste0( altitudeRange[ 2 ], "m" )
      altitude <- paste( altitudeRangeStart, altitudeRangeStop, sep = "-" )
      fileName <- paste( fileName, altitude, sep = "_" )
    }
    # classes for fileName
    if( !is.null( classSelection ) )
    {
      classes <- paste( classAbbreviations$abbr[ match( classSelection, classAbbreviations$class ) ], collapse = "" )
      fileName <- paste( fileName, classes, sep = "_" )
    }
    
    # check for existing files
    idx <- 0
    while( file.exists( paste( filePath, paste0( fileName, "_", idx, ".png" ), sep = "/" ) ) )
    {
      idx = idx + 1
    }
    
    # add index and png ending
    fileName <- paste0( fileName, "_", idx, ".png" )
    
    # save plot
    if( !is.null( plotWidth_mm ) && !is.null( plotHeight_mm ) )
    {
      ggsave( filename = fileName, plot = plot, device = png(), path = filePath, scale = 1, width = plotWidth_mm, height = plotHeight_mm, units = "mm", dpi = 300, limitsize = FALSE )  
    } else
    {
      ggsave( filename = fileName, plot = plot, device = png(), path = filePath, scale = 1, dpi = 300, limitsize = FALSE )  
    }
    
    dev.off()
  }
}

#saveMTR( mtr = mtr_DayNight_25mto1025m_50m, filepath = mtrDataDir, dbName = dbName )
#savePlotToFile( plot = directionPlot, filePath = filePath, plotType = "direction", plotWidth_mm = 150, plotHeight_mm = 150, timeRange = c( timeRange[[ i ]][ 1 ], timeRange[[ i ]][ 2 ] ), classSelection = plotClasses, altitudeRange = c( min( altitudeRange_AGL ), max( altitudeRange_AGL ) ) )
