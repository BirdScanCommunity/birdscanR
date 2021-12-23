
#### saveMTR ------------------------------------------------------
#' @title saveMTR
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}
#' @description saves MTR data to a .rds file in the directory \code{filepath}. If the directory is not existing it will be created if possible.
#'
#' @param mtr=NULL dataframe with MTR values created by the function \code{computeMTR}
#' @param fileName=NULL prefix of the filename (string). If not set, "mtr" is used. Different information about the MTR data will be appended to the filename.
#' @param filepath=NULL character string, path of the directory, e.g. "[your-project-directory]/Data/MTR". If the directory does not exist it will be created if possible.
#' @param dbName=NULL character string, name of the database. Used to create the filename. If not set, the database name will not be appended to the filename
#' @param rotSelection=NULL numeric vector, rotation selection which was used to filter protocols. Used to create the filename. If not set, the rotation selection will not be appended to the filename
#' @param pulseTypeSelection=NULL character vector, pulse type selection which was used to filter protocols. Used to create the filename. If not set, the pulse type selection will not be appended to the filename
#'
#' @export
#' 
saveMTR = function( mtr = NULL, fileName = NULL, filepath = NULL, dbName = NULL, rotSelection = NULL, pulseTypeSelection = NULL )
{
  # sanity check filePath and mtr data
  if( !is.null( filepath ) || is.null( mtr ) )
  {
    ifelse( !dir.exists( filepath ), dir.create( filepath ), FALSE )
    if( dir.exists( filepath ) == TRUE )
    {
      # create fileName
      if( is.null(fileName) ){
        fileName <- "mtr"
      } else {
        fileName <- fileName
      }
      # add dbname to fileName if available
      if( !is.null( dbName ) )
      {
        fileName <- paste( fileName, dbName, sep = "_" )
      }
      # get begin and end of timerange
      timeStart <- format( min( mtr$timeChunkBegin ), "%Y%m%d" )
      timeStop <- format( max( mtr$timeChunkEnd ), "%Y%m%d" )
      # get size of timeBins and/or day-night
      if( max( table( mtr$timeChunkDateSunset ) ) > 2 * length( unique( mtr$altitudeChunkId ) ) )
      {
        timeChunkSize <- paste0( difftime( mtr$timeChunkEnd[ 1 ], mtr$timeChunkBegin[ 1 ], units = "secs" ), "s" )
      } else
      {
        timeChunkSize <- "dayNight"
      }
      # altitude chunk start and stop
      altitudeStart <- paste0( min( mtr$altitudeChunkBegin ), "m" )
      altitudeStop <- paste0( max( mtr$altitudeChunkEnd ), "m" )
      # number of altitude chunks
      nAltitudeBins <- paste0( length( unique( mtr$altitudeChunkId ) ), "bin" )
      altitude <- paste( altitudeStart, altitudeStop, nAltitudeBins, sep = "-" )
      # classes
      classSelection <- names( mtr )[ grepl( "mtr.", names( mtr ), fixed = TRUE) & !grepl( "allClasses", names( mtr ), fixed = TRUE) ]
      classSelection <- gsub( "mtr.", "", classSelection )
      classes <- paste( classAbbreviations$abbr[ match( classSelection, classAbbreviations$class ) ], collapse = "" )
      # combine fileName
      fileName <- paste( fileName, timeStart, timeStop, timeChunkSize, altitude, classes, sep = "_" )
      # rotation
      if( !is.null( rotSelection ) )
      {
        rotation <- paste( "rot", paste( rotSelection, collapse = "-" ), sep = "-" )
        fileName <- paste( fileName, rotation, sep = "_" )
      }
      # pulseType
      if( !is.null( pulseTypeSelection ) )
      {
        pulseType <- paste( "pulse", paste( pulseTypeSelection, collapse = "-" ), sep = "-" )
        fileName <- paste( fileName, pulseType, sep = "_" )
      }
      
      # add ending
      fileName <- paste( fileName, "rds", sep = "." )
      
      saveRDS( mtr, file = file.path( filepath, fileName ) )
    } else
    {
      warning( paste0( "Could not create directory: ", filepath, ". MTR data not saved." ) )
    }
  }
}

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
#'
#' @export
#' 

savePlotToFile = function( plot = NULL, filePath = NULL, plotType = NULL, plotWidth_mm = NULL, plotHeight_mm = NULL, timeRange = NULL, classSelection = NULL, altitudeRange = NULL )
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
