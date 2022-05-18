#### saveMTR ------------------------------------------------------
#' @title saveMTR
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest, \email{birgen.haest@@vogelwarte.ch}  
#' @description saves MTR data to a .rds file in the directory \code{filepath}. If the directory is not existing it will be created if possible.
#'
#' @param mtr dataframe with MTR values created by the function \code{computeMTR}
#' @param fileName=NULL Filename (string) for the file. If not set, the filename will be built using the input of the variables 'filenamePrefix', 'dbName', 'classAbbreviations', and other info in the 'mtr' data. If set, overrides the automatic filename creation.
#' @param fileNamePrefix=NULL prefix of the filename (string). If not set, "mtr" is used. Different information about the MTR data will be appended to the filename.
#' @param filepath character string, path of the directory. If the directory does not exist it will be created if possible.
#' @param dbName=NULL character string, name of the database. Used to create the filename, if 'fileName' is not provided. 
#' @param rotSelection=NULL numeric vector, rotation selection which was used to filter protocols. Used to create the filename, if 'fileName' is not provided. If not set, the rotation selection will not be appended to the filename.
#' @param pulseTypeSelection=NULL character vector, pulse type selection which was used to filter protocols. Used to create the filename, if 'fileName' is not provided. If not set, the pulse type selection will not be appended to the filename.
#' @param classAbbreviations=NULL Two-column dataframe with character first column named 'class' and character second 'abbr', containing the full names of the classes and their abbreviations to use in the output filename. Default = NULL, meaning the abbreviations will be used that are stored in the package; See data(classAbbreviations). Used to create the filename, if 'fileName' is not provided.  
#'
#' @export
#' 
saveMTR = function(mtr, 
                   filepath, 
                   fileName           = NULL, 
                   fileNamePrefix     = NULL,
                   dbName             = NULL, 
                   rotSelection       = NULL, 
                   pulseTypeSelection = NULL, 
                   classAbbreviations = NULL){
# Check whether output file path can be created, or exists already
# =============================================================================
  dir.create(filepath, showWarnings = F, recursive = T)
  if (!dir.exists(filepath)){
    stop(paste0("Output file path does not exist, but can also not be created. ", 
                "Check your input!"))
  } 

# create fileName, if not provided
# =============================================================================
  # CASE: filename is provided
  # ===========================================================================
    if (!is.null(fileName)){
      fileName = fileName
      
  # CASE: filename needs to be created
  # ===========================================================================
    } else {
      if (!is.null(fileNamePrefix)){
        fileName = fileNamePrefix
      } else {
        fileName = "mtr"
      }
      
      # add dbname to fileName 
      # =======================================================================
        if (!is.null(dbName)){
          fileName = paste(fileName, dbName, sep = "_")
        } else {
          stop(paste0("You need to provide a value for 'dbName' when you are ", 
                      "not providing a value for 'fileName. 'dbName' is needed ", 
                      "to build the fileName."))
        }
      
      # get begin and end of timerange
      # =======================================================================
        timeStart = format(min(mtr$timeChunkBegin), "%Y%m%d")
        timeStop  = format(max(mtr$timeChunkEnd), "%Y%m%d")
        
      # get size of timeBins and/or day-night
      # =======================================================================
        if (max(table(mtr$timeChunkDateSunset)) > 2 * length(unique(mtr$altitudeChunkId))){
          timeChunkSize = paste0(difftime(mtr$timeChunkEnd[1], mtr$timeChunkBegin[1], units = "secs"), "s")
        } else {
          timeChunkSize = "dayNight"
        }
      
      # altitude chunk start and stop
      # =======================================================================
        altitudeStart = paste0(min(mtr$altitudeChunkBegin), "m")
        altitudeStop  = paste0(max(mtr$altitudeChunkEnd), "m")
        
      # number of altitude chunks
      # =======================================================================
        nAltitudeBins = paste0(length(unique(mtr$altitudeChunkId)), "bin")
        altitude      = paste(altitudeStart, altitudeStop, nAltitudeBins, sep = "-")
        
      # classes
      # =======================================================================
        classSelection = names(mtr)[grepl("mtr.", names(mtr), fixed = TRUE) & !grepl("allClasses", names(mtr), fixed = TRUE)]
        classSelection = gsub("mtr.", "", classSelection)
        classes        = paste(classAbbreviations$abbr[match(classSelection, classAbbreviations$class)], collapse = "")
        
      # combine fileName
      # =======================================================================
        fileName = paste(fileName, timeStart, timeStop, timeChunkSize, altitude, classes, sep = "_")      
      
      # rotation
      # =======================================================================
        if (!is.null(rotSelection)){
          rotation = paste("rot", paste(rotSelection, collapse = "-"), sep = "-")
          fileName = paste(fileName, rotation, sep = "_")
        }  
        
      # pulseType
      # =======================================================================
        if (!is.null(pulseTypeSelection)){
          pulseType = paste("pulse", paste(pulseTypeSelection, collapse = "-"), sep = "-")
          fileName = paste(fileName, pulseType, sep = "_")
        }
        
      # add ending
      # =======================================================================
        fileName = paste(fileName, "rds", sep = ".")
    }

# Save to file
# =======================================================================
  saveRDS(mtr, file = file.path(filepath, fileName))
}