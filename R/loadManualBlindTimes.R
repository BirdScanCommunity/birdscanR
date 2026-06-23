#' @title loadManualBlindTimes
#' @author Fabian Hertner, Birgen Haest
#' @description Load manual blind times from csv file.
#' For the MTR computation the times when the radar was blind have to be known.
#' The radar itself can be blind in case of a protocol change (block time at the
#' beginning of each protocol, usually 60s) or due to rain/snow or clutter
#' (nearby objects, leaves or similar on radome, etc.). These times are stored
#' in the visibility table or in the time_bins table in relation to the time
#' bins duration (5min). To be flexible and not fixed to the 5 min time bins
#' created by the radar, the visibility table is used in this script. In
#' addition to the radar blind times, manual blind times can be defined. Manual
#' blind times have to be defined in a csv file and are loaded with
#' [loadManualBlindTimes()]. An example dataset is available by running:
#'  `data(manualBlindTimes)`
#'  `write.csv(manualBlindTimes, file = 'the output file destination', row.names = F)`
#' The file path is defined as a global variable ‘manualBlindTimesFile’. A
#' custom file and filepath can be used instead. The manual blind times have to
#' be entered with 3 columns:
#' start time 'yyyy-mm-dd hh:mm:ss', stop time 'yyyy-MM-dd hh:mm:ss', type.
#'
#' Example:
#' 2021-01-16 04:15:00,2021-01-16 05:42:00,rain
#' 2021-01-17 16:33:00,2021-01-17 18:04:00,clutter
#' Manual blind time types can be chosen freely. When computing observation
#' times, it can be decided if some of the defined manual blind time types
#' should be treated as observed time with MTR zero or as blind time
#' (e.g. rain). If no file is present or the file is empty, no manual blind
#' times will be computed.
#' @param filePath character string, absolute filepath of the manual blind time
#' file
#' @param blindTimesTZ time zone of the blind times
#' @param targetTZ target time zone of the blind times
#' @return A dataframe with the manual blind times
#' @family sample data
#' @export
#' @examples
#' \donttest{
#' # Load manual blind time example data from birdscanR package
#' # ===========================================================================
#' data(manualBlindTimes)
#'
#' # Save example manual blind times to a temporary file
#' # ===========================================================================
#' tmpFile = tempfile(fileext = ".csv")
#' write.table(manualBlindTimes,
#'   file = tmpFile, sep = ",",
#'   row.names = FALSE, col.names = FALSE
#' )
#'
#' # Read the manual blind times from file
#' # ===========================================================================
#' manualBlindTimes.new = loadManualBlindTimes(
#'   filePath     = tmpFile,
#'   blindTimesTZ = "Etc/GMT0",
#'   targetTZ     = "Etc/GMT0"
#' )
#' }
#'
loadManualBlindTimes = function(filePath,
                                blindTimesTZ,
                                targetTZ) {
  # Check whether file exists
  # ===========================================================================
  if (!file.exists(filePath)) {
    warning(paste0("manual blind times file does not exist: '", filePath, "'"))
    manualBlindTimes = NULL
    return(manualBlindTimes)
  }
  
  # Detect file extension and load accordingly
  # ===========================================================================
  file_ext = tolower(tools::file_ext(filePath))
  
  if (file_ext == "csv") {
    # Read CSV file
    # =========================================================================
    result <- tryCatch(
      utils::read.csv(file = filePath, header = TRUE),
      error = function(e) {
        warning(paste0("Error reading CSV file: ", e$message))
        return(NULL)
      }
    )
    
    if (!is.null(result) && nrow(result) > 0) {
      names(result) = c("start", "stop", "type")
      manualBlindTimes = result
    } else {
      manualBlindTimes = NULL
    }
    
  } else if (file_ext %in% c("rda", "rdata")) {
    # Load .rda/.rdata file
    # =========================================================================
    env <- new.env()
    result <- tryCatch(
      {
        load(filePath, envir = env)
        # Get the object name(s) loaded
        obj_names <- ls(env)
        if (length(obj_names) == 1) {
          get(obj_names[1], envir = env)
        } else {
          warning(paste0("Multiple objects found in .rda file: ", 
                         paste(obj_names, collapse = ", "), 
                         ". Using the first one."))
          get(obj_names[1], envir = env)
        }
      },
      error = function(e) {
        warning(paste0("Error loading .rda file: ", e$message))
        return(NULL)
      }
    )
    
    if (!is.null(result)) {
      # Ensure it's a data frame
      if (!is.data.frame(result)) {
        warning("Loaded object is not a data frame.")
        manualBlindTimes = NULL
      } else {
        manualBlindTimes = result
      }
    } else {
      manualBlindTimes = NULL
    }
    
  } else {
    warning(paste0("Unsupported file extension: '", file_ext, 
                   "'. Only 'csv', 'rda', and 'rdata' are supported."))
    manualBlindTimes = NULL
  }
  
  # If we have valid data, convert time zones
  # ===========================================================================
  if (!is.null(manualBlindTimes) && nrow(manualBlindTimes) > 0) {
    # Ensure column names are set correctly for .rda files
    if (!all(c("start", "stop", "type") %in% names(manualBlindTimes))) {
      warning("Data frame does not have expected columns (start, stop, type).")
      manualBlindTimes = NULL
    } else {
      # Convert blind times to target time zone
      manualBlindTimes = convertTimeZone(
        data = manualBlindTimes,
        colNames = c("start", "stop"),
        originTZ = blindTimesTZ,
        targetTZ = targetTZ
      )
    }
  }
  
  # Return manual blind times
  # ===========================================================================
  return(manualBlindTimes)
}
