#### extractDbData ------------------------------------------------------------
#' @title Extract DB Data
#' @description Load the data from the database or file and save it to file
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbDriverChar 'SQL Server' The name of the driver. Should be either 
#' 'SQL Server' or 'PostgreSQL'. If 'PostgreSQL', it connects to 
#' cloud.birdradar.com
#' @param dbServer NULL The name of the Server
#' @param dbName NULL The name of the Database
#' @param dbUser NULL The USER name of the Server
#' @param dbPwd NULL The password for the user name
#' @param saveDbToFile FALSE Set to TRUE if you want to save the extracted 
#' database data to an rds file. The output filename is automatically set to 
#' dbName_DataExtract.rds 
#' @param dbDataDir NULL The path to the output directory where to store the 
#' extracted dataset. If the directory does not exist, it will be created.
#' @param radarTimeZone NULL String specifying the radar time zone. Default is 
#' NULL: extract the time zone from the site table of the 'SQL' database.
#' @param targetTimeZone "Etc/GMT0" String specifying the target time zone. 
#' Default is "Etc/GMT0".
#' @param listOfRfFeaturesToExtract NULL or a list of feature to extract
#' @param siteLocation Geographic location of the radar measurements in decimal 
#' format: c(Latitude, Longitude)
#' @param sunOrCivil optional character string, “sun” (sunrise/sunset times) or 
#' “civil” (civil twilight times) to group by day and night. Default is "civil".
#'
#' @return a list of R objects with data extracted from the Database: 'echoData', 
#' 'protocolData', 'siteData', 'visibilityData', 'timeBinData', 'rfFeatures', 
#' 'availableClasses', 'classProbabilitiesAndMtrFactors'
#' @export
#' @examples
#' \dontrun{
#' # Set server, database, and other input settings
#' # ===========================================================================
#'   dbServer       = "MACHINE\\\\SERVERNAME"     # Set the name of your SQL server
#'   dbName         = "db_Name"                   # Set the name of your database
#'   dbDriverChar   = "SQL Server"                # Set either "SQL Server" or "PostgreSQL"
#'   mainOutputDir  = file.path(".", "results")
#'   radarTimeZone  = "Etc/GMT0"
#'   targetTimeZone = "Etc/GMT0"
#'   listOfRfFeaturesToExtract = c(167, 168)
#'   siteLocation   = c(47.494427, 8.716432)
#'   sunOrCivil     = "civil"
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
#' }
#' 
extractDbData = function(dbDriverChar              = "SQL Server", 
                         dbServer                  = NULL, 
                         dbName                    = NULL,  
                         dbUser                    = NULL, 
                         dbPwd                     = NULL,
                         saveDbToFile              = FALSE,
                         dbDataDir                 = NULL,
                         radarTimeZone             = NULL, 
                         targetTimeZone            = "Etc/GMT0", 
                         listOfRfFeaturesToExtract = NULL, 
                         siteLocation              = NULL, 
                         sunOrCivil                = "civil"){
# Check whether the necessary input is present
# =============================================================================
  if(is.null(dbServer)){
    stop("dbserver is not defined. Please check your input!")
  }
  if(is.null(dbName)){stop("dbName is not defined. Please check your input!")}
  if(is.null(targetTimeZone)){
    stop("targetTimeZone is not defined. Please check your input!")
  }
  if((saveDbToFile == TRUE) & is.null(dbDataDir)){
    stop("saveDbToFile is set to TRUE, but no dbDataDir path to save it to has 
         been provided. Please check your input!")
  }
  
# Open the database connection
# =============================================================================
  # CASE: "SQL Server"
  # ===========================================================================
    if (dbDriverChar == "SQL Server"){
      # CASE: Username and Password are provided
      # =======================================================================
        if (!is.null(dbUser) | !is.null(dbPwd)){
          dsn = paste0("driver=", dbDriverChar, ";server=", dbServer,
                       ";database=", dbName,
                       ";uid=", dbUser,
                       ";pwd=", dbPwd)
          
      # CASE: Username and Password are NOT provided
      #       Request the username and pwd via the rstudioAPI
      # =======================================================================
        } else {
          dsn = paste0("driver=", dbDriverChar, ";server=", dbServer,
                       ";database=", dbName,
                       ";uid=", rstudioapi::askForPassword("Database user"),
                       ";pwd=", rstudioapi::askForPassword("Database password"))
        }
      
       dbConnection = RODBC::odbcDriverConnect(dsn)
       
  # CASE: "PostgreSQL"
  # ===========================================================================   
    } else if (dbDriverChar == "PostgreSQL"){
      # CASE: Username and Password are provided
      # =======================================================================
        if (!is.null(dbUser) | !is.null(dbPwd)){
          dbConnection = DBI::dbConnect("PostgreSQL",
                                        host     = "cloud.birdradar.com",
                                        dbname   = dbName,
                                        user     = dbUser,
                                        password = dbPwd)
          
      # CASE: Username and Password are NOT provided
      #       Request the username and pwd via the rstudioAPI
      # =======================================================================
       } else { 
          dbConnection = DBI::dbConnect("PostgreSQL",
                                         host     = "cloud.birdradar.com",
                                         dbname   = dbName,
                                         user     = rstudioapi::askForPassword("Database user"),
                                         password = rstudioapi::askForPassword("Database password"))
       }
    }
 
# Check whether there is a connection
# =============================================================================
  if (if(dbDriverChar == "PostgreSQL"){
        RPostgreSQL::isPostgresqlIdCurrent(dbConnection)
      } else {
        dbConnection != -1
      }){
    # Do nothing
  } else {
    stop("Could not open database. Make sure to set dbServer, dbName, 
         and credentials right.")
  }  
  
# load collection table
# =============================================================================
  message("Extracting collection table from DB...")
  collectionTable = getCollectionTable(dbConnection, dbDriverChar)
 
# load protocol from local MS-SQL DB
# =============================================================================
  message("Extracting protocol table from DB...")
  protocolTable = getProtocolTable(dbConnection, dbDriverChar)

# load radar from local MS-SQL DB
# =============================================================================
  message("Extracting radar table from DB...")
  radarTable = getRadarTable(dbConnection, dbDriverChar)

# load site from local MS-SQL DB
# =============================================================================
  message("Extracting site table from DB...")
  siteTable = getSiteTable(dbConnection, dbDriverChar)

# load visibility from local MS-SQL DB
# =============================================================================
  message("Extracting visibility table from DB...")
  visibilityData = getVisibilityTable(dbConnection, dbDriverChar)

# load manual visibility from local MS-SQL DB
# =============================================================================
  message("Extracting MANUAL visibility table from DB...")
  manualVisibilityTable = try(getManualVisibilityTable(dbConnection, 
                                                       dbDriverChar), 
                              silent = TRUE)
  if (is.data.frame(manualVisibilityTable)){  
    message("MANUAL visibility table extracted")
  } else {
    message("MANUAL visibility table not in DB")
    rm(manualVisibilityTable)
  }

# load time bins from local MS-SQL DB
# =============================================================================
  message("Extracting time_bins table from DB...")
  timeBinsTable = getTimeBinsTable(dbConnection, dbDriverChar)

# load weather from local MS-SQL DB
# =============================================================================
  message("Extracting weather table from DB...")
  weatherTable = QUERY(dbConnection, dbDriverChar, "Select * From weather")

# load weather properties from local MS-SQL DB
# =============================================================================
  message("Extracting weather_property table from DB...")
  weatherPropertyTable = QUERY(dbConnection, dbDriverChar, 
                               "Select * From weather_property")
  weatherPropertyList  = weatherTable$weather_property
  weatherTable$weather_property = weatherPropertyTable$property_name[match(weatherPropertyList, 
                                                                           weatherPropertyTable$id)]
  weather = reshape2::dcast(weatherTable, 
                            time_bin ~ weather_property, 
                            value.var     = "value", 
                            fun.aggregate = mean)
  rm(list = "weatherTable", "weatherPropertyTable", "weatherPropertyList")

# get all listed rf features
# =============================================================================
  message("Extracting rffeatures table from DB...")
  echoRfFeatureMap = getEchoFeatures(dbConnection, dbDriverChar, 
                                     listOfRfFeaturesToExtract = listOfRfFeaturesToExtract)   

# load rf classification
# =============================================================================
  message("Extracting RF classification...")
  rfclassificationTable = getRfClassification(dbConnection, dbDriverChar)

# load echo validation from local MS-SQL DB
# =============================================================================
  message("Extracting echo_validation table from DB...")
  echovalidationTable = getEchoValidationTable(dbConnection, dbDriverChar)

# Merge echo Data
# =============================================================================
  echoData = collectionTable
  names(echoData)[names(echoData) == "row"] = "echo"
  if (!is.null(echoRfFeatureMap$echoRfFeatureMap)){
    echoData = merge(echoData, echoRfFeatureMap$echoRfFeatureMap, 
                     by = "echo", all.x = TRUE, all.y = FALSE)
  }
  echoData = merge(echoData, echovalidationTable, 
                   by = "echo", all.x = TRUE, all.y = FALSE)
  echoData = merge(echoData, rfclassificationTable$rfclassificationTable, 
                   by = "echo", all.x = TRUE, all.y = FALSE)
  availableClasses = rfclassificationTable$availableClasses
  classProbabilitiesAndMtrFactors = rfclassificationTable$classProbabilitiesAndMtrFactors
  rfFeatures                      = echoRfFeatureMap$rfFeatures
  rm(collectionTable, echoRfFeatureMap, 
     echovalidationTable, rfclassificationTable)

# rename protocolTable
# =============================================================================
  protocolData = protocolTable
  rm(protocolTable)

# Merge site Data
# =============================================================================
  siteData = merge(siteTable, radarTable, by = "radarID", all = TRUE)
  rm(siteTable, radarTable)

# Merge timebin Data
# =============================================================================
  timeBinData = timeBinsTable
  names(timeBinData)[names(timeBinData) == "id"] = "time_bin"
  timeBinData = merge(timeBinData, weather, 
                      by = "time_bin", all.x =TRUE, all.y = FALSE)
  rm(timeBinsTable, weather)
 
# insert a.s.l. altitude column to echoData
# =============================================================================
  asl      = data.frame("feature1.altitude_ASL" = echoData$feature1.altitude_AGL) + 
              siteData$altitude
  echoData = data.frame(echoData[, 1:match("feature1.altitude_AGL", 
                                           names(echoData))], 
                        asl, 
                        echoData[, (match("feature1.altitude_AGL", 
                                          names(echoData))+1):length(echoData)])
  rm(asl)
 
# get radarTZ from siteData (or siteTable)
# =============================================================================
  if (is.null(radarTimeZone)){
    # Get time zone saved in the database table 'dbo.site'
      tz_shift = as.numeric(siteData$timeShift) 
      
    if (is.na(tz_shift) | is.null(tz_shift)){
      stop("set a radarTimeZone, or update the timeshift column in the dbo-site 
           table")
    }
    if (tz_shift >= 0 | tz_shift < 0){
      radarTimeZone = paste0("Etc/GMT", ifelse(tz_shift >=0 , "-", "+"), 
                             abs(tz_shift)) # note that "UTC+1" is denoted as "Etc/GMT-1"
      message(paste0("Radar time zone extracted from dbo.site is :", 
                     radarTimeZone))
    }
  }
  TimeZone = data.frame("radarTimeZone"  = radarTimeZone,
                        "targetTimeZone" = targetTimeZone)
   
# time zone conversion
# =============================================================================
  visibilityData = convertTimeZone(data     = visibilityData, 
                                   colNames = c("blind_from", "blind_to"), 
                                   originTZ = radarTimeZone, 
                                   targetTZ = targetTimeZone)
  protocolData   = convertTimeZone(data     = protocolData, 
                                   colNames = c("startTime", "stopTime"), 
                                   originTZ = radarTimeZone, 
                                   targetTZ = targetTimeZone)
  siteData       = convertTimeZone(data     = siteData, 
                                   colNames = c("projectStart", "projectEnd"), 
                                   originTZ = radarTimeZone, 
                                   targetTZ = targetTimeZone)
  timeBinData    = convertTimeZone(data     = timeBinData, 
                                   colNames = c("time_start", "time_stop"), 
                                   originTZ = radarTimeZone, 
                                   targetTZ = targetTimeZone)
  echoData       = convertTimeZone(data     = echoData, 
                                   colNames = c("time_stamp"), 
                                   originTZ = radarTimeZone, 
                                   targetTZ = targetTimeZone)
     
# Define output list with all output data
# =============================================================================
  # CASE: manual VisibilityTable
  # ===========================================================================
    if (exists("manualVisibilityTable")){ 
      outputList = list(echoData                        = echoData, 
                        protocolData                    = protocolData,
                        siteData                        = siteData, 
                        visibilityData                  = visibilityData,
                        manualVisibilityTable           = manualVisibilityTable,
                        timeBinData                     = timeBinData,
                        availableClasses                = availableClasses,
                        rfFeatures                      = rfFeatures,
                        TimeZone                        = TimeZone,
                        classProbabilitiesAndMtrFactors = classProbabilitiesAndMtrFactors)
      
  # CASE: automatic VisibilityTable
  # ===========================================================================
    } else {
      outputList = list(echoData                        = echoData, 
                        protocolData                    = protocolData,
                        siteData                        = siteData, 
                        visibilityData                  = visibilityData,
                        timeBinData                     = timeBinData,
                        availableClasses                = availableClasses,
                        rfFeatures                      = rfFeatures,
                        TimeZone                        = TimeZone,
                        classProbabilitiesAndMtrFactors = classProbabilitiesAndMtrFactors)
    }
  
# Start sunrise/sunset and twilight information calculation
# =============================================================================
  message("Computing sunrise/sunset and twilight information..")
  
# Set min and max time_stamps of echodata as time range for 
# sunrise/sunset calculation
# ===========================================================================
  timeRangeSunriseSunset = c(min(outputList$echoData$time_stamp_targetTZ), 
                             max(outputList$echoData$time_stamp_targetTZ))  

# Compute sunrise/sunset and dawn/dusk (civil)
# ===========================================================================
  sunriseSunset = twilight(timeRange = timeRangeSunriseSunset, 
                           latLon    = siteLocation, 
                           timeZone  = TimeZone$targetTimeZone)

# Add the sunrise/sunset information to the output list
# ===========================================================================
  outputList$sunriseSunset = sunriseSunset  
  rm(sunriseSunset)

# Start adding day/night information for each echo
# ===========================================================================
  message("Adding day/night information per echo..")
        
# Add day/night infor per echo
# =====================================================================
  outputList$echoData = addDayNightInfoPerEcho(echoData      = outputList$echoData,
                                               sunriseSunset = outputList$sunriseSunset, 
                                               sunOrCivil    = sunOrCivil)
  
# Create the output directory if it doesn't exist
# =============================================================================
  dir.create(dbDataDir, showWarnings = FALSE, recursive = TRUE)
     
# save DB Data to a file, if requested
# =============================================================================
  if (saveDbToFile){
    outputFileName = file.path(dbDataDir, 
                               paste0(dbName, "_DataExtract.rds"))
    saveRDS(outputList, file = outputFileName)
  }

# close database connections
# =============================================================================
  if (dbDriverChar != "PostgreSQL") {
    RODBC::odbcCloseAll() 
  } else {
    DBI::dbDisconnect(dbConnection)
  }      

# Return output
# =============================================================================
  return(outputList)

# =============================================================================
# =============================================================================
# End of Function
# =============================================================================
# =============================================================================
}