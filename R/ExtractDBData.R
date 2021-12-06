#### extractDbData ------------------------------------------------------------
#' @title  Extract DB Data
#' @description  Load the data from the database or file and save it to file
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbDriver="{SQL Server Native Client 11.0}" the name of the driver. If different from 'PostgreSQL' it connects to cloud.birdradar.com
#' @param dbServer="" the name of the Server
#' @param dbName=""   the name of the Database
#' @param dbUser="" the USER name of the Server
#' @param dbPwd=""   the password for the user name
#' @param radarTimeZone=NULL NULL or a string specifying the radar time zone
#' @param targetTimeZone=NULL  NULL or a string specifying the target time zone
#' @param forceToExtractDataFromDatabase=FALSE if TRUE, it opens a connection to the Database requiring username and password, default FALSE
#' @param listOfRfFeaturesToExtract=NULL  NULL or a list of feature to extract
#'
#' @return a list of R objects with data extracted from the Database echoData,  protocolData, siteData, visibilityData, timeBinData, rfFeatures, availableClasses, classProbabilitiesAndMtrFactors
#' 
extractDbData = function( dbDriver = "{SQL Server Native Client 11.0}", dbServer = "", dbName = "",  dbUser = "", dbPwd = "", radarTimeZone = NULL, targetTimeZone = NULL, forceToExtractDataFromDatabase = FALSE, listOfRfFeaturesToExtract = NULL )
{
   dbDataName <- paste( "DB_Data", dbName, sep= "_" )
   dbDataName <- paste( dbDataName, "Rdata", sep= "." )
   
   # Extract data from Database if required or forced
   if( ( forceToExtractDataFromDatabase == TRUE || !file.exists( file.path( dbDataDir, dbDataName ) ) )
       && dbServer != "" && dbName != ""
       && !is.null( targetTimeZone ) )
   {
      
      # Open the database connection
      if(dbDriver != 'PostgreSQL') {
         if(nchar(dbUser) != 0 | nchar(dbPwd) != 0 ){
            dsn = paste0("driver={SQL Server Native Client 11.0};server=", dbServer,
                         ";database=", dbName,
                         ";uid=", dbUser,
                         ";pwd=", dbPwd
            )
         } else { # request the username and pwd via the rstudioAPI
            dsn = paste0("driver={SQL Server Native Client 11.0};server=", dbServer,
                         ";database=", dbName,
                         ";uid=", rstudioapi::askForPassword("Database user"),
                         ";pwd=", rstudioapi::askForPassword("Database password")
            )
         }
         dbConnection <- odbcDriverConnect( dsn )
      } else {
         if(nchar(dbUser) != 0 | nchar(dbPwd) != 0 ){
            dbConnection = DBI::dbConnect('PostgreSQL',
                                           host='cloud.birdradar.com',
                                           dbname = dbName,
                                           user = dbUser,
                                           password = dbPwd
            )
         } else { # request the username and pwd via the rstudioAPI
            dbConnection = DBI::dbConnect('PostgreSQL',
                                           host='cloud.birdradar.com',
                                           dbname = dbName,
                                           user = rstudioapi::askForPassword("Database user"),
                                           password = rstudioapi::askForPassword("Database password")
            )
         }
      }
      
      
      
      
      if( 
         if(dbDriver == 'PostgreSQL') 
         {
            isPostgresqlIdCurrent(dbConnection)
         } else {
            dbConnection != -1
         } 
      )
      {
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # load collection table
         message( "Extracting collection table from DB..." )
         collectionTable <- getCollectionTable( dbConnection )
         
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # load protocol from local MS-SQL DB
         message( "Extracting protocol table from DB..." )
         protocolTable = getProtocolTable( dbConnection )
         
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # load radar from local MS-SQL DB
         message( "Extracting radar table from DB..." )
         radarTable = getRadarTable( dbConnection )
         
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # load site from local MS-SQL DB
         message( "Extracting site table from DB..." )
         siteTable = getSiteTable( dbConnection )
         
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # load visibility from local MS-SQL DB
         message( "Extracting visibility table from DB..." )
         visibilityTable = getVisibilityTable( dbConnection )
         visibilityData <- visibilityTable
         rm( visibilityTable )
         
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # load time bins from local MS-SQL DB
         message( "Extracting time_bins table from DB..." )
         timeBinsTable = getTimeBinsTable( dbConnection )
         
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # load weather from local MS-SQL DB
         message( "Extracting weather table from DB..." )
         weatherTable = QUERY(dbConnection, 
                              "Select * From weather"
         )
         
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # load weather properties from local MS-SQL DB
         message( "Extracting weather_property table from DB..." )
         weatherPropertyTable = QUERY(dbConnection, 
                                      "Select * From weather_property"
         )
         
         weatherPropertyList <- weatherTable$weather_property
         weatherTable$weather_property <- weatherPropertyTable$property_name[ match( weatherPropertyList, weatherPropertyTable$id ) ]
         weather = dcast( weatherTable, time_bin ~ weather_property, value.var = "value", fun.aggregate = mean )
         rm( list = "weatherTable", "weatherPropertyTable", "weatherPropertyList" )
         
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # get all listed rf features
         message( "Extracting rffeatures table from DB..." )
         echoRfFeatureMap <- getEchoFeatures( dbConnection, listOfRfFeaturesToExtract = listOfRfFeaturesToExtract )   
         
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # load rf classification
         message( "Extracting RF classification..." )
         rfclassificationTable <- getRfClassification( dbConnection )
         
         # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # load echo validation from local MS-SQL DB
         message( "Extracting echo_validation table from DB..." )
         echovalidationTable <- getEchoValidationTable( dbConnection )
         
         # Merge echo Data
         echoData <- collectionTable
         names( echoData )[ names( echoData ) == "row"] <- "echo"
         if( !is.null( echoRfFeatureMap$echoRfFeatureMap ) )
         {
            echoData <- merge( echoData, echoRfFeatureMap$echoRfFeatureMap, by = "echo", all.x = TRUE, all.y = FALSE )      
         }
         echoData <- merge( echoData, echovalidationTable, by = "echo", all.x = TRUE, all.y = FALSE )
         echoData <- merge( echoData, rfclassificationTable$rfclassificationTable, by = "echo", all.x = TRUE, all.y = FALSE )
         availableClasses <- rfclassificationTable$availableClasses
         classProbabilitiesAndMtrFactors <- rfclassificationTable$classProbabilitiesAndMtrFactors
         rfFeatures <- echoRfFeatureMap$rfFeatures
         rm( collectionTable, echoRfFeatureMap, echovalidationTable, rfclassificationTable )
         
         # rename protocolTable
         protocolData <- protocolTable
         rm( protocolTable )
         
         # Merge site Data
         siteData <- merge( siteTable, radarTable, by = "radarID", all = TRUE )
         rm( siteTable, radarTable )
         
         # Merge timebin Data
         timeBinData <- timeBinsTable
         names( timeBinData )[ names( timeBinData ) == "id"] <- "time_bin"
         timeBinData <- merge( timeBinData, weather, by = "time_bin", all.x =TRUE, all.y = FALSE )
         rm( timeBinsTable, weather )
         
         # insert a.s.l. altitude column to echoData
         asl <- data.frame( "feature1.altitude_ASL" = echoData$feature1.altitude_AGL ) + siteData$altitude
         echoData <- data.frame( echoData[ , 1:match( "feature1.altitude_AGL", names( echoData ) ) ], asl, echoData[ , ( match( "feature1.altitude_AGL", names( echoData ) ) + 1 ) : length( echoData ) ] )
         rm( asl )
         
         # get radarTZ from siteData (or siteTable)
         if( is.null( radarTimeZone ) ) {
            tz_shift <- as.numeric(siteData$timeShift) # Get time zone saved in the database table 'dbo.site'
            if(is.na(tz_shit) | is.null(tz_shit) ) stop("set a radarTimeZone, or update the timeshift column in the dbo-site table")
            if(tz_shit >= 0 | tz_shit < 0){
               radarTimeZone <- paste0("Etc/GMT", ifelse(tz_shift >=0 , "-", "+"), abs(tz_shift)) # note that "UTC+1" is denoted as "Etc/GMT-1"
               message( paste0( "Radar timezone extracted from dbo.site is :", radarTimeZone) )
            }
         }
         TimeZone <- data.frame("radarTimeZone" = radarTimeZone,
                                "targetTimeZone" = targetTimeZone)
         # timezone conversion
         visibilityData <- convertTimeZone( data = visibilityData, colNames = c( "blind_from", "blind_to" ), originTZ = radarTimeZone, targetTZ = targetTimeZone )
         protocolData <- convertTimeZone( data = protocolData, colNames = c( "startTime", "stopTime" ), originTZ = radarTimeZone, targetTZ = targetTimeZone )
         siteData <- convertTimeZone( data = siteData, colNames = c( "projectStart", "projectEnd" ), originTZ = radarTimeZone, targetTZ = targetTimeZone )
         timeBinData <- convertTimeZone( data = timeBinData, colNames = c( "time_start", "time_stop" ), originTZ = radarTimeZone, targetTZ = targetTimeZone )
         echoData <- convertTimeZone( data = echoData, colNames = c( "time_stamp" ), originTZ = radarTimeZone, targetTZ = targetTimeZone )
         
         # Create directory if not existing
         ifelse( !dir.exists( dbDataDir ), dir.create( dbDataDir ), FALSE )
         
         # save DB Data
         save( echoData, 
               protocolData,
               siteData, 
               visibilityData,
               timeBinData,
               availableClasses,
               rfFeatures,
               TimeZone,
               classProbabilitiesAndMtrFactors,
               file = file.path( dbDataDir, dbDataName ) )
         
         # close database connections
         if(dbDriver != 'PostgreSQL') {
            odbcCloseAll() 
         } else {
            dbDisconnect(dbConnection)
         }      
      } else
      {
         warning( "Could not open database. Make sure to set dbServer, dbName and credentials right.")
      }
   } else
   {
      load( file.path( dbDataDir, dbDataName ) )
      
      extractedDataLoaded <- exists( echoDataName ) && 
         exists( protocolDataName ) &&
         exists( siteDataName ) &&
         exists( visibilityDataName ) &&
         exists( timeBinDataName ) &&
         exists( availableClassesName ) &&
         exists( rfFeaturesName ) &&
         exists( classProbabilitiesAndMtrFactorsName )
      
      if( extractedDataLoaded == FALSE )
      {
         stop( "Could not load data extracted from database. Extract data from database first.")
         return()
      }
   }
   
   return( list( echoData = echoData, 
                 protocolData = protocolData,
                 siteData = siteData, 
                 visibilityData = visibilityData,
                 timeBinData = timeBinData,
                 availableClasses = availableClasses,
                 rfFeatures = rfFeatures,
                 classProbabilitiesAndMtrFactors = classProbabilitiesAndMtrFactors ) )
   
}


#### QUERY ------------------------------------------------------------
#' @title  Query SQL database
#' @description  Run an SQL query on an already connected DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#' @param query an SQL string with your query
#' @param as.is=FALSE If TRUE, leaves data as it is.
#'
#' @return the result of the query
#' 
QUERY <- function(dbConnection, query, as.is=FALSE){
   if(dbDriver == 'PostgreSQL'){
      t  <- dbGetQuery(dbConnection, query, as.is=as.is)
   } else {
      t  <- sqlQuery(dbConnection, query,as.is=as.is)
   }
   return(t)
}

#### getRadarTable ------------------------------------------------------------
#' @title  Get a BirdScan radar table
#' @description  get the Radar table from  an already connected DB and rename the columns appropiately
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#'
#' @return the radar table  as a data frame
#' 
getRadarTable = function( dbConnection )
{
   
   radarTable = QUERY(dbConnection, "Select * From radar")
   
   
   colnames(radarTable)[colnames(radarTable) == "radarid"] <- "radarID"
   colnames(radarTable)[colnames(radarTable) == "serialno"] <- "serialNo"
   colnames(radarTable)[colnames(radarTable) == "northoffset"] <- "northOffset"
   colnames(radarTable)[colnames(radarTable) == "short0v"] <- "short0V"
   colnames(radarTable)[colnames(radarTable) == "medium0v"] <- "medium0V"
   colnames(radarTable)[colnames(radarTable) == "long0v"] <- "long0V"
   colnames(radarTable)[colnames(radarTable) == "shortsteepness"] <- "shortSteepness"
   colnames(radarTable)[colnames(radarTable) == "mediumsteepness"] <- "mediumSteepness"
   colnames(radarTable)[colnames(radarTable) == "longsteepness"] <- "longSteepness"
   colnames(radarTable)[colnames(radarTable) == "shortsatlower"] <- "shortSatLower"
   colnames(radarTable)[colnames(radarTable) == "mediumsatlower"] <- "mediumSatLower"
   colnames(radarTable)[colnames(radarTable) == "longsatlower"] <- "longSatLower"
   colnames(radarTable)[colnames(radarTable) == "antennagainindbi"] <- "antennaGainInDBi"
   colnames(radarTable)[colnames(radarTable) == "waveguideattenuation"] <- "waveGuideAttenuation"
   colnames(radarTable)[colnames(radarTable) == "pulselengthshort"] <- "pulseLengthShort"
   colnames(radarTable)[colnames(radarTable) == "pulselengthmedium"] <- "pulseLengthMedium"
   colnames(radarTable)[colnames(radarTable) == "pulselengthlong"] <- "pulseLengthLong"
   colnames(radarTable)[colnames(radarTable) == "tiltangle"] <- "tiltAngle"
   colnames(radarTable)[colnames(radarTable) == "transmitpower"] <- "transmitPower"
   
   return( radarTable )
}

#### getEchoValidationTable ------------------------------------------------------------
#' @title  Get a BirdScan echo validation table
#' @description  gets the EchoValidationTable from an already connected DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#'
#' @return A dataframe called echovalidationTable
#'
getEchoValidationTable = function( dbConnection )
{
   echovalidationTypesTable = QUERY(dbConnection, 
                                    "Select * From echo_validation_type"
   )
   
   echovalidationTable = QUERY(dbConnection, 
                               "Select * From echo_validation order by echo_id asc"
   )
   
   echoValidationList <- echovalidationTable$type
   echovalidationTable$type <- echovalidationTypesTable$name[ match( echoValidationList, echovalidationTypesTable$id ) ]
   rm( list = "echovalidationTypesTable", "echoValidationList" )
   names( echovalidationTable )[ names( echovalidationTable ) == "echo_id"] <- "echo"
   names( echovalidationTable )[ names( echovalidationTable ) == "type"] <- "echoValidationType"
   
   return( echovalidationTable )
}

#### getRfClassification ------------------------------------------------------------
#' @title  Get a BirdScan RFClassification table
#' @description  gets  rfclasses from local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#'
#' @return A dataframe called rfclasses
#'
getRfClassification = function( dbConnection )
{
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load rfclasses from local MS-SQL DB
   rfClasses = QUERY(dbConnection, 
                     "Select * From rfclasses"
   )
   
   colnames(rfClasses)[colnames(rfClasses) == "is_protected"] <- "isProtected"
   colnames(rfClasses)[colnames(rfClasses) == "sphere_dia_cm"] <- "sphereDiaCm"
   colnames(rfClasses)[colnames(rfClasses) == "is_used_for_classification"] <- "isUsedForClassification"
   
   availableClasses <- rfClasses[ rfClasses$isUsedForClassification == 1, ]
   availableClasses$name <- as.character( availableClasses$name )
   availableClasses$description <- as.character( availableClasses$description )
   
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load rfclassification from local MS-SQL DB
   rfclassificationTable = QUERY(dbConnection, 
                                 "select * from rf_classification where rf_classification.class is not null and rf_classification.mtr_factor is not null order by echo asc"
   )
   
   rfClassificationList <- rfclassificationTable$class
   rfclassificationTable$class <- availableClasses$name[ match( rfClassificationList, availableClasses$id ) ]
   
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load rfclassification probabilities from local MS-SQL DB
   rfclassProbabilityTable = QUERY(dbConnection, 
                                   "Select * From rf_class_probability where rf_class_probability.class is not null order by echo asc, class asc"
   )
   
   rfClassList <- rfclassProbabilityTable$class
   rfclassProbabilityTable$class <- availableClasses$name[ match( rfClassList, availableClasses$id ) ]
   if( nrow( rfclassProbabilityTable ) > 0 )
   {
      classProbabilites <- dcast( rfclassProbabilityTable[ !is.na(rfclassProbabilityTable$class) & !is.na(rfclassProbabilityTable$echo), ], echo ~ class, value.var="value", fun.aggregate = mean )
      MTRFactorPerClass <- dcast( rfclassProbabilityTable[ !is.na(rfclassProbabilityTable$class) & !is.na(rfclassProbabilityTable$echo), ], echo ~ class, value.var="mtr_factor", fun.aggregate = mean )
      names( classProbabilites )[ !names( classProbabilites ) == "echo" ] <- paste( "classProb.", names( classProbabilites )[ !names( classProbabilites ) == "echo" ], sep = "" )
      names( MTRFactorPerClass )[ !names( MTRFactorPerClass ) == "echo" ] <- paste( "MTRFact.", names( MTRFactorPerClass )[ !names( MTRFactorPerClass ) == "echo" ], sep = "" )
      classProbabilitiesAndMtrFactors <- merge( classProbabilites, MTRFactorPerClass, by = "echo", all.x =TRUE, all.y = FALSE )
   } else
   {
      classProbabilitiesAndMtrFactors <- data.frame()
   }
   
   names( rfclassificationTable )[ names( rfclassificationTable ) == "mtr_factor" ] <- "mtr_factor_rf"
   
   return( list( rfclassificationTable = rfclassificationTable, classProbabilitiesAndMtrFactors = classProbabilitiesAndMtrFactors, availableClasses = availableClasses ) )
}

#### getEchoFeatures ------------------------------------------------------------
#' @title  Get BirdScan echo features
#' @description load echo rffeature map from local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#' @param listOfRfFeaturesToExtract a list of feature to extract
#'
#' @return A list of the features extracted
#'
getEchoFeatures = function( dbConnection, listOfRfFeaturesToExtract )
{
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load echo rffeatures from local MS-SQL DB
   rffeaturesTable = QUERY(dbConnection, 
                           "Select * From rffeatures"
   )
   
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load echo rffeature map from local MS-SQL DB
   if( !is.null( listOfRfFeaturesToExtract ) )
   {
      echorffeaturesMapTable = QUERY(dbConnection, 
                                     paste( "Select * From echo_rffeature_map where feature in ( ", paste( listOfRfFeaturesToExtract, collapse = ", " ), " )" )
      )   
      
      featurelist <- echorffeaturesMapTable$feature
      echorffeaturesMapTable$feature <- rffeaturesTable$name[ match( featurelist, rffeaturesTable$id ) ]
      echoRfFeatureMap = dcast( echorffeaturesMapTable, echo ~ feature, value.var = "value", fun.aggregate = mean )
      
      return( list( echoRfFeatureMap = echoRfFeatureMap, rfFeatures = rffeaturesTable ) )
   }
   
   return( list( echoRfFeatureMap = NULL, rfFeatures = rffeaturesTable ) )
}

#### getCollectionTable ------------------------------------------------------------
#' @title  Get BirdScan collection table
#' @description load collection from local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#'
#' @return A dataframe with the collection table
#'
getCollectionTable = function( dbConnection )
{
   # ::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
   # load collection from local MS-SQL DB
   if(dbDriver != 'PostgreSQL') {
      collectionTable = QUERY( dbConnection,paste0("Select * From collection Where time_stamp < '", timing ,"' order by row asc"))
      collectionTable_time_stamp = QUERY( dbConnection, paste0("Select time_stamp From collection Where time_stamp < '", timing ,"' order by row asc"), as.is = TRUE)
      collectionTable$time_stamp <- collectionTable_time_stamp$time_stamp
   } else {
      collectionTable = QUERY( dbConnection,paste0("Select *, time_stamp::character varying ts From collection Where time_stamp < '", timing ,"' order by row asc"))
      collectionTable$time_stamp <- collectionTable$ts
      collectionTable$ts <-  NULL
      #colnames(collectionTable)[colnames(collectionTable) == "ts"] <- "time_stamp"
   }
   colnames(collectionTable)[colnames(collectionTable) == "echoid"] <- "echoID"
   colnames(collectionTable)[colnames(collectionTable) == "protocolid"] <- "protocolID"
   
   # rename "old" features and remove unused feature columns in collectionTable
   featureCols <- match( names( collectionTable ), featureNames$feature )
   colNames <- paste( featureNames$feature, featureNames$featureNames, sep = "." )
   colNames[ is.na( featureNames$featureNames ) ] <- NA
   names( collectionTable )[ !is.na( featureCols ) ] <- colNames [ na.omit( featureCols ) ]
   collectionTable <- collectionTable[ !is.na( names( collectionTable ) ) ]
   
   names( collectionTable )[ names( collectionTable ) == "mtr_fact" ] <- "mtr_factor_old"
   names( collectionTable )[ names( collectionTable ) == "statistical_classification" ] <- "statistical_classification_old"
   
   return( collectionTable )
}

#### getProtocolTable ------------------------------------------------------------
#' @title  Get BirdScan protocol table
#' @description load protocol table from an already connect local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#'
#' @return A dataframe with the protocal table
#'
getProtocolTable = function( dbConnection )
{
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load protocol table from local MS-SQL DB
   if(dbDriver != 'PostgreSQL') {
      protocolTable = QUERY(dbConnection, "Select * From protocol order by protocolID asc")
      colnames(protocolTable)[colnames(protocolTable) == "starttime"] <- "startTime"
      colnames(protocolTable)[colnames(protocolTable) == "stoptime"] <- "stopTime"
      
      protocolTable_times = QUERY(dbConnection, "Select startTime, stopTime From protocol order by protocolID asc", as.is = TRUE)
      colnames(protocolTable_times)[colnames(protocolTable_times) == "starttime"] <- "startTime"
      colnames(protocolTable_times)[colnames(protocolTable_times) == "stoptime"] <- "stopTime"
      protocolTable$startTime <- protocolTable_times$startTime
      protocolTable$stopTime <- protocolTable_times$stopTime
      
   } else {
      protocolTable = QUERY(dbConnection, "Select *,starttime::character varying as start,stoptime::character varying as stop From protocol order by protocolid asc")
      protocolTable$starttime <- protocolTable$start
      protocolTable$stoptime <- protocolTable$stop
      colnames(protocolTable)[colnames(protocolTable) == "starttime"] <- "startTime"
      colnames(protocolTable)[colnames(protocolTable) == "stoptime"] <- "stopTime"
      protocolTable$start <- NULL
      protocolTable$stop <- NULL
   }
   
   colnames(protocolTable)[colnames(protocolTable) == "protocolid"] <- "protocolID"
   colnames(protocolTable)[colnames(protocolTable) == "protocolname"] <- "protocolName"
   colnames(protocolTable)[colnames(protocolTable) == "siteid"] <- "siteID"
   colnames(protocolTable)[colnames(protocolTable) == "pulsetype"] <- "pulseType"
   colnames(protocolTable)[colnames(protocolTable) == "hystfact"] <- "hystFact"
   colnames(protocolTable)[colnames(protocolTable) == "autothreshold"] <- "autoThreshold"
   colnames(protocolTable)[colnames(protocolTable) == "visibilitythreshold"] <- "visibilityThreshold"
   colnames(protocolTable)[colnames(protocolTable) == "blocktime"] <- "blockTime"
   colnames(protocolTable)[colnames(protocolTable) == "maxmatcherror"] <- "maxMatchError"
   colnames(protocolTable)[colnames(protocolTable) == "measurementnoisefactor"] <- "measurementNoiseFactor"
   colnames(protocolTable)[colnames(protocolTable) == "clutterrejectiondb"] <- "clutterRejectionDB"
   colnames(protocolTable)[colnames(protocolTable) == "clutterrejectionseconds"] <- "clutterRejectionSeconds"
   colnames(protocolTable)[colnames(protocolTable) == "clutterfactor"] <- "clutterFactor"
   
   return( protocolTable )
}

#### getVisibilityTable ------------------------------------------------------------
#' @title  Get BirdScan visibility table
#' @description load visibility table from an already connect local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#'
#' @return A dataframe with the visibility table
#'
getVisibilityTable = function( dbConnection )
{
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load protocol table from local MS-SQL DB
   if(dbDriver != 'PostgreSQL') {
      visibilityTable = QUERY(dbConnection, "Select * From visibility order by visibilityLogID asc")
      visibilityTable_times = QUERY(dbConnection, "Select blind_from, blind_to From visibility order by visibilityLogID asc", as.is = TRUE)
      visibilityTable$blind_from <- visibilityTable_times$blind_from
      visibilityTable$blind_to <- visibilityTable_times$blind_to
   } else {
      visibilityTable = QUERY(dbConnection, "Select *,blind_from::character varying as blindfrom,blind_to::character varying as blindto From visibility order by visibilitylogid asc")
      visibilityTable$blind_from <- visibilityTable$blindfrom 
      visibilityTable$blind_to <-  visibilityTable$blindto 
      visibilityTable$blindfrom <- NULL
      visibilityTable$blindto <-  NULL
   }
   
   colnames(visibilityTable)[colnames(visibilityTable) == "visibilitylogid"] <- "visibilityLogID"
   colnames(visibilityTable)[colnames(visibilityTable) == "protocolid"] <- "protocolID"
   
   
   
   return( visibilityTable )
}

#### getSiteTable ------------------------------------------------------------
#' @title  Get BirdScan site table
#' @description load site table from an already connect local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid database connection
#'
#' @return A dataframe with the site table
#'
getSiteTable = function( dbConnection )
{
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load protocol table from local MS-SQL DB
   siteTable = QUERY(dbConnection, 
                     "Select * From site order by row asc"
   )
   
   colnames(siteTable)[colnames(siteTable) == "siteid"] <- "siteID"
   colnames(siteTable)[colnames(siteTable) == "sitecode"] <- "siteCode"
   colnames(siteTable)[colnames(siteTable) == "radarid"] <- "radarID"
   colnames(siteTable)[colnames(siteTable) == "sitename"] <- "siteName"
   colnames(siteTable)[colnames(siteTable) == "sitedesc"] <- "siteDesc"
   colnames(siteTable)[colnames(siteTable) == "projectstart"] <- "projectStart"
   colnames(siteTable)[colnames(siteTable) == "projectend"] <- "projectEnd"
   colnames(siteTable)[colnames(siteTable) == "timeshift"] <- "timeShift"
   colnames(siteTable)[colnames(siteTable) == "radarorientation"] <- "radarOrientation"
   colnames(siteTable)[colnames(siteTable) == "ftpupload"] <- "ftpUpload"
   colnames(siteTable)[colnames(siteTable) == "automode"] <- "autoMode"
   
   siteTable_times = QUERY(dbConnection, 
                           "Select projectStart, projectEnd From site order by row asc", as.is = TRUE
   )
   
   colnames(siteTable_times)[colnames(siteTable_times) == "projectstart"] <- "projectStart"
   colnames(siteTable_times)[colnames(siteTable_times) == "projectend"] <- "projectEnd"
   
   siteTable$projectStart <- siteTable_times$projectStart
   siteTable$projectEnd <- siteTable_times$projectEnd
   
   return( siteTable )
}

#### getTimeBinsTable ------------------------------------------------------------
#' @title  Get BirdScan timebins table
#' @description load timebins table from an already connect local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid database connection
#'
#' @return A dataframe with the timebins table
#'
getTimeBinsTable = function( dbConnection )
{
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load protocol table from local MS-SQL DB
   if(dbDriver != 'PostgreSQL') {
      timeBinsTable = QUERY(dbConnection, "Select * From time_bins order by id asc")
      timeBinsTable_times = QUERY(dbConnection,"Select time_start, time_stop From time_bins order by id asc", as.is = TRUE)
      timeBinsTable$time_start <- timeBinsTable_times$time_start
      timeBinsTable$time_stop <- timeBinsTable_times$time_stop
   } else {
      timeBinsTable = QUERY(dbConnection, "Select *,time_start::character varying as start,time_stop::character varying as stop FROM time_bins order by id asc")
      timeBinsTable$time_start <- timeBinsTable$start
      timeBinsTable$time_stop <- timeBinsTable$stop
      timeBinsTable$start <- NULL
      timeBinsTable$stop <- NULL
   }
   
   colnames(timeBinsTable)[colnames(timeBinsTable) == "siteid"] <- "siteID"
   
   return( timeBinsTable )
}

#extractDbData( dbServer = dbServer, dbName = dbName, radarTimeZone = radarTimeZone, targetTimeZone = targetTimeZone, forceToExtractDataFromDatabase = forceToExtractDataFromDatabase, listOfRfFeaturesToExtract = listOfRfFeaturesToExtract )



