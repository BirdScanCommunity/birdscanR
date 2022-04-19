#### getCollectionTable ------------------------------------------------------------
#' @title  Get BirdScan collection table
#' @description load collection from local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}, Birgen Haest (SOI) \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' it connects to cloud.birdradar.com
#'
#' @return A dataframe with the collection table
#' @export
#'
getCollectionTable = function( dbConnection , dbDriverChar){
  # Set feature name translations
  # ===========================================================================
    featureNames = data.frame(feature = c("feature1", "feature2", "feature3", 	
                                          "feature4", "feature5", "feature6", 	
                                          "feature7", "feature8", "feature9", 	
                                          "feature10", "feature11", "feature12",
                                          "feature13", "feature14", "feature15", 
                                          "feature16", "feature17", "feature18",
                                          "feature19", "feature20", "feature21",
                                          "feature22", "feature23", "feature24",
                                          "feature25", "feature26", "feature27",
                                          "feature28", "feature29", "feature30",
                                          "feature31", "feature32", "feature33",
                                          "feature34", "feature35", "feature36", 
                                          "feature37", "feature38", "feature39",
                                          "feature40", "feature41", "feature42", 
                                          "feature43", "feature44", "feature45",
                                          "feature46", "feature47", "feature48",
                                          "feature49", "feature50"), 
                              featureNames = c("altitude_AGL",         # Altitude above ground level (m)
                                               "azimuth",              # Azimuth (true north)
                                               "speed",                # Speed (m/s)
                                               NA, NA, "rotationFreq", # Rotation frequency in relation to sampling frequency
                                               NA, NA, NA, NA, NA, NA,
                                               "freqRatio",            # Frequency Ratio
                                               "maxLevel",             # Maximum level
                                               "polRatio",             # Polarisation ratio
                                               "absPolarisation",      # Absolute polarisation
                                               "rcs",                  # Radar cross section (m^2)
                                               "sqrt(RCS)",            # Square root of radar cross section (m)
                                               "durationOfEcho",       # Duration of echo (seconds)
                                               "durationOfEchoInSTC",  # Duration of echo in STC
                                               NA, NA, NA,
                                               "alpha",                # Alpha: |AlphaEnd – AlphaStart|
                                               "theta",                # Theta: 2⋅EpsilonCalc
                                               NA, NA, NA, NA, 
                                               "altitudeLeftSideOfEcho",  # Altitude left side of echo (m)
                                               "altitudeRightSideOfEcho", # Altitude right side of echo (m)
                                               NA,
                                               "distLeftToBottom",     # Distance between left side and bottom of echo (samples)
                                               "nSamplesInEcho",       # Length of echo (samples)
                                               "areaOfEcho",           # Area of echo (seconds multiplied by meter)
                                               NA, NA, NA, NA, NA, NA, NA, NA, 
                                               NA, NA, NA, NA, NA, NA, NA))
    
   # load collection from local MS-SQL DB
   # ===========================================================================
   if(dbDriverChar != 'PostgreSQL') {
      collectionTable = QUERY( dbConnection, dbDriverChar, paste0("Select * From collection order by row asc"))
      collectionTable_time_stamp = QUERY( dbConnection, dbDriverChar, paste0("Select time_stamp From collection order by row asc"), as.is = TRUE)
      collectionTable$time_stamp <- collectionTable_time_stamp$time_stamp
   } else {
      collectionTable = QUERY( dbConnection, dbDriverChar, paste0("Select *, time_stamp::character varying ts From collection order by row asc"))
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