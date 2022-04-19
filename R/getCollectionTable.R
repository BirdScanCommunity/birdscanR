#### getCollectionTable ------------------------------------------------------------
#' @title  Get BirdScan collection table
#' @description load collection from local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' it connects to cloud.birdradar.com
#'
#' @return A dataframe with the collection table
#' @export
#'
getCollectionTable = function( dbConnection , dbDriverChar)
{
   # ::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
   # load collection from local MS-SQL DB
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