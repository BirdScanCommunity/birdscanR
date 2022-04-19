#### getEchoFeatures ------------------------------------------------------------
#' @title  Get BirdScan echo features
#' @description load echo rffeature map from local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' it connects to cloud.birdradar.com
#' @param listOfRfFeaturesToExtract a list of feature to extract
#'
#' @return A list of the features extracted
#'
getEchoFeatures = function( dbConnection, dbDriverChar, listOfRfFeaturesToExtract )
{
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load echo rffeatures from local MS-SQL DB
   rffeaturesTable = QUERY(dbConnection, dbDriverChar, 
                           "Select * From rffeatures"
   )
   
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load echo rffeature map from local MS-SQL DB
   if( !is.null( listOfRfFeaturesToExtract ) )
   {
      echorffeaturesMapTable = QUERY(dbConnection, dbDriverChar, 
                                     paste( "Select * From echo_rffeature_map where feature in ( ", paste( listOfRfFeaturesToExtract, collapse = ", " ), " )" )
      )   
      
      featurelist <- echorffeaturesMapTable$feature
      echorffeaturesMapTable$feature <- rffeaturesTable$name[ match( featurelist, rffeaturesTable$id ) ]
      echoRfFeatureMap = dcast( echorffeaturesMapTable, echo ~ feature, value.var = "value", fun.aggregate = mean )
      
      return( list( echoRfFeatureMap = echoRfFeatureMap, rfFeatures = rffeaturesTable ) )
   }
   
   return( list( echoRfFeatureMap = NULL, rfFeatures = rffeaturesTable ) )
}