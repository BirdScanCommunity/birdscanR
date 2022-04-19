#### getRfClassification ------------------------------------------------------------
#' @title  Get a BirdScan RFClassification table
#' @description  gets  rfclasses from local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#'
#' @return A dataframe called rfclasses
#'
getRfClassification = function( dbConnection, dbDriverChar )
{
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load rfclasses from local MS-SQL DB
   rfClasses = QUERY(dbConnection, dbDriverChar, 
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
   rfclassificationTable = QUERY(dbConnection, dbDriverChar, 
                                 "select * from rf_classification where rf_classification.class is not null and rf_classification.mtr_factor is not null order by echo asc"
   )
   
   rfClassificationList <- rfclassificationTable$class
   rfclassificationTable$class <- availableClasses$name[ match( rfClassificationList, availableClasses$id ) ]
   
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load rfclassification probabilities from local MS-SQL DB
   rfclassProbabilityTable = QUERY(dbConnection, dbDriverChar, 
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