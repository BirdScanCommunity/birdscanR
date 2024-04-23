#### getBatClassification -------------------------------------------------------
#' @title  Get a BirdScan 'batClassification' table
#' @description  gets  the 'rfClasses' table from a 'Birdscan MR1' 'SQL' 
#' database
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar dbDriverChar 'SQL Server' The name of the driver. Should 
#' be either 'SQL Server' or 'PostgreSQL'. If 'PostgreSQL', it connects to 
#' cloud.birdradar.com
#'
#' @return A list containing three variables: (1) batClassificationTable: The 
#' 'batClassification' database table; (2) classProbabilitiesAndMtrFactors: A 
#' dataframe containing the classification probabilities for all classes for 
#' each object; and (3) availableClasses: the classes used for the 
#' classification of the objects.
#' @export
#' @examples
#' \dontrun{
#' # Set server and database settings
#' # ===========================================================================
#'   dbServer       = "MACHINE\\SERVERNAME"     # Set the name of your SQL server
#'   dbName         = "db_Name"                   # Set the name of your database
#'   dbDriverChar   = "SQL Server"                # Set either "SQL Server" or "PostgreSQL"
#'
#' # Open the connection with the database
#' # ===========================================================================
#'   dsn = paste0("driver=", dbDriverChar, ";server=", dbServer,
#'                ";database=", dbName,
#'                ";uid=", rstudioapi::askForPassword("Database user"),
#'                ";pwd=", rstudioapi::askForPassword("Database password"))
#'   dbConnection = RODBC::odbcDriverConnect(dsn)
#'
#' rfClassification = getBatClassification(dbConnection, dbDriverChar)
#' }
#'
getBatClassification = function(dbConnection, dbDriverChar){
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # check if batClassification table exists
  if(dbDriverChar == 'PostgreSQL'){
    batClassificationTableExists <- DBI::dbExistsTable( dbConnection, "bat_classification" )
    batProbabilityTableExists <- DBI::dbExistsTable( dbConnection, "bat_class_probability" )
  } else {
    batClassificationTableExists <- "bat_classification" %in% RODBC::sqlTables(dbConnection)$TABLE_NAME
    batProbabilityTableExists <- "bat_class_probability" %in% RODBC::sqlTables(dbConnection)$TABLE_NAME  
  }
  
  if(batClassificationTableExists && batProbabilityTableExists){
    # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # load rfclasses from DB
    rfClasses = QUERY(dbConnection, dbDriverChar, 
                      "Select * From rfclasses"
    )
    colnames(rfClasses)[colnames(rfClasses) == "is_protected"] <- "isProtected"
    colnames(rfClasses)[colnames(rfClasses) == "sphere_dia_cm"] <- "sphereDiaCm"
    colnames(rfClasses)[colnames(rfClasses) == "is_used_for_bat_classification"] <- "isUsedForBatClassification"
    availableClasses <- rfClasses[ rfClasses$isUsedForBatClassification == 1, ]
    availableClasses$name <- as.character( availableClasses$name )
    availableClasses$description <- as.character( availableClasses$description )
    
    # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # load batClassification from DB
    batClassificationTable = QUERY(dbConnection,
                                   dbDriverChar, 
                                   paste0("select * from bat_classification where ",
                                          "bat_classification.class is not null ",
                                          "and bat_classification.mtr_factor is ",
                                          "not null order by echo asc"))
    
    batClassificationList <- batClassificationTable$class
    batClassificationTable$class <- availableClasses$name[ match( batClassificationList, 
                                                                  availableClasses$id ) ]
    
    # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # load bat classification probabilities from DB
    batClassProbabilityTable = QUERY(dbConnection,
                                     dbDriverChar, 
                                     paste0("Select * From bat_class_probability ",
                                            "where bat_class_probability.class ",
                                            "is not null order by echo asc, ",
                                            "class asc"))
    
    batClassList <- batClassProbabilityTable$class
    batClassProbabilityTable$class <- availableClasses$name[ match( batClassList, 
                                                                    availableClasses$id ) ]
    
    if( nrow( batClassProbabilityTable ) > 0 ){
      classProbabilites <- reshape2::dcast( batClassProbabilityTable[ !is.na(batClassProbabilityTable$class) & 
                                                                      !is.na(batClassProbabilityTable$echo), ], 
                                            echo ~ class, 
                                            value.var="value", 
                                            fun.aggregate = mean )
      MTRFactorPerClass <- reshape2::dcast( batClassProbabilityTable[ !is.na(batClassProbabilityTable$class) & 
                                                                        !is.na(batClassProbabilityTable$echo), ], 
                                            echo ~ class, 
                                            value.var="mtr_factor", 
                                            fun.aggregate = mean )
      names( classProbabilites )[ !names( classProbabilites ) == "echo" ] <- 
        paste( "classProb.", 
               names( classProbabilites )[ !names( classProbabilites ) == "echo" ], 
               sep = "" )
      names( MTRFactorPerClass )[ !names( MTRFactorPerClass ) == "echo" ] <- 
        paste( "MTRFact.", 
               names( MTRFactorPerClass )[ !names( MTRFactorPerClass ) == "echo" ], 
               sep = "" )
      classProbabilitiesAndMtrFactors <- merge( classProbabilites, MTRFactorPerClass, 
                                                by = "echo", 
                                                all.x =TRUE, 
                                                all.y = FALSE )
    } else {
      classProbabilitiesAndMtrFactors <- data.frame()
    }
    
    names( batClassificationTable )[ names( batClassificationTable ) == "mtr_factor" ] <- "mtr_factor_rf"
    
    colnames( batClassificationTable )[ colnames( batClassificationTable ) == "class"] <- "batClass"
    colnames( batClassificationTable )[ colnames( batClassificationTable ) == "mtr_factor_rf"] <- "bat_mtr_factor_rf"
    colnames( batClassificationTable )[ colnames( batClassificationTable ) == "class_probability"] <- "bat_class_probability"
    colnames( batClassificationTable )[ colnames( batClassificationTable ) == "mtr_factor_sphereDiaCm"] <- "bat_mtr_factor_sphereDiaCm"
    colnames( batClassificationTable )[ colnames( batClassificationTable ) == "classifierVersion"] <- "batClassifierVersion"
  } else {
    batClassificationTable <- data.frame(echo = integer(),
                                         batClass = character(),
                                         bat_mtr_factor_rf = double(),
                                         bat_class_probability = double(),
                                         bat_mtr_factor_sphereDiaCm = double(),
                                         batClassifierVersion = character(),
                                         stringsAsFactors = FALSE )
    classProbabilitiesAndMtrFactors <- data.frame( echo = integer(),
                                                   ClassProb.bat = double(),
                                                   ClassProb.nonbat = double(),
                                                   MTRFact.bat = double(),
                                                   MTRFact.nonbat = double(),
                                                   stringsAsFactors = FALSE )
    availableClasses <- data.frame( id = integer(),
                                    name = character(),
                                    description = character(),
                                    isProtected = integer(),
                                    sphereDiaCm = double(),
                                    isUsedForClassification = integer(),
                                    isUsedForBatClassification = integer(),
                                    stringsAsFactors = FALSE )
  }
  
  return(list(batClassificationTable          = batClassificationTable, 
              classProbabilitiesAndMtrFactors = classProbabilitiesAndMtrFactors, 
              availableClasses                = availableClasses ) )
}