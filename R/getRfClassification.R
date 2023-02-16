#### getRfClassification -------------------------------------------------------
#' @title  Get a BirdScan 'rfClassification' table
#' @description  gets  the 'rfClasses' table from a 'Birdscan MR1' 'SQL' 
#' database
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar dbDriverChar 'SQL Server' The name of the driver. Should 
#' be either 'SQL Server' or 'PostgreSQL'. If 'PostgreSQL', it connects to 
#' cloud.birdradar.com
#'
#' @return A list containing three variables: (1) rfclassificationTable: The 
#' 'rfClassification' database table; (2) classProbabilitiesAndMtrFactors: A 
#' dataframe containing the classification probabilities for all classes for 
#' each object; and (3) availableClasses: the classes used for the 
#' classification of the objects.
#' @export
#' @examples
#' \dontrun{
#' # Set server and database settings
#' # ===========================================================================
#'   dbServer       = "MACHINE\\\\SERVERNAME"     # Set the name of your SQL server
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
#' rfClassification = getRfClassification(dbConnection, dbDriverChar)
#' }
#'
getRfClassification = function(dbConnection, dbDriverChar){
  # load 'rfClasses' table from MS-SQL DB
  # ============================================================================
    rfClasses = QUERY(dbConnection, dbDriverChar, 
                     "Select * From rfclasses")
    colnames(rfClasses)[colnames(rfClasses) == "is_protected"]               = "isProtected"
    colnames(rfClasses)[colnames(rfClasses) == "sphere_dia_cm"]              = "sphereDiaCm"
    colnames(rfClasses)[colnames(rfClasses) == "is_used_for_classification"] = "isUsedForClassification"
    availableClasses             = rfClasses[rfClasses$isUsedForClassification == 1, ]
    availableClasses$name        = as.character(availableClasses$name)
    availableClasses$description = as.character(availableClasses$description)
   
  # load rfclassification from local MS-SQL DB
  # ============================================================================
    rfclassificationTable = QUERY(dbConnection, 
                                  dbDriverChar, 
                                 paste0("select * from rf_classification where ", 
                                        "rf_classification.class is not null", 
                                        " and rf_classification.mtr_factor is ", 
                                        "not null order by echo asc"))
    rfClassificationList        = rfclassificationTable$class
    rfclassificationTable$class = availableClasses$name[match(rfClassificationList, 
                                                              availableClasses$id)]
   
  # load rfclassification probabilities from local MS-SQL DB
  # ============================================================================
    rfclassProbabilityTable = QUERY(dbConnection, dbDriverChar, 
                                    paste0("Select * From rf_class_probability", 
                                           " where rf_class_probability.class", 
                                           " is not null order by echo asc, ", 
                                           "class asc"))
    rfClassList                   = rfclassProbabilityTable$class
    rfclassProbabilityTable$class = availableClasses$name[match(rfClassList, 
                                                                availableClasses$id)]
    if (nrow(rfclassProbabilityTable) > 0){
      classProbabilites = reshape2::dcast(rfclassProbabilityTable[!is.na(rfclassProbabilityTable$class) & 
                                                                  !is.na(rfclassProbabilityTable$echo), ], 
                                          echo ~ class, 
                                          value.var     = "value", 
                                          fun.aggregate = mean)
      MTRFactorPerClass = reshape2::dcast(rfclassProbabilityTable[!is.na(rfclassProbabilityTable$class) & 
                                                                  !is.na(rfclassProbabilityTable$echo), ], 
                                          echo ~ class, 
                                          value.var     = "mtr_factor", 
                                          fun.aggregate = mean)
      names(classProbabilites)[!names(classProbabilites) == "echo"] = 
        paste("classProb.", 
              names(classProbabilites)[!names(classProbabilites) == "echo" ], 
              sep = "")
      names(MTRFactorPerClass)[!names(MTRFactorPerClass ) == "echo"] = 
        paste("MTRFact.", 
              names( MTRFactorPerClass )[!names(MTRFactorPerClass) == "echo" ], 
              sep = "")
      classProbabilitiesAndMtrFactors = merge(classProbabilites, MTRFactorPerClass, 
                                              by = "echo", 
                                              all.x =TRUE, 
                                              all.y = FALSE )
    } else {
      classProbabilitiesAndMtrFactors = data.frame()
    }
   
  names(rfclassificationTable )[names( rfclassificationTable ) == "mtr_factor"] = "mtr_factor_rf"
   
  return(list(rfclassificationTable           = rfclassificationTable, 
              classProbabilitiesAndMtrFactors = classProbabilitiesAndMtrFactors, 
              availableClasses                = availableClasses))
}