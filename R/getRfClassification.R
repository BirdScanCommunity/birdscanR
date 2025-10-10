#' @title Get a BirdScan 'rfClassification' table
#' @author Fabian Hertner, Birgen Haest
#' @description Gets the 'rfClasses' table from a 'Birdscan MR1' 'SQL'
#' database.
#' @inheritParams QUERY
#'
#' @return A list containing three variables: (1) rfclassificationTable: The
#' 'rfClassification' database table; (2) classProbabilitiesAndMtrFactors: A
#' dataframe containing the classification probabilities for all classes for
#' each object; and (3) availableClasses: the classes used for the
#' classification of the objects.
#' @family read SQL database functions
#' @export
#' @examples
#' \dontrun{
#' # Set server and database settings
#' # ==========================================================================
#' # Using and Microsoft SQL database
#' # ========================================================================
#' dbServer = "MACHINE\\SERVERNAME" # Set the name of your SQL server
#' dbName = "db_Name" # Set the name of your database
#' dbDriverChar = "SQL Server" # Set to "SQL Server"
#'
#' # Using a PostgreSQL
#' # ========================================================================
#' dbServer = "cloud.birdradar.com" # Set the name or IP of your postgreSQL
#' dbName = "db_Name" # Set the name of your database
#' dbDriverChar = "PostgreSQL" # Set to "PostgreSQL"
#'
#' # Open the connection with the database
#' # ==========================================================================
#' dbConnection = dbConnectBirdscanSQL(
#'   dbDriverChar = dbDriverChar,
#'   dbServer     = dbServer,
#'   dbName       = dbName,
#' )
#'
#' rfClassification = getRfClassification(dbConnection)
#' }
#'
getRfClassification = function(dbConnection, dbDriverChar) {
  if (!missing(dbDriverChar)) {
    lifecycle::deprecate_warn("0.4.0", "getRfClassification(dbDriverChar)")
  }
  # load 'rfClasses' table from MS-SQL DB
  # ============================================================================
  rfClasses = QUERY(
    dbConnection,
    query =
      "SELECT * FROM rfclasses"
  )
  colnames(rfClasses)[colnames(rfClasses) == "is_protected"] = "isProtected"
  colnames(rfClasses)[colnames(rfClasses) == "sphere_dia_cm"] = "sphereDiaCm"
  colnames(rfClasses)[colnames(rfClasses) == "is_used_for_classification"] = "isUsedForClassification"
  availableClasses = rfClasses[rfClasses$isUsedForClassification == 1, ]
  availableClasses$name = as.character(availableClasses$name)
  availableClasses$description = as.character(availableClasses$description)

  # load rfclassification from local MS-SQL DB
  # ============================================================================
  rfclassificationTable = QUERY(
    dbConnection,
    query = paste0(
      "SELECT * FROM rf_classification where ",
      "rf_classification.class is not null",
      " AND rf_classification.mtr_factor is ",
      "not null order by echo asc"
    )
  )
  rfClassificationList = rfclassificationTable$class
  rfclassificationTable$class = availableClasses$name[match(
    rfClassificationList,
    availableClasses$id
  )]

  # load rfclassification probabilities from local MS-SQL DB
  # ============================================================================
  rfclassProbabilityTable = QUERY(
    dbConnection,
    query =
      paste0(
        "SELECT * FROM rf_class_probability",
        " WHERE rf_class_probability.class",
        " is not null order by echo asc, ",
        "class asc"
      )
  )
  rfClassList = rfclassProbabilityTable$class
  rfclassProbabilityTable$class = availableClasses$name[match(
    rfClassList,
    availableClasses$id
  )]
  if (nrow(rfclassProbabilityTable) > 0) {
    classProbabilites = reshape2::dcast(
      rfclassProbabilityTable[!is.na(rfclassProbabilityTable$class) &
        !is.na(rfclassProbabilityTable$echo), ],
      echo ~ class,
      value.var = "value",
      fun.aggregate = mean
    )
    MTRFactorPerClass = reshape2::dcast(
      rfclassProbabilityTable[!is.na(rfclassProbabilityTable$class) &
        !is.na(rfclassProbabilityTable$echo), ],
      echo ~ class,
      value.var = "mtr_factor",
      fun.aggregate = mean
    )
    names(classProbabilites)[!names(classProbabilites) == "echo"] =
      paste("classProb.",
        names(classProbabilites)[!names(classProbabilites) == "echo"],
        sep = ""
      )
    names(MTRFactorPerClass)[!names(MTRFactorPerClass) == "echo"] =
      paste("MTRFact.",
        names(MTRFactorPerClass)[!names(MTRFactorPerClass) == "echo"],
        sep = ""
      )
    classProbabilitiesAndMtrFactors = merge(classProbabilites, MTRFactorPerClass,
      by = "echo",
      all.x = TRUE,
      all.y = FALSE
    )
  } else {
    classProbabilitiesAndMtrFactors = data.frame()
  }

  names(rfclassificationTable)[names(rfclassificationTable) == "mtr_factor"] = "mtr_factor_rf"

  return(list(
    rfclassificationTable = rfclassificationTable,
    classProbabilitiesAndMtrFactors = classProbabilitiesAndMtrFactors,
    availableClasses = availableClasses
  ))
}
