#### getEchoValidationTable ------------------------------------------------------------
#' @title  Get a BirdScan echo validation table
#' @description  gets the EchoValidationTable from an already connected DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar dbDriverChar 'SQL Server' The name of the driver. Should be either 'SQL Server' or 'PostgreSQL'. If 'PostgreSQL', it connects to cloud.birdradar.com
#'
#' @return A dataframe called echovalidationTable
#' @export
#'
getEchoValidationTable = function(dbConnection, dbDriverChar){
  echovalidationTypesTable = QUERY(dbConnection, 
                                   dbDriverChar, 
                                   "Select * From echo_validation_type")
   
  echovalidationTable = QUERY(dbConnection, 
                              dbDriverChar, 
                              "Select * From echo_validation order by echo_id asc")
   
  echoValidationList       = echovalidationTable$type
  echovalidationTable$type = echovalidationTypesTable$name[match(echoValidationList, echovalidationTypesTable$id)]
  rm(list = "echovalidationTypesTable", "echoValidationList")
  names(echovalidationTable)[names(echovalidationTable) == "echo_id"] = "echo"
  names(echovalidationTable)[names(echovalidationTable) == "type"]    = "echoValidationType"
   
  return(echovalidationTable)
}