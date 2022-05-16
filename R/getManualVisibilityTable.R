#### getManualVisibilityTable ------------------------------------------------------------
#' @title  Get manual visibility table
#' @description load visibility table from an already connect local MS-SQL DB
#' @author Baptiste Schmid (Swiss Ornithological Institute) \email{baptiste.schmid@@vogelwarte.ch}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' it connects to cloud.birdradar.com
#'
#' @return A dataframe with the manual visibility table
#' @export
#'
getManualVisibilityTable = function( dbConnection, dbDriverChar ){
# load protocol table from local MS-SQL DB
# ===========================================================================
  if (dbDriverChar == 'SQL Server'){
    manualVisibilityTable = QUERY(dbConnection, dbDriverChar, 
                                  "Select * From visibility_manual order by blind_from asc")
    
  } else if (dbDriverChar == 'PostgreSQL'){
    message("fetching manual visibility table from PostgrSQL not yet implemented")
  }
 
  return( manualVisibilityTable )
}