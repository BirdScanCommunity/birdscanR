#### QUERY ------------------------------------------------------------
#' @title  Query SQL database
#' @description  Run an SQL query on an already connected DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid  database connection
#' @param dbDriverChar the name of the driver. 
#' @param query an SQL string with your query
#' @param as.is If TRUE, leaves data as it is.
#'
#' @return the result of the query
#' @export
#' 
QUERY <- function(dbConnection, dbDriverChar, query, as.is = FALSE){
   if(dbDriverChar == 'PostgreSQL'){
      t  <- DBI::dbGetQuery(dbConnection, query, as.is = as.is)
   } else {
      t  <- RODBC::sqlQuery(dbConnection, query, as.is = as.is)
   }
   return(t)
}