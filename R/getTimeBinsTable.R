#### getTimeBinsTable ------------------------------------------------------------
#' @title  Get BirdScan timebins table
#' @description load timebins table from an already connect local MS-SQL DB
#' @author Fabian Hertner (SBRS) \email{fabian.hertner@@swiss-birdradar.com}
#' @param dbConnection a valid database connection
#' @param dbDriverChar the name of the driver. If different from 'PostgreSQL' it connects to cloud.birdradar.com
#'
#' @return A dataframe with the timebins table
#' @export
#'
getTimeBinsTable = function( dbConnection, dbDriverChar )
{
   # :::::::::::::::::::::::::::::::::::::::::::::::::::::::
   # load protocol table from local MS-SQL DB
   if(dbDriverChar != 'PostgreSQL') {
      timeBinsTable = QUERY(dbConnection, dbDriverChar, "Select * From time_bins order by id asc")
      timeBinsTable_times = QUERY(dbConnection, dbDriverChar,"Select time_start, time_stop From time_bins order by id asc", as.is = TRUE)
      timeBinsTable$time_start <- timeBinsTable_times$time_start
      timeBinsTable$time_stop <- timeBinsTable_times$time_stop
   } else {
      timeBinsTable = QUERY(dbConnection, dbDriverChar, "Select *,time_start::character varying as start,time_stop::character varying as stop FROM time_bins order by id asc")
      timeBinsTable$time_start <- timeBinsTable$start
      timeBinsTable$time_stop <- timeBinsTable$stop
      timeBinsTable$start <- NULL
      timeBinsTable$stop <- NULL
   }
   
   colnames(timeBinsTable)[colnames(timeBinsTable) == "siteid"] <- "siteID"
   
   return( timeBinsTable )
}