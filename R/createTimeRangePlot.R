#' @title Extract DB Data
#' @description Segment the time range into regular periods for plots.
#' @author Baptiste Schmid
#' @param start_date Per default, the first element of the input setting 'timeRangeData'
#' @param end_date Per default, the second element of the input setting 'timeRangeData'
#' @param period_length duration in days of the period
#' @param returnAsList TRUE per default, otherwise as data.frame.
#'
#' @return A list of time periods 
#' @family 
#' @export
#' @examples
#' \dontrun{
#' # Set server, database, and other input settings
#' # ===========================================================================
#' #Example with seven days time window
#' timeRangeData <- c("2024-01-01", "2024-02-15")
#' timeRangePlot <- createTimeRangePlot(timeRangeData[1], timeRangeData[2], 7)
#' print(timeRangePlot)
#' }
#'
createTimeRangePlot = function(
                         start_date = timeRangeData[1], 
                         end_date = timeRangeData[2], 
                         period_length = 7, 
                         returnAsList = TRUE
                         ) {
  # Convert inputs to Date class if they're not already
  # =============================================================================
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Calculate number of complete periods
  # =============================================================================
  total_days <- as.numeric(difftime(end_date, start_date, units = "days"))
  num_periods <- ceiling(total_days / period_length)
  
  # Create sequence of dates
  period_starts <- seq(from = start_date, 
                       length.out = num_periods, 
                       by = paste(period_length, "days"))
  
  # Create period ends (one day less than next period start)
  # =============================================================================
  period_ends <- c(period_starts[-1] - 1, end_date)
  
  # Combine into a data frame
  # =============================================================================
  periods <- data.frame(
    period_num = 1:length(period_starts),
    start = period_starts,
    end = period_ends
  )
  
  # =============================================================================
  # Create list of periods
  if(returnAsList){
    periods_df <- periods
    periods <- list()
    for(i in 1:nrow(periods_df)) {
      periods[[i]] <- c(period_starts[i], period_ends[i])
    }    
  }

  # Return output
  # =============================================================================
  return(periods)
  
  # =============================================================================
  # =============================================================================
  # End of Function
  # =============================================================================
  # =============================================================================
}
