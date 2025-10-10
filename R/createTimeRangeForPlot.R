#' @title Create  Time Range for Plot
#' @description Create a list by segmenting the input time range into regular periods for plots.
#' @author Baptiste Schmid, Birgen Haest
#' @param startDate Per default, the first element of the input setting `timeRangeData`
#' @param endDate Per default, the second element of the input setting `timeRangeData``
#' @param periodLength Duration in days of each period
#' @param returnAsList TRUE per default, otherwise as data.frame.
#'
#' @return A list of time periods
#' @family plot functions
#' @export

#' @examples
#' \dontrun{
#' # Set server, database, and other input settings
#' # ===========================================================================
#' # Example with seven days time window
#' timeRangeData <- c("2024-01-01", "2024-02-15")
#' timeRangePlot <- createTimeRangePlot(timeRangeData[1], timeRangeData[2], 7)
#' print(timeRangePlot)
#' }
#'
createTimeRangeForPlot = function(startDate = NULL,
                                  endDate = NULL,
                                  periodLength = 7,
                                  returnAsList = TRUE) {
  # Convert inputs to Date class if they're not already
  # =============================================================================
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)

  # Calculate number of complete periods
  # =============================================================================
  total_days <- as.numeric(difftime(endDate, startDate, units = "days"))
  num_periods <- ceiling(total_days / periodLength)

  # Create sequence of dates
  period_starts <- seq(
    from = startDate,
    length.out = num_periods,
    by = paste(periodLength, "days")
  )

  # Create period ends (one day less than next period start)
  # =============================================================================
  period_ends <- c(period_starts[-1] - 1, endDate)

  # Combine into a data frame
  # =============================================================================
  periods <- data.frame(
    period_num = 1:length(period_starts),
    start = period_starts,
    end = period_ends
  )

  # =============================================================================
  # Create list of periods
  if (returnAsList) {
    periods_df <- periods
    periods <- list()
    for (i in 1:nrow(periods_df)) {
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
