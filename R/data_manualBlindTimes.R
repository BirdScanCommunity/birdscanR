#' Example file on how to include manual blind times for your 'Birdscan MR1'
#' database.
#'
#' To create your own manual blind times file, just copy this file, and adjust.
#' The example blind times (rain, a technical outage and clutter) fall within
#' the period of the sample data extract
#' `CH_Sempach_2024_SEP24_25_DataExtract.rds` (24-25 September 2024), so they
#' can be used together in examples.
#'
#' @docType data
#'
#' @usage data(manualBlindTimes)
#'
#' @format A `data.frame` with one row per blind time and three character
#' columns without header names: start and stop of the blind time (format
#' `"%Y-%m-%d %H:%M:%S"`), and the type of blind time. Write it to a csv file
#' without row and column names, and load it with [loadManualBlindTimes()].
#'
#' @keywords datasets
#' @family sample data
#'
#' @examples
#' data(manualBlindTimes)
"manualBlindTimes"
