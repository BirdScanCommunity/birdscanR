#' Example Birdscan MR1 data extract from Sempach, Switzerland
#'
#' A complete data extract from a Birdscan MR1 weather radar, as returned by
#' [extractDbData()]. Collected at Sempach, Switzerland (47.13°N, 8.19°E)
#' on September 24–25, 2024. Load with:
#' `readRDS(system.file("extdata", "CH_Sempach_2024_SEP24_25_DataExtract.rds",
#' package = "birdscanR"))`
#'
#' @name CH_Sempach_2024_SEP24_25_DataExtract
#'
#' @format A named list as returned by [extractDbData()], with components:
#' \describe{
#'   \item{echoData}{data.frame with one row per detected echo}
#'   \item{protocolData}{data.frame with protocol/collection intervals}
#'   \item{siteData}{data.frame with radar site metadata}
#'   \item{visibilityData}{data.frame with automatic visibility/blind times}
#'   \item{timeBinData}{data.frame with raw time-bin table from the database}
#'   \item{availableClasses}{character vector of classification labels}
#'   \item{availableBatClasses}{character vector of bat classification labels}
#'   \item{rfFeatures}{data.frame with RF feature metadata}
#'   \item{TimeZone}{data.frame with radarTimeZone and targetTimeZone}
#'   \item{classProbabilitiesAndMtrFactors}{data.frame with class probabilities}
#'   \item{batProbabilitiesAndMtrFactors}{data.frame with bat class probabilities}
#'   \item{sunriseSunset}{data.frame with sunrise/sunset and civil twilight times}
#' }
#'
#' @keywords datasets
#' @family sample data
#'
#' @examples
#' dbData = readRDS(system.file("extdata",
#'   "CH_Sempach_2024_SEP24_25_DataExtract.rds",
#'   package = "birdscanR"
#' ))
NULL
