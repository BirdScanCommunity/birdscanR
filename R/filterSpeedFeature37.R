#### filterSpeedFeature37 -------------------------------------------------------
#' @title  Filter outliers in Speed feature (collection.feature37)
#' @description  Filter outliers in Speed feature (collection.feature37)
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com};
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @param echoData valid echodata
#' @param minEchoDuration minimum duration of a echo to allow speed feature
#'
#' @return echoData with the filtered speed feature 37
#' @family manipulation functions
#' @export
#' @examples
#' \donttest{
#' # Load example data
#' # ===========================================================================
#' dbData = readRDS(system.file("extdata",
#'   "CH_Sempach_2024_SEP24_25_DataExtract.rds",
#'   package = "birdscanR"
#' ))
#'
#' # Filter speed feature 37
#' # ===========================================================================
#' minEchoDuration = 5
#' dbData$echoData = filterSpeedFeature37(
#'   echoData        = dbData$echoData,
#'   minEchoDuration = minEchoDuration
#' )
#' }
filterSpeedFeature37 <- function(echoData = NULL, minEchoDuration = 5) {
  if (!is.null(echoData)) {
    # Create a validity mask to only compute the ratio when both values are non-NA and nSamplesInEcho is not 0
    valid <- !is.na(echoData$feature33.distLeftToBottom) &
      !is.na(echoData$feature34.nSamplesInEcho) &
      echoData$feature34.nSamplesInEcho != 0

    # Initialize ratio as NA for all rows
    ratio <- rep(NA, length(echoData$feature33.distLeftToBottom))

    # Compute the ratio only for valid cases
    ratio[valid] <- echoData$feature33.distLeftToBottom[valid] / echoData$feature34.nSamplesInEcho[valid]

    # Compute the update condition:
    # - feature19.durationOfEcho is less than minEchoDuration (e.g., 5 when minEchoDuration is set to 5)
    # - OR for valid rows, the ratio is either below 0.2 or above 0.75
    # - OR the absolute difference between feature31.altitudeRightSideOfEcho and feature30.altitudeLeftSideOfEcho is greater than 25
    condition <- (
      (!is.na(echoData$feature19.durationOfEcho) & echoData$feature19.durationOfEcho < minEchoDuration) |
        (valid & (ratio < 0.2 | ratio > 0.75)) |
        (abs(echoData$feature31.altitudeRightSideOfEcho - echoData$feature30.altitudeLeftSideOfEcho) > 25)
    )

    # Set feature37.speed to NA where the condition is met
    echoData$feature37.speed[condition] <- NA
  }

  return(echoData)
}
