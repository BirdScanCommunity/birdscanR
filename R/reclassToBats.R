#' @title integrate bat classification
#' @author Fabian Hertner
#' @description Reclassifies echoes based on bat classification.
#' @param echoData echodata dataframe, output from extractDbData
#' @param batProbabilitiesAndMtrFactors probabilities of bat classification,
#' output from extractDbData'
#' @param reclassToBatCutoff Threshold (0..1), classification of echoes with
#' bat probability higher than reclassToBatCutoff will be set to 'bat'
#'
#' @return echoData dataframe
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
#' # Reclass To Bats
#' # ===========================================================================
#' dbData$echoData = reclassToBats(
#'   echoData = dbData$echoData,
#'   batProbabilitiesAndMtrFactors = dbData$batProbabilitiesAndMtrFactors,
#'   reclassToBatCutoff = 0.5
#' )
#' }
reclassToBats = function(echoData = NULL,
                         batProbabilitiesAndMtrFactors = NULL,
                         reclassToBatCutoff = -1) {
  # reclass by bat probability
  if (!is.null(echoData) &&
    !is.null(batProbabilitiesAndMtrFactors) &&
    !is.null(reclassToBatCutoff) &&
    is.numeric(reclassToBatCutoff) &&
    reclassToBatCutoff >= 0 &&
    reclassToBatCutoff <= 1) {
    if (nrow(batProbabilitiesAndMtrFactors) == 0) {
      stop("no bat class probabilities, check database and settings")
    }

    echoDataTmp <- merge(echoData, batProbabilitiesAndMtrFactors, by = "echo", all.x = TRUE, all.y = FALSE)

    echoData[!is.na(echoDataTmp$classProb.bat) & echoDataTmp$classProb.bat > reclassToBatCutoff, ]$class = "bat"
    echoData[!is.na(echoDataTmp$classProb.bat) & echoDataTmp$classProb.bat > reclassToBatCutoff, ]$class_probability = echoDataTmp[!is.na(echoDataTmp$classProb.bat) & echoDataTmp$classProb.bat > reclassToBatCutoff, ]$classProb.bat
    echoData[!is.na(echoDataTmp$classProb.bat) & echoDataTmp$classProb.bat > reclassToBatCutoff, ]$mtr_factor_rf = echoDataTmp[!is.na(echoDataTmp$classProb.bat) & echoDataTmp$classProb.bat > reclassToBatCutoff, ]$MTRFact.bat
  }
  return(echoData)
}
