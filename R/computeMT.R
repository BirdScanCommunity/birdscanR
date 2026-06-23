#' @title computeMT
#' @description This function will estimate the Activity / Migration Traffic
#' (MT, expressed as #objects / km) based on a migration traffic rate data frame  
#' calculated from the observations in your database.
#' @param mtrObject data frame with migration traffic rates, calculated from 
#' computeMTR()
#' @param classSelection character string vector with all classes which should 
#' be used to calculate the MT. The MT will be calculated for each class as well 
#' as for all classes together.
#'
#' @return Migration Traffic 
#' @family 
#' @export
#'
#'
# =============================================================================

computeMT <- function(mtrObject, classSelection) {
  
  mt <- data.frame(
    observationTime_h = mtrObject$observationTime_h
  )
  
  for (class in classSelection) {
    
    mtr.selectedClass <- mtrObject[[paste("mtr", class, sep = ".")]]
    
    mt[[paste("migrationtraffic", class, sep = ".")]] <-
      mtrObject$observationTime_h * mtr.selectedClass
  }
  
  mt[["migrationtraffic.allClasses"]] <-
    mtrObject$observationTime_h * mtrObject$mtr.allClasses
  
  return(mt)
}

